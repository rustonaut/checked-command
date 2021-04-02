//! A `std::process::Command` replacement which is a bit more flexible and testable.
//!
//! For now this is focused on cases which wait until the subprocess is completed
//! and then map the output (or do not care about the output).
//!
//! - by default check the exit status
//!
//! - bundle a mapping of the captured stdout/stderr to an result into the command,
//!   i.e. the `Command` type is `Command<Output, Error>` e.g. `Command<Vec<String>, Error>`.
//!
//! - implicitly define if stdout/stderr needs to be captured to prevent mistakes
//!   wrt. this, this is done through through the same mechanism which is used to
//!   define how the output is mapped, e.g. `Command::new("ls", ReturnStdoutString)`
//!   will implicitly enabled stdout capturing and disable `stderr` capturing.
//!
//! - allow replacing command execution with an callback, this is mainly used to
//!   allow mocking the command.
//!
//! - besides allowing to decide weather the sub-process should inherit the environment and
//!   which variables get removed/set/overwritten this type also allows you to whitelist which
//!   env variables should be inherited.
//!
//! - do not have `&mut self` pass through based API. This makes it more bothersome to create
//!   functions which create and return commands, which this types intents to make simple so
//!   that you can e.g. have a function like `fn ls_command() -> Command<Vec<String>, Error>`
//!   which returns a command which if run runs the ls command and returns a vector of string
//!   (or an error if spawning, running or utf8 validation fails).
//!
//! - be generic over Output and Error type but dynamic over how the captured stdout/err is
//!   mapped to the given `Result<Output, Error>`. This allows you to e.g. at runtime switch
//!   between different function which create a command with the same output but on different
//!   ways (i.e. with different called programs and output mapping, e.g. based on a config
//!   setting).
//!
//! # Basic Examples
//!
//! ```rust
//! use mapped_command::{Command, MapStdoutString, ReturnStdoutString, ExecResult, CommandExecutionWithStringOutputError as Error};
//!
//! /// Usage: `echo().run()`.
//! fn echo() -> Command<String, Error> {
//!     // implicitly enables stdout capturing but not stderr capturing
//!     // and converts the captured bytes to string
//!     Command::new("echo", ReturnStdoutString)
//! }
//!
//! /// Usage: `ls_command().run()`.
//! fn ls_command() -> Command<Vec<String>, Error> {
//!     Command::new("ls", MapStdoutString(|out| {
//!         let lines = out.lines().map(Into::into).collect::<Vec<_>>();
//!         Ok(lines)
//!     }))
//! }
//!
//! fn main() {
//!     let res = ls_command()
//!         //mock
//!         .with_mock_result(|_options, capture_stdout, capture_stderr| {
//!             assert_eq!(capture_stdout, true);
//!             assert_eq!(capture_stderr, false);
//!             Ok(ExecResult {
//!                 exit_status: 0.into(),
//!                 // Some indicates in the mock that stdout was captured, None would mean it was not.
//!                 stdout: Some("foo\nbar\ndoor\n".to_owned().into()),
//!                 ..Default::default()
//!             })
//!         })
//!         // run, check exit status and map captured outputs
//!         .run()
//!         .unwrap();
//!
//!     assert_eq!(res, vec!["foo", "bar", "door"]);
//!
//!     let err = ls_command()
//!         //mock
//!         .with_mock_result(|_options, capture_stdout, capture_stderr| {
//!             assert_eq!(capture_stdout, true);
//!             assert_eq!(capture_stderr, false);
//!             Ok(ExecResult {
//!                 exit_status: 1.into(),
//!                 stdout: Some("foo\nbar\ndoor\n".to_owned().into()),
//!                 ..Default::default()
//!             })
//!         })
//!         .run()
//!         .unwrap_err();
//!
//!     assert_eq!(err.to_string(), "Unexpected exit status. Got: 0x1, Expected: 0x0");
//! }
//! ```
//!
//! # Handling arguments and environment variables
//!
//! ```rust
//! use mapped_command::{Command,ReturnStdoutString, EnvChange};
//! # #[cfg(unix)]
//! # fn main() {
//! std::env::set_var("FOOBAR", "the foo");
//! std::env::set_var("DODO", "no no");
//! let echoed = Command::new("bash", ReturnStdoutString)
//!     .with_arguments(&["-c", "echo $0 ${DODO:-yo} $FOOBAR $BARFOOT $(pwd)", "arg1"])
//!     .with_inherit_env(false)
//!     .with_env_update("BARFOOT", "the bar")
//!     //inherit this even if env inheritance is disabled (it is see above)
//!     .with_env_update("FOOBAR", EnvChange::Inherit)
//!     .with_working_directory_override(Some("/usr"))
//!     .run()
//!     .unwrap();
//!
//! assert_eq!(echoed, "arg1 yo the foo the bar /usr\n");
//! # }
//! ```
//!
use std::{
    ffi::OsString,
    fmt::{self, Debug},
    io,
    ops::{Deref, DerefMut},
    path::PathBuf,
};

use thiserror::Error;

pub use self::{env::*, exit_status::*, pipe::*, return_settings::*, spawn::*};

#[macro_use]
mod utils;
mod env;
mod exit_status;
pub mod mock;
mod pipe;
mod return_settings;
mod spawn;
pub mod sys;

//TODO impl Debug

/// A alternative to `std::process::Command` see module level documentation.
pub struct Command<Output, Error>
where
    Output: 'static,
    Error: From<io::Error> + From<UnexpectedExitStatus> + 'static,
{
    spawn_options: SpawnOptions,
    expected_exit_status: Option<ExitStatus>,
    output_mapping: Box<dyn OutputMapping<Output = Output, Error = Error>>,
    spawn_impl: Box<dyn SpawnImpl>,
}

impl<Output, Error> Command<Output, Error>
where
    Output: 'static,
    Error: From<io::Error> + From<UnexpectedExitStatus> + 'static,
{
    /// Create a new command for given program and output mapping.
    ///
    /// The output mapping will imply if stdout/stderr is captured and how the
    /// captured output is mapped to a `Result<Self::Output, Self::Error>`.
    ///
    pub fn new(
        program: impl Into<OsString>,
        output_mapping: impl OutputMapping<Output = Output, Error = Error>,
    ) -> Self {
        Command {
            spawn_options: SpawnOptions::new(program.into()),
            expected_exit_status: Some(ExitStatus::Code(0)),
            output_mapping: Box::new(output_mapping) as _,
            spawn_impl: Box::new(sys::SpawnImpl),
        }
    }

    /// Returns this command with new arguments added to the end of the argument list
    pub fn with_arguments<T>(mut self, args: impl IntoIterator<Item = T>) -> Self
    where
        T: Into<OsString>,
    {
        self.arguments.extend(args.into_iter().map(|v| v.into()));
        self
    }

    /// Returns this command with a new argument added to the end of the argument list
    pub fn with_argument(mut self, arg: impl Into<OsString>) -> Self {
        self.arguments.push(arg.into());
        self
    }

    /// Returns this command with the map of env updates updated by given iterator of key value pairs.
    ///
    /// If any key from the new map already did exist in the current updates it will
    /// replace the old key & value.
    ///
    /// - Common supported values for keys include `OsString`, `&OsStr`, `String`, `&str`.
    /// - Common supported values for values include `EnvChange`, `OsString`, `&OsStr`, `String`,
    ///   `&str`
    ///
    /// So you can pass in containers like `Vec<(&str, &str)>`, `HashMap<&str, &str>` or
    /// `HashMap<OsString, EnvChange>`, etc.
    ///
    /// # Warning
    ///
    /// The keys of env variables will *not* be evaluated for syntactic validity.
    /// Setting a key invalid on given platform *might* cause the process spawning to
    /// fail (e.g. using a key lik `"="` or `""`). It also *might* also do other thinks
    /// like the env variable being passed in but being unaccessible or similar. It's completely
    /// dependent on the OS and the impl. of `std::process::Command` or whatever is used to
    /// execute the command.
    pub fn with_env_updates<K, V>(mut self, map: impl IntoIterator<Item = (K, V)>) -> Self
    where
        K: Into<OsString>,
        V: Into<EnvChange>,
    {
        self.env_updates
            .extend(map.into_iter().map(|(k, v)| (k.into(), v.into())));
        self
    }

    /// Returns this command with the map of env updates updated by one key value pair.
    ///
    /// If the new key already did exist in the current updates it will replace that
    /// old key & value.
    ///
    /// See [`Command::with_env_updates()`].
    pub fn with_env_update(
        mut self,
        key: impl Into<OsString>,
        value: impl Into<EnvChange>,
    ) -> Self {
        self.env_updates.insert(key.into(), value.into());
        self
    }

    /// Returns this command with a change to weather or the sub-process will inherit env variables.
    ///
    /// See [`Command::inherit_env()`] for how this affects the sub-process env.
    pub fn with_inherit_env(mut self, do_inherit: bool) -> Self {
        self.inherit_env = do_inherit;
        self
    }

    /// Replaces the working directory override.
    ///
    /// Setting it to `None` will unset the override making the spawned
    /// process inherit the working directory from the spawning process.
    pub fn with_working_directory_override(
        mut self,
        wd_override: Option<impl Into<PathBuf>>,
    ) -> Self {
        self.working_directory_override = wd_override.map(Into::into);
        self
    }

    /// Set which exit status is treated as successful.
    ///
    /// **This enables exit status checking even if it
    ///   was turned of before.**
    pub fn with_expected_exit_status(mut self, exit_status: impl Into<ExitStatus>) -> Self {
        self.expected_exit_status = Some(exit_status.into());
        self
    }

    /// Disables exit status checking.
    pub fn without_expected_exit_status(mut self) -> Self {
        self.expected_exit_status = None;
        self
    }

    /// Run the command, blocking until completion and then mapping the output.
    ///
    /// This will:
    ///
    /// 1. run the program with the specified arguments and env variables
    /// 2. capture the necessary outputs as specified by the output mapping
    /// 3. if exit status checking was not disabled check the exit status and potentially
    ///    fail.
    /// 4. if 3 doesn't fail now map captured outputs to a `Result<Output, Error>`
    ///
    /// If [`Command::with_exec_replacement_callback()`] is used instead of running the
    /// program and capturing the output the given callback is called. The callback
    /// could mock the program execution. The exit status checking and output mapping
    /// are still done as normal.
    ///
    /// # Panics
    ///
    /// If the inner `SpawnOptions` are changed so that a *reserved* `stdout_setup`/`stderr_setup`
    /// was changed this might panic.
    ///
    /// Pretty much the only way to change a *reserved* `stdout_setup`/`stderr_setup` is by
    /// using taking a `DerefMut` and then replacing the spawn option with another on, so
    /// it's normally not a thing which you will run into.
    pub fn run(self) -> Result<Output, Error> {
        self.spawn()?.wait()
    }

    //TODO doc
    ///
    /// # Panics
    ///
    /// If the inner `SpawnOptions` are changed so that a *reserved* `stdout_setup`/`stderr_setup`
    /// was changed this *might* panic.
    ///
    /// Pretty much the only way to change a *reserved* `stdout_setup`/`stderr_setup` is by
    /// using taking a `DerefMut` and then replacing the spawn option with another on, so
    /// it's normally not a thing which you will run into.
    pub fn spawn(self) -> Result<Child<Output, Error>, io::Error> {
        let Command {
            spawn_options,
            output_mapping,
            spawn_impl,
            expected_exit_status,
        } = self;

        let child = spawn_impl.spawn(
            spawn_options,
            output_mapping.needs_captured_stdout(),
            output_mapping.needs_captured_stderr(),
        )?;

        Ok(Child {
            child,
            output_mapping,
            expected_exit_status,
        })
    }

    // /// Sets a callback which is called instead of executing the command when running the command.
    // ///
    // /// This is mainly meant to be used for mocking command execution during testing, but can be used for
    // /// other thinks, too. E.g. the current implementation does have a default callback for normally executing
    // /// the command this method was not called.
    // ///
    // ///
    // /// # Implementing Mocks with an exec_replacement_callback
    // ///
    // /// You MUST NOT call following methods in the callback:
    // ///
    // /// - [`Command::run()`], recursively calling run will not work.
    // /// - [`Command::will_capture_stdout()`], the second parameter passed in to the callback has the result of this method
    // /// - [`Command::will_capture_stderr()`], the third parameter passed in to the callback has the result of this method
    // ///
    // /// An emulation of captured output and exit status is returned as [`ExecResult`] instance:
    // ///
    // /// - Any exit code can be returned including a target specific one,
    // ///   the `From<num> for ExitStatus` implementations are useful here.
    // /// - If the second argument is `true` (the one corresponding to [`OutputMapping::capture_stdout()`]) then [`ExecResult::stdout`] must be `Some`
    // ///   else it must be `None`. Failing to do so will panic on unwrap of debug assertions.
    // /// - If  the third argument is `true` (the one corresponding to [`OutputMapping::capture_stdout()`]) then [`ExecResult::stdout`] must be `Some`
    // ///   else it must be `None`. Failing to do so will panic on unwrap of debug assertions.
    // ///
    // /// If used for mocking in tests you already know if stdout/stderr is assumed to (not) be
    // /// captured so you do not need to access [`OutputMapping::capture_stdout()`]/[`OutputMapping::capture_stdout()`].
    // ///
    // /// Settings like env updates and inheritance can be retrieved from the passed in `Command` instance.
    // ///
    // /// # Implement custom subprocess spawning
    // ///
    // /// *Be aware that if you execute the program in the callback you need to make sure the right program, arguments
    // /// stdout/stderr capture setting and env variables are used. Especially note should be taken to how `EnvChange::Inherit`
    // /// is handled.*
    // ///
    // /// The [`Command::create_expected_env_iter()`] method can be used to find what exact env variables
    // /// are expected to be in the sub-process. Clearing the sub-process env and then setting all env vars
    // /// using [`Command::create_expected_env_iter()`] is not the most efficient but most simple and robust
    // /// to changes way to set the env. It's recommended to be used.
    // ///
    // pub fn with_exec_replacement_callback(
    //     mut self,
    //     callback: impl FnOnce(SpawnOptions) -> Result<ExecResult, io::Error> + 'static,
    // ) -> Self {
    //     self.spawn_impl = Box::new(callback);
    //     self
    // }

    //TODO doc
    pub fn with_spawn_impl(
        mut self,
        //TODO Arc<dyn SpawnImpl>??, &'static dyn SpawnImpl ??
        spawn_impl: Box<dyn SpawnImpl>,
    ) -> Self {
        self.spawn_impl = spawn_impl;
        self
    }

    /// Syntax short form for `.with_spawn_impl(crate::mock::mock_result(func))`
    pub fn with_mock_result(
        self,
        func: impl 'static + Fn(SpawnOptions, bool, bool) -> Result<ExecResult, io::Error>,
    ) -> Self {
        self.with_spawn_impl(mock::mock_result(func))
    }

    /// Syntax short form for `.with_spawn_impl(crate::mock::mock_result_once(func))`
    pub fn with_mock_result_once(
        self,
        func: impl 'static + FnOnce(SpawnOptions, bool, bool) -> Result<ExecResult, io::Error>,
    ) -> Self {
        self.with_spawn_impl(mock::mock_result_once(func))
    }

    /// Returns a reference to the used output mapping.
    pub fn output_mapping(&self) -> &dyn OutputMapping<Output = Output, Error = Error> {
        &*self.output_mapping
    }
}

impl<Output, Error> Deref for Command<Output, Error>
where
    Output: 'static,
    Error: From<io::Error> + From<UnexpectedExitStatus> + 'static,
{
    type Target = SpawnOptions;

    fn deref(&self) -> &Self::Target {
        &self.spawn_options
    }
}

impl<Output, Error> DerefMut for Command<Output, Error>
where
    Output: 'static,
    Error: From<io::Error> + From<UnexpectedExitStatus> + 'static,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.spawn_options
    }
}

/// Trait used to configure what [`Command::run()`] returns.
pub trait OutputMapping: 'static {
    /// The output produced by this command, if it is run and doesn't fail.
    type Output: 'static;

    /// The error produced by this command, if it is run and does fail.
    type Error: 'static;

    /// Return if stdout needs to be captured for this output mapping `map_output` function.
    ///
    /// *This should be a pure function only depending on `&self`.*
    ///
    /// This is called when creating the command, storing the result of it
    /// in the command settings.
    fn needs_captured_stdout(&self) -> bool;

    /// Return if stderr needs to be captured for this output mapping `map_output` function.
    ///
    /// *This should be a pure function only depending on `&self`.*
    ///
    /// This is called when creating the command, storing the result of it
    /// in the command settings.
    fn needs_captured_stderr(&self) -> bool;

    /// The function called once the command's run completed.
    ///
    /// This function is used to convert the captured stdout/stderr
    /// to an instance of the given `Output` type.
    ///
    /// If exist code checking is enabled and fails this function will
    /// not be called.
    ///
    /// If it is disabled this function will be called and the implementation
    /// can still decide to fail due to an unexpected/bad exit status.
    fn map_output(self: Box<Self>, result: ExecResult) -> Result<Self::Output, Self::Error>;
}

/// Child Process (Handle).
pub struct Child<Output, Error>
where
    Output: 'static,
    Error: From<io::Error> + From<UnexpectedExitStatus> + 'static,
{
    expected_exit_status: Option<ExitStatus>,
    output_mapping: Box<dyn OutputMapping<Output = Output, Error = Error>>,
    child: Box<dyn ChildHandle>,
}

impl<Output, Error> Child<Output, Error>
where
    Output: 'static,
    Error: From<io::Error> + From<UnexpectedExitStatus> + 'static,
{
    /// Awaits the exit of the child mapping the captured output.
    ///
    /// Depending of the setup this either does start capturing the
    /// output which needs to be captured or just waits until the
    /// output is successfully captured and the process exited.
    ///
    pub fn wait(self) -> Result<Output, Error> {
        let Child {
            child,
            output_mapping,
            expected_exit_status,
        } = self;

        let result = child.wait_with_output()?;

        if let Some(status) = expected_exit_status {
            if status != result.exit_status {
                return Err(UnexpectedExitStatus {
                    got: result.exit_status,
                    expected: status,
                }
                .into());
            }
        }

        output_mapping.map_output(result)
    }
}

impl<Output, Error> Debug for Child<Output, Error>
where
    Output: 'static,
    Error: From<io::Error> + From<UnexpectedExitStatus> + 'static,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        //TODO log at least expected exit status
        f.write_str("Child { ... }")
    }
}

/// The command failed due to an unexpected exit status.
///
/// By default this means the exit status was not 0, but
/// this can be reconfigured.
#[derive(Debug, Error)]
#[error("Unexpected exit status. Got: {got}, Expected: {expected}")]
pub struct UnexpectedExitStatus {
    got: ExitStatus,
    expected: ExitStatus,
}

/// Type used for `exec_replacement_callback` to return mocked output and exit status.
#[derive(Debug, Default)]
pub struct ExecResult {
    /// The exit status the process did exit with.
    pub exit_status: ExitStatus,

    /// The stdout output captured during sub-process execution (if any).
    ///
    /// This must be `Some` if `stdout` is expected to be captured, it must
    /// be `None` if it's expected to not be captured.
    pub stdout: Option<Vec<u8>>,

    /// The stderr output captured during sub-process execution (if any).
    ///
    /// This must be `Some` if `stderr` is expected to be captured, it must
    /// be `None` if it's expected to not be captured.
    pub stderr: Option<Vec<u8>>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;
    use thiserror::Error;

    #[derive(Debug, Error)]
    enum TestCommandError {
        #[error(transparent)]
        Io(#[from] io::Error),

        #[error(transparent)]
        UnexpectedExitStatus(#[from] UnexpectedExitStatus),

        #[error("TestCase error: {0}")]
        Prop(TestCaseError),
    }

    impl From<TestCaseError> for TestCommandError {
        fn from(prop_err: TestCaseError) -> Self {
            Self::Prop(prop_err)
        }
    }

    impl TestCommandError {
        pub fn unwrap_prop(self) -> TestCaseError {
            match self {
                Self::Io(err) => panic!("unexpected io error: {:?}", err),
                Self::UnexpectedExitStatus(err) => panic!("unexpected exit status: {:?}", err),
                Self::Prop(prop_err) => return prop_err,
            }
        }
    }

    struct TestOutputMapping {
        capture_stdout: bool,
        capture_stderr: bool,
    }

    impl OutputMapping for TestOutputMapping {
        type Output = bool;
        type Error = TestCommandError;

        fn needs_captured_stdout(&self) -> bool {
            self.capture_stdout
        }
        fn needs_captured_stderr(&self) -> bool {
            self.capture_stderr
        }

        fn map_output(self: Box<Self>, result: ExecResult) -> Result<Self::Output, Self::Error> {
            (|| {
                prop_assert_eq!(result.stdout.is_some(), self.needs_captured_stdout());
                prop_assert_eq!(result.stderr.is_some(), self.needs_captured_stderr());
                Ok(())
            })()?;
            Ok(true)
        }
    }

    mod Command {
        #![allow(non_snake_case)]

        mod new {
            use std::ffi::OsStr;

            use super::super::super::*;
            use proptest::prelude::*;

            #[test]
            fn comp_can_be_created_using_str_string_os_str_or_os_string() {
                Command::new("ls", ReturnNothing);
                Command::new("ls".to_owned(), ReturnNothing);
                Command::new(OsString::from("ls"), ReturnNothing);
                Command::new(OsStr::new("ls"), ReturnNothing);
            }

            #[test]
            fn comp_when_creating_command_different_capture_modes_can_be_used() {
                Command::new("foo", ReturnNothing);
                Command::new("foo", ReturnStdout);
                Command::new("foo", ReturnStderr);
                Command::new("foo", ReturnStdoutAndErr);
            }

            proptest! {
                #[test]
                fn the_used_program_can_be_queried(s in any::<OsString>()) {
                    let s = OsStr::new(&*s);
                    let cmd = Command::new(s, ReturnNothing);
                    prop_assert_eq!(&*cmd.program, s)
                }
            }
        }

        mod arguments {
            use super::super::super::*;
            use proptest::prelude::*;
            use std::{collections::HashSet, ffi::OsStr, iter};

            #[test]
            fn default_arguments_are_empty() {
                let cmd = Command::new("foo", ReturnNothing);
                assert!(cmd.arguments.is_empty());
            }

            #[test]
            fn comp_arguments_can_be_set_from_iterables() {
                Command::new("foo", ReturnNothing).with_arguments(Vec::<OsString>::new());
                Command::new("foo", ReturnNothing).with_arguments(HashSet::<OsString>::new());
                Command::new("foo", ReturnNothing).with_arguments(&[] as &[OsString]);
            }

            proptest! {
                #[test]
                fn new_arguments_can_be_added(
                    cmd in any::<OsString>(),
                    argument in any::<OsString>(),
                    arguments in proptest::collection::vec(any::<OsString>(), 0..5),
                    arguments2 in proptest::collection::vec(any::<OsString>(), 0..5)
                ) {
                    let cmd = OsStr::new(&*cmd);
                    let cmd = Command::new(cmd, ReturnNothing)
                        .with_arguments(&arguments);
                    prop_assert_eq!(&cmd.arguments, &arguments);
                    let cmd = cmd.with_argument(&argument);
                    prop_assert_eq!(
                        cmd.arguments.iter().collect::<Vec<_>>(),
                        arguments.iter().chain(iter::once(&argument)).collect::<Vec<_>>()
                    );
                    let cmd = cmd.with_arguments(&arguments2);
                    prop_assert_eq!(
                        cmd.arguments.iter().collect::<Vec<_>>(),
                        arguments.iter()
                            .chain(iter::once(&argument))
                            .chain(arguments2.iter())
                            .collect::<Vec<_>>()
                    );
                }
            }
        }

        mod run {
            use super::super::super::*;

            #[test]
            fn run_can_lead_to_and_io_error() {
                let res = Command::new("foo", ReturnNothing)
                    .with_mock_result(|_, _, _| Err(io::Error::new(io::ErrorKind::Other, "random")))
                    .run();

                res.unwrap_err();
            }

            #[test]
            fn return_no_error_if_the_command_has_zero_exit_status() {
                let res = Command::new("foo", ReturnNothing)
                    .with_mock_result(move |_, _, _| {
                        Ok(ExecResult {
                            exit_status: 0.into(),
                            ..Default::default()
                        })
                    })
                    .run();

                res.unwrap();
            }
        }

        mod spawn {
            use std::sync::{
                atomic::{AtomicBool, Ordering},
                Arc,
            };

            use mock::MockResultFn;

            use crate::mock::{MockResult, MockSpawn};

            use super::super::super::*;

            #[test]
            fn can_spawn_and_then_await_outputs() {
                let child = Command::new("foo", ReturnStdoutString)
                    .with_mock_result(move |_, _, _| {
                        Ok(ExecResult {
                            exit_status: 0.into(),
                            stdout: Some("hy".to_owned().into()),
                            ..Default::default()
                        })
                    })
                    .spawn()
                    .unwrap();

                let res = child.wait().unwrap();
                assert_eq!(res, "hy");
            }

            #[test]
            fn spawn_failure_and_wait_failure_are_seperate() {
                Command::new("foo", ReturnNothing)
                    .with_spawn_impl(MockSpawn::new(|_, _, _| {
                        Err(io::Error::new(io::ErrorKind::Other, "failed spawn"))
                    }))
                    .spawn()
                    .unwrap_err();

                let child = Command::new("foo", ReturnNothing)
                    .with_spawn_impl(MockSpawn::new(|_, _, _| {
                        Ok(MockResult::new(Err(io::Error::new(
                            io::ErrorKind::Other,
                            "failed wait",
                        ))))
                    }))
                    .spawn()
                    .unwrap();

                child.wait().unwrap_err();
            }

            #[test]
            fn spawn_already_spawns_wait_only_awaits_completion() {
                let is_running = Arc::new(AtomicBool::new(false));
                let child = Command::new("foo", ReturnNothing)
                    .with_spawn_impl({
                        let is_running = is_running.clone();
                        MockSpawn::new(move |_, _, _| {
                            let is_running = is_running.clone();
                            is_running.store(true, Ordering::SeqCst);
                            Ok(MockResultFn::new(move || {
                                is_running.store(false, Ordering::SeqCst);
                                Ok(ExecResult {
                                    exit_status: 0.into(),
                                    ..Default::default()
                                })
                            }))
                        })
                    })
                    .spawn()
                    .unwrap();

                assert_eq!(is_running.load(Ordering::SeqCst), true);

                let () = child.wait().unwrap();

                assert_eq!(is_running.load(Ordering::SeqCst), false);
            }
        }

        mod OutputMapping {
            use super::super::super::*;
            use super::super::TestOutputMapping;
            use proptest::prelude::*;

            #[test]
            fn comp_command_must_only_be_generic_over_the_output() {
                if false {
                    let mut _cmd = Command::new("foo", ReturnNothing);
                    _cmd = Command::new("foo", ReturnNothingAlt);
                }

                //---
                struct ReturnNothingAlt;
                impl OutputMapping for ReturnNothingAlt {
                    type Output = ();
                    type Error = CommandExecutionError;
                    fn needs_captured_stdout(&self) -> bool {
                        false
                    }
                    fn needs_captured_stderr(&self) -> bool {
                        false
                    }
                    fn map_output(
                        self: Box<Self>,
                        _result: ExecResult,
                    ) -> Result<Self::Output, Self::Error> {
                        unimplemented!()
                    }
                }
            }

            #[test]
            fn allow_custom_errors() {
                let _result: MyError = Command::new("foo", ReturnError)
                    .with_mock_result(|_, _, _| {
                        Ok(ExecResult {
                            exit_status: 0.into(),
                            ..Default::default()
                        })
                    })
                    .run()
                    .unwrap_err();

                //------------
                struct ReturnError;
                impl OutputMapping for ReturnError {
                    type Output = ();
                    type Error = MyError;
                    fn needs_captured_stdout(&self) -> bool {
                        false
                    }
                    fn needs_captured_stderr(&self) -> bool {
                        false
                    }
                    fn map_output(
                        self: Box<Self>,
                        _result: ExecResult,
                    ) -> Result<Self::Output, Self::Error> {
                        Err(MyError::BarFoot)
                    }
                }
                #[derive(Debug, Error)]
                enum MyError {
                    #[error("FooBar")]
                    BarFoot,

                    #[error(transparent)]
                    Io(#[from] io::Error),

                    #[error(transparent)]
                    UnexpectedExitStatus(#[from] UnexpectedExitStatus),
                }
            }

            #[test]
            fn returning_stdout_even_if_needs_captured_stdout_does_not_panic() {
                let _ = Command::new("foo", ReturnNothing)
                    .without_expected_exit_status()
                    .with_mock_result(|_, _, _| {
                        Ok(ExecResult {
                            exit_status: 1.into(),
                            stdout: Some(Vec::new()),
                            ..Default::default()
                        })
                    })
                    .run();
            }
            #[test]
            fn returning_stderr_even_if_needs_captured_stderr_does_not_panic() {
                let _ = Command::new("foo", ReturnNothing)
                    .without_expected_exit_status()
                    .with_mock_result(|_, _, _| {
                        Ok(ExecResult {
                            exit_status: 1.into(),
                            stderr: Some(Vec::new()),
                            ..Default::default()
                        })
                    })
                    .run();
            }

            proptest! {
                #[test]
                fn only_pass_stdout_stderr_to_map_output_if_return_settings_indicate_they_capture_it(
                    capture_stdout in proptest::bool::ANY,
                    capture_stderr in proptest::bool::ANY
                ) {
                    let res = Command::new("foo", TestOutputMapping { capture_stdout, capture_stderr })
                        .with_mock_result(move |_,_,_| {
                            Ok(ExecResult {
                                exit_status: 0.into(),
                                stdout: if capture_stdout { Some(Vec::new()) } else { None },
                                stderr: if capture_stderr { Some(Vec::new()) } else { None }
                            })
                        })
                        .run()
                        .map_err(|e| e.unwrap_prop())?;

                    assert!(res);
                }

                #[test]
                fn command_provides_a_getter_to_check_if_stdout_and_err_will_likely_be_captured(
                    capture_stdout in proptest::bool::ANY,
                    capture_stderr in proptest::bool::ANY
                ) {
                    let cmd = Command::new("foo", TestOutputMapping { capture_stdout, capture_stderr });
                    prop_assert_eq!(cmd.output_mapping().needs_captured_stdout(), capture_stdout);
                    prop_assert!(cmd.custom_stdout_setup.is_none());
                    prop_assert_eq!(cmd.output_mapping().needs_captured_stderr(), capture_stderr);
                    prop_assert!(cmd.custom_stderr_setup.is_none());
                }

                //TODO
                #[ignore = "with opaque Stdio this doesn't work"]
                #[test]
                fn capture_hints_are_available_in_the_callback(
                    _capture_stdout in proptest::bool::ANY,
                    _capture_stderr in proptest::bool::ANY
                ) {
                    // Command::new("foo", TestOutputMapping { capture_stdout, capture_stderr })
                    //     .with_exec_replacement_callback(move |cmd| {
                    //         assert_eq!(return_settings.needs_captured_stdout(), capture_stdout);
                    //         assert_eq!(return_settings.needs_captured_stderr(), capture_stderr);
                    //         Ok(ExecResult {
                    //             exit_status: 0.into(),
                    //             stdout: if capture_stdout { Some(Vec::new()) } else { None },
                    //             stderr: if capture_stderr { Some(Vec::new()) } else { None }
                    //         })
                    //     })
                    //     .run()
                    //     .unwrap();
                }

                #[test]
                fn new_env_variables_can_be_added(
                    cmd in any::<OsString>(),
                    variable in any::<OsString>(),
                    value in any::<OsString>(),
                    map1 in proptest::collection::hash_map(
                        any::<OsString>(),
                        any::<OsString>().prop_map(|s| EnvChange::Set(s)),
                        0..4
                    ),
                    map2 in proptest::collection::hash_map(
                        any::<OsString>(),
                        any::<OsString>().prop_map(|s| EnvChange::Set(s)),
                        0..4
                    ),
                ) {
                    let cmd = Command::new(cmd, ReturnNothing)
                        .with_env_updates(&map1);

                    prop_assert_eq!(&cmd.env_updates, &map1);

                    let cmd = cmd.with_env_update(&variable, &value);

                    let mut n_map = map1.clone();
                    n_map.insert(variable, EnvChange::Set(value));
                    prop_assert_eq!(&cmd.env_updates, &n_map);

                    let cmd = cmd.with_env_updates(&map2);

                    for (key, value) in &map2 {
                        n_map.insert(key.into(), value.into());
                    }
                    prop_assert_eq!(&cmd.env_updates, &n_map);
                }
            }
        }
        mod environment {
            use std::{borrow::Cow, collections::HashMap};

            use super::super::super::*;

            #[test]
            fn by_default_no_environment_updates_are_done() {
                let cmd = Command::new("foo", ReturnNothing);
                assert!(cmd.env_updates.is_empty());
            }

            #[test]
            fn create_expected_env_iter_includes_the_current_env_by_default() {
                let process_env = ::std::env::vars_os()
                    .into_iter()
                    .map(|(k, v)| (Cow::Owned(k), Cow::Owned(v)))
                    .collect::<HashMap<_, _>>();
                let cmd = Command::new("foo", ReturnNothing);
                let created_map = cmd.create_expected_env_iter().collect::<HashMap<_, _>>();
                assert_eq!(process_env, created_map);
            }

            #[test]
            fn by_default_env_is_inherited() {
                let cmd = Command::new("foo", ReturnNothing);
                assert_eq!(cmd.inherit_env, true);
                //FIXME fluky if there is no single ENV variable set
                //But this kinda can't happen as the test environment set's some
                assert_ne!(cmd.create_expected_env_iter().count(), 0);
            }

            #[test]
            fn inheritance_of_env_variables_can_be_disabled() {
                let cmd = Command::new("foo", ReturnNothing).with_inherit_env(false);
                assert_eq!(cmd.inherit_env, false);
                assert_eq!(cmd.create_expected_env_iter().count(), 0);
            }
        }

        mod working_directory {
            use super::super::super::*;
            use crate::utils::opt_arbitrary_path_buf;
            use proptest::prelude::*;

            #[test]
            fn by_default_no_explicit_working_directory_is_set() {
                let cmd = Command::new("foo", ReturnNothing);
                assert_eq!(cmd.working_directory_override.as_ref(), None);
            }

            proptest! {
                #[test]
                fn the_working_directory_can_be_changed(
                    cmd in any::<OsString>(),
                    wd_override in opt_arbitrary_path_buf(),
                    wd_override2 in opt_arbitrary_path_buf()
                ) {
                    let cmd = Command::new(cmd, ReturnNothing)
                        .with_working_directory_override(wd_override.as_ref());

                    assert_eq!(cmd.working_directory_override.as_ref(), wd_override.as_ref());

                    let cmd = cmd.with_working_directory_override(wd_override2.as_ref());
                    assert_eq!(cmd.working_directory_override.as_ref(), wd_override2.as_ref());
                }
            }
        }

        mod exit_status_checking {
            use super::super::super::*;
            use proptest::prelude::*;

            #[test]
            fn by_default_the_expected_exit_status_is_0() {
                let cmd = Command::new("foo", ReturnNothing);
                assert_eq!(cmd.expected_exit_status.as_ref().unwrap(), &0);
            }

            #[test]
            fn by_default_exit_status_checking_is_enabled() {
                let cmd = Command::new("foo", ReturnNothing);
                assert_eq!(cmd.expected_exit_status.is_some(), true);
            }

            #[test]
            fn setting_check_exit_status_to_false_disables_it() {
                Command::new("foo", ReturnNothing)
                    .without_expected_exit_status()
                    .with_mock_result(|_, _, _| {
                        Ok(ExecResult {
                            exit_status: 1.into(),
                            ..Default::default()
                        })
                    })
                    .run()
                    .unwrap();
            }

            #[test]
            fn you_can_expect_no_exit_status_to_be_returned() {
                let cmd = Command::new("foo", ReturnNothing).with_expected_exit_status(
                    ExitStatus::OsSpecific(OpaqueOsExitStatus::target_specific_default()),
                );

                assert_eq!(
                    &cmd.expected_exit_status,
                    &Some(ExitStatus::OsSpecific(
                        OpaqueOsExitStatus::target_specific_default()
                    ))
                );
            }

            #[test]
            fn setting_the_expected_exit_status_will_enable_checking() {
                let cmd = Command::new("foo", ReturnNothing)
                    .without_expected_exit_status()
                    .with_expected_exit_status(0);

                assert_eq!(cmd.expected_exit_status.is_some(), true);
            }

            proptest! {
                #[test]
                fn return_an_error_if_the_command_has_non_zero_exit_status(
                    cmd in any::<OsString>(),
                    exit_status in prop_oneof!(..0, 1..).prop_map(ExitStatus::from)
                ) {
                    let res = Command::new(cmd, ReturnNothing)
                        .with_mock_result(move |_,_,_| {
                            Ok(ExecResult {
                                exit_status,
                                ..Default::default()
                            })
                        })
                        .run();

                    res.unwrap_err();
                }

                #[test]
                fn replacing_the_expected_exit_status_causes_error_on_different_exit_status(
                    exit_status in -5..6,
                    offset in prop_oneof!(-100..0, 1..101)
                ) {
                    let res = Command::new("foo", ReturnNothing)
                        .with_expected_exit_status(exit_status)
                        .with_mock_result(move |_,_,_| {
                            Ok(ExecResult {
                                exit_status: ExitStatus::from(exit_status + offset),
                                ..Default::default()
                            })
                        })
                        .run();

                    match res {
                        Err(CommandExecutionError::UnexpectedExitStatus(UnexpectedExitStatus {got, expected})) => {
                            assert_eq!(expected, exit_status);
                            assert_eq!(got, exit_status+offset);
                        },
                        _ => panic!("Unexpected Result: {:?}", res)
                    }
                }
            }
        }

        mod exec_replacement_callback {
            use std::{cell::RefCell, rc::Rc};

            use super::super::super::*;

            #[test]
            fn program_execution_can_be_replaced_with_an_callback() {
                let was_run = Rc::new(RefCell::new(false));
                let was_run_ = was_run.clone();
                let cmd = Command::new("some_cmd", ReturnStdoutAndErr).with_mock_result(
                    move |options, capture_stdout, capture_stderr| {
                        assert_eq!(capture_stdout, true);
                        assert_eq!(capture_stderr, true);
                        *(*was_run_).borrow_mut() = true;
                        assert_eq!(&options.program, "some_cmd");
                        Ok(ExecResult {
                            exit_status: 0.into(),
                            stdout: Some("result=12".to_owned().into()),
                            stderr: Some(Vec::new()),
                        })
                    },
                );

                let res = cmd.run().unwrap();
                assert_eq!(*was_run.borrow_mut(), true);
                assert_eq!(&*res.stdout, "result=12".as_bytes());
                assert_eq!(&*res.stderr, "".as_bytes());
            }
        }
    }
}
