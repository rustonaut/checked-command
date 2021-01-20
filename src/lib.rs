pub use self::return_settings::*;
use std::{
    borrow::Cow,
    collections::HashMap,
    env::{self, VarsOs},
    ffi::{OsStr, OsString},
    fmt,
    fmt::Display,
    io,
    path::{Path, PathBuf},
};
use thiserror::Error;

#[macro_use]
mod utils;
mod return_settings;
mod sys;

pub struct Command<Output, Error>
where
    Output: 'static,
    Error: From<CommandExecutionError> + 'static,
{
    program: OsString,
    arguments: Vec<OsString>,
    env_updates: HashMap<OsString, EnvChange>,
    working_directory_override: Option<PathBuf>,
    expected_exit_code: ExitCode,
    check_exit_code: bool,
    inherit_env: bool,
    return_settings: Option<Box<dyn ReturnSettings<Output = Output, Error = Error>>>,
    run_callback: Option<
        Box<
            dyn FnOnce(
                Self,
                &dyn ReturnSettings<Output = Output, Error = Error>,
            ) -> Result<ExecResult, io::Error>,
        >,
    >,
}

impl<Output, Error> Command<Output, Error>
where
    Output: 'static,
    Error: From<CommandExecutionError> + 'static,
{
    /// Create a new command for given program and return setting.
    ///
    /// The return settings will imply if stdout/stderr is captured and how the
    /// captured output is mapped to a `Result<Self::Output, Self::Error>`.
    ///
    pub fn new(
        program: impl Into<OsString>,
        return_settings: impl ReturnSettings<Output = Output, Error = Error>,
    ) -> Self {
        Command {
            program: program.into(),
            arguments: Vec::new(),
            env_updates: HashMap::new(),
            check_exit_code: true,
            inherit_env: true,
            expected_exit_code: ExitCode::Some(0),
            return_settings: Some(Box::new(return_settings) as _),
            working_directory_override: None,
            run_callback: Some(Box::new(sys::actual_exec_exec_replacement_callback)),
        }
    }

    /// Return the program the command will run.
    pub fn program(&self) -> &OsStr {
        &*self.program
    }

    /// Returns the arguments passed the the program when run.
    pub fn arguments(&self) -> &[OsString] {
        &self.arguments
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

    /// Return a map of all env variables which will be set/overwritten in the subprocess.
    ///
    /// # Warning
    ///
    /// The keys of env variables have not *not* been evaluated for syntactic validity.
    /// So the given keys can cause process spawning or calls to [`std::env::set_var`] to
    /// fail.
    pub fn env_updates(&self) -> &HashMap<OsString, EnvChange> {
        &self.env_updates
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
    /// See [`Self.with_env_updates()`].
    pub fn with_env_update(
        mut self,
        key: impl Into<OsString>,
        value: impl Into<EnvChange>,
    ) -> Self {
        self.env_updates.insert(key.into(), value.into());
        self
    }

    /// Returns true if the env of the current process is inherited.
    ///
    /// Updates to then environment are applied after the inheritance:
    ///
    /// - [`EnvChange::Set`] can be use to override inherited env vars, or
    ///   add new ones if no variable with given key was inherited
    /// - [`EnvChange::Remove`] can be used to remove an inherited (or previously added)
    ///   env variable
    /// - [`EnvChange::Inherit`] can be used to state a env variable should be inherited
    ///   even if `inherit_env` is `false`. If `inherit_env` is true this will have no
    ///   effect.
    pub fn inherit_env(&self) -> bool {
        self.inherit_env
    }

    /// Returns this command with a change to weather or the sub-process will inherit env variables.
    ///
    /// See [`Self.inherit_env()`] for how this affects the sub-process env.
    pub fn with_inherit_env(mut self, do_inherit: bool) -> Self {
        self.inherit_env = do_inherit;
        self
    }

    /// Returns a map with all env variables the sub-process spawned by this command would have
    /// if the current processes env is not changed.
    ///
    /// # Site note about `env::set_var()` problems
    ///
    /// Note that if you use `std::env::set_var()` in a multi-threaded setup depending on
    /// the operating system you run this on this can lead to all kind of problem, including
    /// unexpected race conditions in some situations (especially if `inherit_env(true)` is
    /// combined with `EnvChange::Inherit` and multiple variables are changed in another thread
    /// racing with this function and some but not all are covered by `EnvChange::Inherit`).
    ///
    /// Given that [`std::env::set_var()`] should strictly be avoided in a multi-threaded context
    /// this is seen as an acceptable drawback.
    ///
    /// Note that this function + `std::env::set_var()` is not unsafe it might just have a
    /// very unexpected result. Except if `env::set_var()` + reading env races are inherently
    /// unsafe on your system, in which case this has nothing to do with this function.
    pub fn create_expected_env_iter(&self) -> impl Iterator<Item = (Cow<OsStr>, Cow<OsStr>)> {
        let inherit = if self.inherit_env() {
            Some(env::vars_os())
        } else {
            None
        };

        return ExpectedEnvIter {
            self_: self,
            inherit,
            update: Some(self.env_updates.iter()),
        };

        //FIXME[rust/generators] use yield base iterator
        struct ExpectedEnvIter<'a, Output, Error>
        where
            Output: 'static,
            Error: From<CommandExecutionError> + 'static,
        {
            self_: &'a Command<Output, Error>,
            inherit: Option<VarsOs>,
            update: Option<std::collections::hash_map::Iter<'a, OsString, EnvChange>>,
        }

        impl<'a, O, E> Iterator for ExpectedEnvIter<'a, O, E>
        where
            O: 'static,
            E: From<CommandExecutionError> + 'static,
        {
            type Item = (Cow<'a, OsStr>, Cow<'a, OsStr>);

            fn next(&mut self) -> Option<Self::Item> {
                loop {
                    fused_opt_iter_next!(&mut self.inherit, |(key, val)| {
                        match self.self_.env_updates.get(&key) {
                            Some(_) => continue,
                            None => return Some((Cow::Owned(key), Cow::Owned(val))),
                        }
                    });
                    fused_opt_iter_next!(&mut self.update, |(key, change)| {
                        match change {
                            EnvChange::Set(val) => {
                                return Some((Cow::Borrowed(&key), Cow::Borrowed(&val)));
                            }
                            EnvChange::Inherit => {
                                // Mostly used if inherit_var is valse in which case we *should* not
                                // have done aboves loop-part on vars_os. We could "optimize" this to
                                // handle Inherit in aboves loop if we run that loop, but why add that
                                // complexity?
                                if let Some(val) = env::var_os(&key) {
                                    return Some((Cow::Borrowed(&key), Cow::Owned(val)));
                                } else {
                                    continue;
                                }
                            }
                            EnvChange::Remove => {
                                continue;
                            }
                        }
                    });
                    return None;
                }
            }
        }
    }

    /// Return the working directory which will be used instead of the current working directory.
    ///
    /// If `None` is returned it means no override is set and the working directory will be inherited
    /// from the spawning process.
    pub fn working_directory_override(&self) -> Option<&Path> {
        self.working_directory_override.as_ref().map(|s| &**s)
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

    /// Return which exit code is treated as success.
    pub fn expected_exit_code(&self) -> ExitCode {
        self.expected_exit_code
    }

    /// Set which exit code is treated as successful.
    ///
    /// **This enables exit code checking even if it
    ///   was turned of before.**
    pub fn with_expected_exit_code(self, exit_code: impl Into<ExitCode>) -> Self {
        let mut cmd = self.with_check_exit_code(true);
        cmd.expected_exit_code = exit_code.into();
        cmd
    }

    /// Returns true if the exit code is checked before mapping the output(s).
    pub fn check_exit_code(&self) -> bool {
        self.check_exit_code
    }

    /// Sets if the exit code is checked before mapping the output(s).
    pub fn with_check_exit_code(mut self, val: bool) -> Self {
        self.check_exit_code = val;
        self
    }

    /// Returns true if stdout will be captured.
    ///
    /// # Panics
    ///
    /// **If called in a `exec_replacement_callback` this will panic.
    pub fn will_capture_stdout(&self) -> bool {
        self.return_settings
            .as_ref()
            .expect("Can not be called in a exec_replacement_callback.")
            .capture_stdout()
    }

    /// Returns true if stderr will be captured.
    ///
    /// # Panics
    ///
    /// **If called in a `exec_replacement_callback` this will panic.
    pub fn will_capture_stderr(&self) -> bool {
        self.return_settings
            .as_ref()
            .expect("Can not be called in a exec_replacement_callback.")
            .capture_stderr()
    }

    /// Run the command, blocking until completion and then mapping the output.
    ///
    /// This will:
    ///
    /// 1. run the program with the specified arguments and env variables
    /// 2. capture the necessary outputs as specified by the return settings
    /// 3. if exit code checking was not disabled check the exit code and potentially
    ///    fail.
    /// 4. if 3 doesn't fail now map captured outputs to a `Result<Output, Error>`
    ///
    /// If [`Self.with_exec_replacement_callback()`] is used instead of running the
    /// program and capturing the output the given callback is called. The callback
    /// could mock the program execution. The exit code checking and output mapping
    /// are still done as normal.
    ///
    /// # Panics
    ///
    /// **This will panic if called in a `exec_replacement_callback`.**
    pub fn run(mut self) -> Result<Output, Error> {
        let expected_exit_code = self.expected_exit_code;
        let check_exit_code = self.check_exit_code;
        let return_settings = self
            .return_settings
            .take()
            .expect("run recursively called in exec replacing callback");
        let run_callback = self
            .run_callback
            .take()
            .expect("run recursively called in exec replacing callback");

        let result = run_callback(self, &*return_settings)
            .map_err(|err| CommandExecutionError::SpawningProcessFailed(err))?;

        if check_exit_code && result.exit_code != expected_exit_code {
            Err(Error::from(CommandExecutionError::UnexpectedExitCode {
                got: result.exit_code,
                expected: expected_exit_code,
            }))
        } else {
            let stdout = if return_settings.capture_stdout() {
                result.stdout
            } else {
                debug_assert!(result.stdout.is_none());
                None
            };
            let stderr = if return_settings.capture_stderr() {
                result.stderr
            } else {
                debug_assert!(result.stderr.is_none());
                None
            };
            let exit_code = result.exit_code;
            return_settings.map_output(stdout, stderr, exit_code)
        }
    }

    /// Sets a callback which is called instead of executing the command when running the command.
    ///
    /// While the callback get a instance of this type some fields have been extracted which means
    /// that some method can not be called inside of the callback and will panic if you do so:
    ///
    /// - [`Self.run()`], recursively calling run will not work.
    /// - [`Self.will_capture_stdout()`], use the passed in return settings [`ReturnSetting.capture_stdout()`] method instead.
    /// - [`Self.will_capture_stderr()`], use the passed in return settings [`ReturnSetting.capture_stderr()`] method instead.
    ///
    /// This is mainly meant to be used for mocking command execution during testing, but can be used for
    /// other thinks, too. E.g. the current implementation does have a default callback for normally executing
    /// the command this method was not called.
    ///
    /// Be aware that if you execute the program in the callback you need to make sure the right program, arguments
    /// stdout/stderr capture setting and env variables are used. Especially note should be taken to how `EnvChange::Inherit`
    /// is handled.
    pub fn with_exec_replacement_callback(
        mut self,
        callback: impl FnOnce(
                Self,
                &dyn ReturnSettings<Output = Output, Error = Error>,
            ) -> Result<ExecResult, io::Error>
            + 'static,
    ) -> Self {
        self.run_callback = Some(Box::new(callback));
        self
    }
}

/// Trait used to configure what [`Command::run()`] returns.
pub trait ReturnSettings: 'static {
    /// The output produced by this command, if it is run and doesn't fail.
    type Output: 'static;

    /// The error produced by this command, if it is run and does fail.
    type Error: 'static;

    /// Return if stdout needs to be captured for this return settings `map_output` function.
    ///
    /// *This should be a pure function only depending on `&self`.*
    fn capture_stdout(&self) -> bool;

    /// Return if stderr needs to be captured for this return settings `map_output` function.
    ///
    /// *This should be a pure function only depending on `&self`.*
    fn capture_stderr(&self) -> bool;

    /// The function called once the command's run completed.
    ///
    /// This function is used to convert the captured stdout/stderr
    /// to an instance of the given `Output` type.
    ///
    /// If exist code checking is enabled and fails this function will
    /// not be called.
    ///
    /// If it is disabled this function will be called and the implementation
    /// can still decide to fail due to an unexpected/bad exit code.
    fn map_output(
        self: Box<Self>,
        stdout: Option<Vec<u8>>,
        stderr: Option<Vec<u8>>,
        exit_code: ExitCode,
    ) -> Result<Self::Output, Self::Error>;
}

/// The most basic error which can be produced by running a command.
#[derive(Debug, Error)]
pub enum CommandExecutionError {
    /// Spawning the process failed.
    ///
    /// This can happen because of a variety of reasons, like the os
    /// preventing it or the program not being found.
    #[error("Spawning process failed: {}", _0)]
    SpawningProcessFailed(#[from] io::Error),

    /// The process exited with an unexpected exit code.
    ///
    /// By default this means the exit code was not 0, but
    /// this can be changed.
    #[error("Unexpected exit code. Got: {got}, Expected: {expected}")]
    UnexpectedExitCode { got: ExitCode, expected: ExitCode },
}

/// Type representing a process exit code.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ExitCode {
    /// Normally processes exit with a i32 exit code.
    ///
    /// Depending on the systems less then i32::MAX exit codes
    /// are supported! As this is only used in return position this
    /// is fine but e.g. if you pass this to `std::process::exit` it
    /// might be clamped to e.g. the lower 8 bit.
    Some(i32),

    /// On some systems (e.g. unix) a process might exit without exit code.
    ///
    /// This mainly happens if the process is terminated using certain signals
    /// (e.g. on unix 9).
    ///
    /// If there is an alternative value and what meaning it has is OS specific.
    /// Furthermore there are OS specific ways to encode both exit code and signal
    /// code into one value but they are OS specific, not unix specific and as such
    /// it's not a good idea to handle such values without decoding it!
    ProcessTerminatedBeforeExiting,
}

impl Display for ExitCode {
    fn fmt(&self, fter: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Some(code) => Display::fmt(code, fter),
            Self::ProcessTerminatedBeforeExiting => fter.write_str("terminated before exit"),
        }
    }
}

impl Default for ExitCode {
    fn default() -> Self {
        Self::Some(0)
    }
}

impl From<i32> for ExitCode {
    fn from(code: i32) -> Self {
        ExitCode::Some(code)
    }
}

impl PartialEq<i32> for ExitCode {
    fn eq(&self, other: &i32) -> bool {
        match self {
            Self::Some(code) => code == other,
            Self::ProcessTerminatedBeforeExiting => false,
        }
    }
}

/// Used to determine how a env variable should be updated.
#[derive(Debug, Clone, PartialEq)]
pub enum EnvChange {
    /// Remove the env value if it normally would have been set
    ///
    /// (e.g. because of inherited environment)
    Remove,

    /// Make sure the env variable will have given value in the sub-process.
    Set(OsString),

    /// Make sure the env variable is inherited from the process spawning the sub-process.
    ///
    /// If environment inheritance is disabled (e.g. using `with_inherit_env(false)`) this
    /// will cause given values to still be inherited anyway.
    ///
    /// If environment inheritance is enabled this won't have any effect.
    Inherit,
}

impl From<&Self> for EnvChange {
    fn from(borrow: &Self) -> Self {
        borrow.clone()
    }
}
impl From<&OsString> for EnvChange {
    fn from(val: &OsString) -> Self {
        EnvChange::Set(val.clone())
    }
}

impl From<OsString> for EnvChange {
    fn from(val: OsString) -> Self {
        EnvChange::Set(val)
    }
}

impl From<&OsStr> for EnvChange {
    fn from(val: &OsStr) -> Self {
        EnvChange::Set(val.into())
    }
}

impl From<String> for EnvChange {
    fn from(val: String) -> Self {
        EnvChange::Set(val.into())
    }
}

impl From<&str> for EnvChange {
    fn from(val: &str) -> Self {
        EnvChange::Set(val.into())
    }
}

#[derive(Debug, Default)]
pub struct ExecResult {
    pub exit_code: ExitCode,
    pub stdout: Option<Vec<u8>>,
    pub stderr: Option<Vec<u8>>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    #[derive(Debug)]
    enum TestCommandError {
        Lib(CommandExecutionError),
        Prop(TestCaseError),
    }

    impl From<CommandExecutionError> for TestCommandError {
        fn from(cee: CommandExecutionError) -> Self {
            Self::Lib(cee)
        }
    }

    impl From<TestCaseError> for TestCommandError {
        fn from(cee: TestCaseError) -> Self {
            Self::Prop(cee)
        }
    }

    impl TestCommandError {
        pub fn unwrap_prop(self) -> TestCaseError {
            match self {
                Self::Lib(err) => panic!("unexpected error: {:?}", err),
                Self::Prop(prop_err) => return prop_err,
            }
        }
    }

    struct TestReturnSetting {
        capture_stdout: bool,
        capture_stderr: bool,
    }

    impl ReturnSettings for TestReturnSetting {
        type Output = bool;
        type Error = TestCommandError;

        fn capture_stdout(&self) -> bool {
            self.capture_stdout
        }
        fn capture_stderr(&self) -> bool {
            self.capture_stderr
        }

        fn map_output(
            self: Box<Self>,
            stdout: Option<Vec<u8>>,
            stderr: Option<Vec<u8>>,
            _exit_code: ExitCode,
        ) -> Result<Self::Output, Self::Error> {
            (|| {
                prop_assert_eq!(stdout.is_some(), self.capture_stdout());
                prop_assert_eq!(stderr.is_some(), self.capture_stderr());
                Ok(())
            })()?;
            Ok(true)
        }
    }

    mod Command {
        #![allow(non_snake_case)]

        mod new {
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
                    prop_assert_eq!(&*cmd.program(), s)
                }
            }
        }

        mod arguments {
            use super::super::super::*;
            use proptest::prelude::*;
            use std::{collections::HashSet, iter};

            #[test]
            fn default_arguments_are_empty() {
                let cmd = Command::new("foo", ReturnNothing);
                assert!(cmd.arguments().is_empty());
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
                    prop_assert_eq!(cmd.arguments(), &arguments);
                    let cmd = cmd.with_argument(&argument);
                    prop_assert_eq!(
                        cmd.arguments().iter().collect::<Vec<_>>(),
                        arguments.iter().chain(iter::once(&argument)).collect::<Vec<_>>()
                    );
                    let cmd = cmd.with_arguments(&arguments2);
                    prop_assert_eq!(
                        cmd.arguments().iter().collect::<Vec<_>>(),
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
                    .with_exec_replacement_callback(|_, _| {
                        Err(io::Error::new(io::ErrorKind::Other, "random"))
                    })
                    .run();

                res.unwrap_err();
            }

            #[test]
            fn return_no_error_if_the_command_has_zero_exit_status() {
                let res = Command::new("foo", ReturnNothing)
                    .with_exec_replacement_callback(move |_, _| {
                        Ok(ExecResult {
                            exit_code: 0.into(),
                            ..Default::default()
                        })
                    })
                    .run();

                res.unwrap();
            }
        }

        mod ReturnSetting {
            use super::super::super::*;
            use super::super::TestReturnSetting;
            use proptest::prelude::*;

            #[test]
            fn comp_command_must_only_be_generic_over_the_output() {
                if false {
                    let mut _cmd = Command::new("foo", ReturnNothing);
                    _cmd = Command::new("foo", ReturnNothingAlt);
                }

                //---
                struct ReturnNothingAlt;
                impl ReturnSettings for ReturnNothingAlt {
                    type Output = ();
                    type Error = CommandExecutionError;
                    fn capture_stdout(&self) -> bool {
                        false
                    }
                    fn capture_stderr(&self) -> bool {
                        false
                    }
                    fn map_output(
                        self: Box<Self>,
                        _stdout: Option<Vec<u8>>,
                        _stderr: Option<Vec<u8>>,
                        _exit_code: ExitCode,
                    ) -> Result<Self::Output, Self::Error> {
                        unimplemented!()
                    }
                }
            }

            #[test]
            fn allow_custom_errors() {
                let _result: MyError = Command::new("foo", ReturnError)
                    .with_exec_replacement_callback(|_, _| {
                        Ok(ExecResult {
                            exit_code: 0.into(),
                            ..Default::default()
                        })
                    })
                    .run()
                    .unwrap_err();

                //------------
                struct ReturnError;
                impl ReturnSettings for ReturnError {
                    type Output = ();
                    type Error = MyError;
                    fn capture_stdout(&self) -> bool {
                        false
                    }
                    fn capture_stderr(&self) -> bool {
                        false
                    }
                    fn map_output(
                        self: Box<Self>,
                        _stdout: Option<Vec<u8>>,
                        _stderr: Option<Vec<u8>>,
                        _exit_code: ExitCode,
                    ) -> Result<Self::Output, Self::Error> {
                        Err(MyError::BarFoot)
                    }
                }
                #[derive(Debug, Error)]
                enum MyError {
                    #[error("FooBar")]
                    BarFoot,

                    #[error(transparent)]
                    CommandExecutionError(#[from] CommandExecutionError),
                }
            }

            #[should_panic]
            #[test]
            fn returning_stdout_which_should_not_be_captured_triggers_a_debug_assertion() {
                let _ = Command::new("foo", ReturnNothing)
                    .with_check_exit_code(false)
                    .with_exec_replacement_callback(|_, _| {
                        Ok(ExecResult {
                            exit_code: 1.into(),
                            stdout: Some(Vec::new()),
                            ..Default::default()
                        })
                    })
                    .run();
            }

            #[should_panic]
            #[test]
            fn returning_stderr_which_should_not_be_captured_triggers_a_debug_assertion() {
                let _ = Command::new("foo", ReturnNothing)
                    .with_check_exit_code(false)
                    .with_exec_replacement_callback(|_, _| {
                        Ok(ExecResult {
                            exit_code: 1.into(),
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
                    let res = Command::new("foo", TestReturnSetting { capture_stdout, capture_stderr })
                        .with_exec_replacement_callback(move |_,_| {
                            Ok(ExecResult {
                                exit_code: 0.into(),
                                stdout: if capture_stdout { Some(Vec::new()) } else { None },
                                stderr: if capture_stderr { Some(Vec::new()) } else { None }
                            })
                        })
                        .run()
                        .map_err(|e| e.unwrap_prop())?;

                    assert!(res);
                }

                #[test]
                fn command_provides_a_getter_to_check_if_stdout_and_err_will_be_captured(
                    capture_stdout in proptest::bool::ANY,
                    capture_stderr in proptest::bool::ANY
                ) {
                    let cmd = Command::new("foo", TestReturnSetting { capture_stdout, capture_stderr });
                    prop_assert_eq!(cmd.will_capture_stdout(), capture_stdout);
                    prop_assert_eq!(cmd.will_capture_stderr(), capture_stderr);
                }

                #[test]
                fn capture_hints_are_available_in_the_callback(
                    capture_stdout in proptest::bool::ANY,
                    capture_stderr in proptest::bool::ANY
                ) {
                    Command::new("foo", TestReturnSetting { capture_stdout, capture_stderr })
                        .with_exec_replacement_callback(move |_cmd, return_settings| {
                            assert_eq!(return_settings.capture_stdout(), capture_stdout);
                            assert_eq!(return_settings.capture_stderr(), capture_stderr);
                            Ok(ExecResult {
                                exit_code: 0.into(),
                                stdout: if capture_stdout { Some(Vec::new()) } else { None },
                                stderr: if capture_stderr { Some(Vec::new()) } else { None }
                            })
                        })
                        .run()
                        .unwrap();
                }
            }
        }
        mod environment {
            use super::super::super::*;
            use proptest::prelude::*;

            #[test]
            fn by_default_no_environment_updates_are_done() {
                let cmd = Command::new("foo", ReturnNothing);
                assert!(cmd.env_updates().is_empty());
            }

            #[test]
            fn create_expected_env_iter_includes_the_current_env_by_default() {
                let process_env = env::vars_os()
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
                assert_eq!(cmd.inherit_env(), true);
                //FIXME fluky if there is no single ENV variable set
                assert_ne!(cmd.create_expected_env_iter().count(), 0);
            }

            #[test]
            fn inheritance_of_env_variables_can_be_disabled() {
                let cmd = Command::new("foo", ReturnNothing).with_inherit_env(false);
                assert_eq!(cmd.inherit_env(), false);
                assert_eq!(cmd.create_expected_env_iter().count(), 0);
            }

            proptest! {
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

                    prop_assert_eq!(cmd.env_updates(), &map1);

                    let cmd = cmd.with_env_update(&variable, &value);

                    let mut n_map = map1.clone();
                    n_map.insert(variable, EnvChange::Set(value));
                    prop_assert_eq!(cmd.env_updates(), &n_map);

                    let cmd = cmd.with_env_updates(&map2);

                    for (key, value) in &map2 {
                        n_map.insert(key.into(), value.into());
                    }
                    prop_assert_eq!(cmd.env_updates(), &n_map);
                }


                //FIXME on CI this test can leak secrets if it fails
                #[test]
                fn env_variables_can_be_set_to_be_removed_from_inherited_env(
                    cmd in any::<OsString>(),
                    rem_key in proptest::sample::select(env::vars_os().map(|(k,_v)| k).collect::<Vec<_>>())
                ) {
                    let cmd = Command::new(cmd, ReturnNothing).with_env_update(rem_key.clone(), EnvChange::Remove);
                    prop_assert_eq!(cmd.env_updates().get(&rem_key), Some(&EnvChange::Remove));

                    let produced_env = cmd.create_expected_env_iter()
                        .map(|(k,v)| (k.into_owned(), v.into_owned()))
                        .collect::<HashMap<OsString, OsString>>();

                    prop_assert_eq!(produced_env.get(&rem_key), None);
                }

                //FIXME on CI this test can leak secrets if it fails
                #[test]
                fn env_variables_can_be_set_to_be_replaced_from_inherited_env(
                    cmd in any::<OsString>(),
                    rem_key in proptest::sample::select(env::vars_os().map(|(k,_v)| k).collect::<Vec<_>>()),
                    replacement in any::<OsString>()
                ) {
                    let cmd = Command::new(cmd, ReturnNothing).with_env_update(rem_key.clone(), EnvChange::Set(replacement.clone()));
                    let expect = EnvChange::Set(replacement.clone());
                    prop_assert_eq!(cmd.env_updates().get(&rem_key), Some(&expect));
                    let produced_env = cmd.create_expected_env_iter()
                        .map(|(k,v)| (k.into_owned(), v.into_owned()))
                        .collect::<HashMap<OsString, OsString>>();

                    prop_assert_eq!(produced_env.get(&rem_key), Some(&replacement));
                }

                //FIXME on CI this test can leak secrets if it fails
                #[test]
                fn env_variables_can_be_set_to_inherit_even_if_inheritance_is_disabled(
                    cmd in any::<OsString>(),
                    inherit in proptest::sample::select(env::vars_os().map(|(k,_v)| k).collect::<Vec<_>>()),
                )  {
                    let expected_val = env::var_os(&inherit);
                    let cmd = Command::new(cmd, ReturnNothing)
                        .with_inherit_env(false)
                        .with_env_update(&inherit, EnvChange::Inherit);

                    assert_eq!(cmd.create_expected_env_iter().count(), 1);
                    let got_value = cmd.create_expected_env_iter().find(|(k,_v)| &*k==&*inherit)
                        .map(|(_k,v)| v);
                    assert_eq!(
                        expected_val.as_ref().map(|v|&**v),
                        got_value.as_ref().map(|v|&**v)
                    );
                }

                #[test]
                fn env_variables_can_be_set_to_inherit_even_if_inheritance_is_disabled_2(
                    cmd in any::<OsString>(),
                    inherit in proptest::sample::select(env::vars_os().map(|(k,_v)| k).collect::<Vec<_>>()),
                )  {
                    let expected_val = env::var_os(&inherit);
                    let cmd = Command::new(cmd, ReturnNothing)
                        .with_env_update(&inherit, EnvChange::Inherit)
                        .with_inherit_env(false);

                    assert_eq!(cmd.create_expected_env_iter().count(), 1);
                    let got_value = cmd.create_expected_env_iter().find(|(k,_v)| &*k==&*inherit)
                        .map(|(_k,v)| v);
                    assert_eq!(
                        expected_val.as_ref().map(|v|&**v),
                        got_value.as_ref().map(|v|&**v)
                    );
                }

                //FIXME on CI this test can leak secrets if it fails
                #[test]
                fn setting_inherit_does_not_affect_anything_if_we_anyway_inherit_all(
                    cmd in any::<OsString>(),
                    pointless_inherit in proptest::sample::select(env::vars_os().map(|(k,_v)| k).collect::<Vec<_>>()),
                ) {
                    const NON_EXISTING_VAR_KEY: &'static str = "____CHECKED_COMMAND__THIS_SHOULD_NOT_EXIST_AS_ENV_VARIABLE____";
                    assert_eq!(env::var_os(NON_EXISTING_VAR_KEY), None);

                    let expected_values = env::vars_os()
                        .map(|(k,v)| (Cow::Owned(k), Cow::Owned(v)))
                        .collect::<HashMap<_,_>>();

                    let cmd = Command::new(cmd, ReturnNothing)
                        .with_env_update(&pointless_inherit, EnvChange::Inherit)
                        .with_env_update(NON_EXISTING_VAR_KEY, EnvChange::Inherit);

                    let values = cmd.create_expected_env_iter().collect::<HashMap<_,_>>();

                    assert!(!values.contains_key(OsStr::new(NON_EXISTING_VAR_KEY)));
                    assert_eq!(expected_values.len(), values.len());
                    assert_eq!(
                        expected_values.get(&pointless_inherit),
                        values.get(&*pointless_inherit)
                    );

                }
            }
        }

        mod working_directory {
            use super::super::super::*;
            use crate::utils::opt_arbitrary_path_buf;
            use proptest::prelude::*;

            #[test]
            fn by_default_no_explicit_working_directory_is_set() {
                let cmd = Command::new("foo", ReturnNothing);
                assert_eq!(cmd.working_directory_override(), None);
            }

            //FIXME proptest
            #[test]
            fn replacing_the_working_dir_override() {
                let cmd = Command::new("foo", ReturnNothing)
                    .with_working_directory_override(Some("/foo/bar"));

                assert_eq!(
                    cmd.working_directory_override(),
                    Some(Path::new("/foo/bar"))
                );

                let cmd = cmd.with_working_directory_override(Some(Path::new("/bar/foot")));
                assert_eq!(
                    cmd.working_directory_override(),
                    Some(Path::new("/bar/foot"))
                );
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

                    assert_eq!(cmd.working_directory_override(), wd_override.as_ref().map(|i|&**i));

                    let cmd = cmd.with_working_directory_override(wd_override2.as_ref());
                    assert_eq!(cmd.working_directory_override(), wd_override2.as_ref().map(|i|&**i));
                }
            }
        }

        mod exit_code_checking {
            use super::super::super::*;
            use proptest::prelude::*;

            #[test]
            fn by_default_the_expected_exit_code_is_0() {
                let cmd = Command::new("foo", ReturnNothing);
                assert_eq!(cmd.expected_exit_code(), 0);
            }

            #[test]
            fn by_default_exit_code_checking_is_enabled() {
                let cmd = Command::new("foo", ReturnNothing);
                assert_eq!(cmd.check_exit_code(), true);
            }

            #[test]
            fn setting_check_exit_code_to_false_disables_it() {
                Command::new("foo", ReturnNothing)
                    .with_check_exit_code(false)
                    .with_exec_replacement_callback(|_, _| {
                        Ok(ExecResult {
                            exit_code: 1.into(),
                            ..Default::default()
                        })
                    })
                    .run()
                    .unwrap();
            }

            #[test]
            fn you_can_expect_no_exit_code_to_be_returned() {
                let cmd = Command::new("foo", ReturnNothing)
                    .with_expected_exit_code(ExitCode::ProcessTerminatedBeforeExiting);

                assert_eq!(
                    cmd.expected_exit_code(),
                    ExitCode::ProcessTerminatedBeforeExiting
                );
            }

            #[test]
            fn setting_the_expected_exit_code_will_enable_checking() {
                let cmd = Command::new("foo", ReturnNothing)
                    .with_check_exit_code(false)
                    .with_expected_exit_code(0);

                assert_eq!(cmd.check_exit_code(), true);
            }

            proptest! {
                #[test]
                fn return_an_error_if_the_command_has_non_zero_exit_status(
                    cmd in any::<OsString>(),
                    exit_code in prop_oneof!(..0, 1..).prop_map(ExitCode::from)
                ) {
                    let res = Command::new(cmd, ReturnNothing)
                        .with_exec_replacement_callback(move |_,_| {
                            Ok(ExecResult {
                                exit_code,
                                ..Default::default()
                            })
                        })
                        .run();

                    res.unwrap_err();
                }

                #[test]
                fn replacing_the_expected_exit_code_causes_error_on_different_exit_codes(
                    exit_code in -5..6,
                    offset in prop_oneof!(-100..0, 1..101)
                ) {
                    let res = Command::new("foo", ReturnNothing)
                        .with_expected_exit_code(exit_code)
                        .with_exec_replacement_callback(move |cmd,_| {
                            assert_eq!(cmd.expected_exit_code(), exit_code);
                            Ok(ExecResult {
                                exit_code: ExitCode::from(exit_code + offset),
                                ..Default::default()
                            })
                        })
                        .run();

                    match res {
                        Err(CommandExecutionError::UnexpectedExitCode {got, expected}) => {
                            assert_eq!(expected, exit_code);
                            assert_eq!(got, exit_code+offset);
                        },
                        _ => panic!("Unexpected Result: {:?}", res)
                    }
                }

                #[test]
                fn exit_code_checking_can_be_disabled_and_enabled(
                    change1 in proptest::bool::ANY,
                    change2 in proptest::bool::ANY,
                ) {
                    let cmd = Command::new("foo", ReturnNothing)
                        .with_check_exit_code(change1);

                    assert_eq!(cmd.check_exit_code(), change1);

                    let cmd = cmd.with_check_exit_code(change2);
                    assert_eq!(cmd.check_exit_code(), change2);
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
                let cmd = Command::new("some_cmd", ReturnStdoutAndErr)
                    .with_exec_replacement_callback(move |for_cmd, _| {
                        *(*was_run_).borrow_mut() = true;
                        assert_eq!(&*for_cmd.program(), "some_cmd");
                        Ok(ExecResult {
                            exit_code: 0.into(),
                            stdout: Some("result=12".to_owned().into()),
                            stderr: Some(Vec::new()),
                        })
                    });

                let res = cmd.run().unwrap();
                assert_eq!(*was_run.borrow_mut(), true);
                assert_eq!(&*res.stdout, "result=12".as_bytes());
                assert_eq!(&*res.stderr, "".as_bytes());
            }
        }
    }
}
