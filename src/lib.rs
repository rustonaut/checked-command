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
//!         .with_exec_replacement_callback(|_cmd, _rs| {
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
//!         .with_exec_replacement_callback(|_cmd, _rs| {
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

pub use self::return_settings::*;

#[macro_use]
mod utils;
mod return_settings;
mod sys;

/// A alternative to `std::process::Command` see module level documentation.
pub struct Command<Output, Error>
where
    Output: 'static,
    Error: From<CommandExecutionError> + 'static,
{
    program: OsString,
    arguments: Vec<OsString>,
    env_updates: HashMap<OsString, EnvChange>,
    working_directory_override: Option<PathBuf>,
    expected_exit_status: ExitStatus,
    check_exit_status: bool,
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
            check_exit_status: true,
            inherit_env: true,
            expected_exit_status: ExitStatus::Code(0),
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
    /// See [`Command::with_env_updates()`].
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
    /// See [`Command::inherit_env()`] for how this affects the sub-process env.
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

    /// Return which exit status is treated as success.
    pub fn expected_exit_status(&self) -> ExitStatus {
        self.expected_exit_status
    }

    /// Set which exit status is treated as successful.
    ///
    /// **This enables exit status checking even if it
    ///   was turned of before.**
    pub fn with_expected_exit_status(self, exit_status: impl Into<ExitStatus>) -> Self {
        let mut cmd = self.with_check_exit_status(true);
        cmd.expected_exit_status = exit_status.into();
        cmd
    }

    /// Returns true if the exit status is checked before mapping the output(s).
    pub fn check_exit_status(&self) -> bool {
        self.check_exit_status
    }

    /// Sets if the exit status is checked before mapping the output(s).
    pub fn with_check_exit_status(mut self, val: bool) -> Self {
        self.check_exit_status = val;
        self
    }

    /// Returns true if stdout will be captured.
    ///
    /// # Panics
    ///
    /// **If called in a `exec_replacement_callback` this will panic.**
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
    /// **If called in a `exec_replacement_callback` this will panic.**
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
    /// **This will panic if called in a `exec_replacement_callback`.**
    pub fn run(mut self) -> Result<Output, Error> {
        let expected_exit_status = self.expected_exit_status;
        let check_exit_status = self.check_exit_status;
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

        if check_exit_status && result.exit_status != expected_exit_status {
            Err(Error::from(CommandExecutionError::UnexpectedExitStatus {
                got: result.exit_status,
                expected: expected_exit_status,
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
            let exit_status = result.exit_status;
            return_settings.map_output(stdout, stderr, exit_status)
        }
    }

    /// Sets a callback which is called instead of executing the command when running the command.
    ///
    /// This is mainly meant to be used for mocking command execution during testing, but can be used for
    /// other thinks, too. E.g. the current implementation does have a default callback for normally executing
    /// the command this method was not called.
    ///
    ///
    /// # Implementing Mocks with an exec_replacement_callback
    ///
    /// You must not call following methods in the callback:
    ///
    /// - [`Command::run()`], recursively calling run will not work.
    /// - [`Command::will_capture_stdout()`], use the passed in return settings [`ReturnSettings::capture_stdout()`] method instead.
    /// - [`Command::will_capture_stderr()`], use the passed in return settings [`ReturnSettings::capture_stderr()`] method instead.
    ///
    /// An emulation of captured output and exit status is returned as [`ExecResult`] instance:
    ///
    /// - Any exit code can be returned including a target specific one,
    ///   the `From<num> for ExitStatus` implementations are useful here.
    /// - If  [`ReturnSettings::capture_stdout()`] is `true` then [`ExecResult::stdout`] must be `Some`
    ///   else it must be `None`. Failing to do so will panic on unwrap of debug assertions.
    /// - If  [`ReturnSettings::capture_stdout()`] is `true` then [`ExecResult::stdout`] must be `Some`
    ///   else it must be `None`. Failing to do so will panic on unwrap of debug assertions.
    ///
    /// If used for mocking in tests you already know if stdout/stderr is assumed to (not) be
    /// captured so you do not need to access [`ReturnSettings::capture_stdout()`]/[`ReturnSettings::capture_stdout()`].
    ///
    /// Settings like env updates and inheritance can be retrieved from the passed in `Command` instance.
    ///
    /// # Implement custom subprocess spawning
    ///
    /// *Be aware that if you execute the program in the callback you need to make sure the right program, arguments
    /// stdout/stderr capture setting and env variables are used. Especially note should be taken to how `EnvChange::Inherit`
    /// is handled.*
    ///
    /// The [`Command::create_expected_env_iter()`] method can be used to find what exact env variables
    /// are expected to be in the sub-process. Clearing the sub-process env and then setting all env vars
    /// using [`Command::create_expected_env_iter()`] is not the most efficient but most simple and robust
    /// to changes way to set the env. It's recommended to be used.
    ///
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
    /// can still decide to fail due to an unexpected/bad exit status.
    fn map_output(
        self: Box<Self>,
        stdout: Option<Vec<u8>>,
        stderr: Option<Vec<u8>>,
        exit_status: ExitStatus,
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

    /// The process exited with an unexpected exit status.
    ///
    /// By default this means the exit status was not 0, but
    /// this can be changed.
    #[error("Unexpected exit status. Got: {got}, Expected: {expected}")]
    UnexpectedExitStatus {
        got: ExitStatus,
        expected: ExitStatus,
    },
}

/// A ExitStatus type similar to `std::process::ExitStatus` but which can be created (e.g. for testing).
///
/// # Display
///
/// If there is an exit code it will always be displayed as hexadecimal.
/// This is done because of two reasons:
///
/// - Some platforms allow rather large exit codes which are just very unreadable (and unrecognizable) in decimal formatting.
/// - The hex format is always bit based which removes confusions around differences between targets having signed and unsigned
///   exit codes. The drawback is that on platforms which do allow negative exit codes you need to convert the number not just
///   from hex to decimal but also consider signing wrt. the max supported unsigned size on that platform.
///
/// An target specific exit status is displayed in a target specific way.
/// The non os specific fallback defaults to displaying `NO_exit_status`.
/// A signal termination exit status on unix will be displayed as e.g.
/// `signal(9)`.
///
///
/// # Os Support
///
/// For now part of this type are only supported for targets of the os families
/// windows and unix(-like).
///
/// If you need support for _any_ other OS feel free to open an issue, I will
/// add the necessary code path for the methods which are not OS independent
/// then.
///
/// Currently this only affects the [`ExitStatus::successful()`] method.
///
/// # Why not `std::process::ExitStatus`?
///
/// The standard library and this library have different design goals, most
/// importantly this library can introduce braking changes while the standard
/// library ones can't really do so.
///
/// Major differences include:
///
/// - Just one enum instead of an `.exit_status() -> Option<i32>` accessor.
/// - Implements `PartialEq<RHS>` for various numbers making testing easier.
/// - Has a platform independent constructor, `std::process::ExitStatus` has
///   various platform specific constructors.
/// - Uses `i64` as exit code to more correctly represents exits codes (see below).
///
/// ## Incompatibilities and limitations.
///
/// Due to the current structures a various exit codes can be constructed which
/// are not possible to appear on the target you are currently compiling against.
/// **This is already true for the std implementation, but with slightly less
/// constraints in our case.** For example if you target linux you can still
/// create a exit code > 0xFF but in linux no exit code > 0xFF can be returned (
/// furthermore returning any exit code > 127 is a cause of unexpected problems
/// and must be avoided).
///
/// Furthermore `std::process::Command` returning a i32 code is a major problem
/// as it's incompatible with various platforms:
///
/// - Windows has a `u32`! exit code, rust std's Command does reinterpret
///   it as `i32` when returning it which in some cases lead to negative
///   exit codes even through there *are not negative exit codes on windows*.
///   Furthermore Fushisa does have a i64 exit status which they currently
///   can't handle at all. *Be aware that this library still uses
///   `std::process::Command` internally and a such can't handle this either*.
///   But we do "fix" the exit code so that an exit code of `u32::MAX` is still
///   `u32::MAX` and not `-1`!.
///
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum ExitStatus {
    /// The process exited with an exit code.
    ///
    /// As this allows any i64 this allows you to create an exit status which can not
    /// appear on the current target. (This is also possible with the standard libraries
    /// `ExitStatus`). This makes testing easier and allows you to test cases of exit
    /// codes which can't appear on your but other platforms.
    ///
    /// # Differences to `std::process::ExitStatus`
    ///
    /// This uses a `i64` as this allows a more correct representation of exit codes.
    ///
    /// *On windows a exit code > `i32::MAX` will be correctly be represented as such
    /// instead of wrongly being displayed as negative number.*
    Code(i64),

    /// An exit status which isn't a simple exit code was returned.
    ///
    /// On unix if a process was directly terminated via an signal no exit code was
    /// set but the signal causing the exit is returned (encoded with other values
    /// in the raw exit status).
    ///
    /// Rust represents this separately as depending on the exact unix-like operating
    /// system it might be encoded in different ways, in some cases instead of a integer
    /// encoding the status a struct with multiple fields is returned, as such there
    /// is no correct or reliable way to encode exit an status just as an number.
    ///
    /// Be aware that for testability [`OpaqueOsExitStatus`] can be created on all platforms
    /// even through e.g. on windows there are only exit codes! Note that the exact inner
    /// implementation of [`OpaqueOsExitStatus`] is platform dependent, but it implements
    /// [`arbitrary_default()`](OpaqueOsExitStatus::target_specific_default())
    OsSpecific(OpaqueOsExitStatus),
}

impl ExitStatus {
    /// Returns true if the command did succeed.
    ///
    /// As not all operating systems use 0 == success we need to have platform
    /// specific code for all of them. Which is infeasible and as such this is
    /// only enabled on the unix and window target family. (Note that windows
    /// and unix are currently the only target families as e.g. linux, all BSD's,
    /// OsX, iOs are unix-like enough to count as part of the unix family).
    #[cfg(any(window, unix))]
    pub fn successful(&self) -> bool {
        match self {
            Self::Code(code) if *code == 0 => true,
            _ => false,
        }
    }
}

impl Display for ExitStatus {
    fn fmt(&self, fter: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Code(code) => write!(fter, "0x{:X}", code),
            Self::OsSpecific(alt) => Display::fmt(alt, fter),
        }
    }
}

impl Default for ExitStatus {
    fn default() -> Self {
        Self::Code(0)
    }
}

impl From<OpaqueOsExitStatus> for ExitStatus {
    fn from(ooes: OpaqueOsExitStatus) -> Self {
        ExitStatus::OsSpecific(ooes)
    }
}

macro_rules! impl_from_and_partial_eq_for_fitting_int {
    ($($int:ty),*) => ($(
        impl From<$int> for ExitStatus {
            fn from(code: $int) -> Self {
                Self::Code(code as _)
            }
        }

        impl PartialEq<$int> for ExitStatus {
            fn eq(&self, other: &$int) -> bool {
                match self {
                    Self::Code(code) => *code == *other as i64,
                    Self::OsSpecific(_) => false,
                }
            }
        }
    )*);
}

impl_from_and_partial_eq_for_fitting_int!(u8, i8, u16, i16, u32, i32, i64);
/// A platform specific opaque exit status.
///
/// An exit status which is not an exit code, e.g.
/// on unix the signal which terminated an process
/// preventing it from exiting with an exit status.
///
/// **Warning: Besides [`OpaqueOsExitStatus::target_specific_default()`]
/// all other methods only exist on _some_ targets but not all.** As such
/// using them can lead to code which only compiles on some targets.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct OpaqueOsExitStatus {
    #[cfg(not(unix))]
    _priv: (),
    #[cfg(unix)]
    signal: i32,
}

impl OpaqueOsExitStatus {
    /// Creates a instance of this type.
    ///
    /// This is meant for allowing non-platform specific tests which
    /// handle the case of a non exit code process exit status.
    ///
    /// Platform specific tests likely still are needed as what
    /// this type means is platform specific.
    ///
    /// This will always create the same default value but it's
    /// a target_specific_value *and* it's picked arbitrary so
    /// it's not really appropriately to implement [`Default`].
    /// (To make clear why it isn't consider `u32` would default
    /// to `246` or similar arbitrary value.)
    pub fn target_specific_default() -> Self {
        Self {
            #[cfg(not(unix))]
            _priv: (),
            #[cfg(unix)]
            signal: 9,
        }
    }

    /// Return the signal number which did lead to the process termination.
    #[cfg(unix)]
    pub fn signal_number(&self) -> i32 {
        self.signal
    }

    /// Create a unix [`OpaqueOsExitStatus`] instance based on the signal code
    /// causing the non exit code termination.
    ///
    /// Like some other aspects you can define (and test) unrealistic signal numbers.
    /// IMHO this is better (more simple, flexible etc.) then to have a result which
    /// is potentially target dependent or a implicit target dependent bit masking.
    ///
    // E.g. on linux and most (all) unix it's limited to 7 bit (&0x7f) but on at least
    // OpenBSD the value 0x7F is reserved and doesn't count as signal (the macro for
    // testing if it exited with an signal excludes it). Also in any case 0 is not a
    // valid signal either.
    //
    // POSIX defines signals as `int` and with this more or less as i32, but this seems to
    // be because of practical reasons i.e. bitmasking a i32 produces a i32. I do not think
    // there are any negative signals at all, nor do there seem to be any platforms with more
    // than a handful of valid signals.
    #[cfg(unix)]
    pub fn from_signal_number(signal: i32) -> Self {
        Self { signal }
    }
}

impl Display for OpaqueOsExitStatus {
    fn fmt(&self, fter: &mut fmt::Formatter) -> fmt::Result {
        #[cfg(not(unix))]
        {
            fter.write_str("NO_EXIT_CODE")
        }
        #[cfg(unix)]
        {
            write!(fter, "signal({})", self.signal)
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

    struct TestReturnSettings {
        capture_stdout: bool,
        capture_stderr: bool,
    }

    impl ReturnSettings for TestReturnSettings {
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
            _exit_status: super::ExitStatus,
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
                            exit_status: 0.into(),
                            ..Default::default()
                        })
                    })
                    .run();

                res.unwrap();
            }
        }

        mod ReturnSetting {
            use super::super::super::*;
            use super::super::TestReturnSettings;
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
                        _exit_status: ExitStatus,
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
                            exit_status: 0.into(),
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
                        _exit_status: ExitStatus,
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
                    .with_check_exit_status(false)
                    .with_exec_replacement_callback(|_, _| {
                        Ok(ExecResult {
                            exit_status: 1.into(),
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
                    .with_check_exit_status(false)
                    .with_exec_replacement_callback(|_, _| {
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
                    let res = Command::new("foo", TestReturnSettings { capture_stdout, capture_stderr })
                        .with_exec_replacement_callback(move |_,_| {
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
                fn command_provides_a_getter_to_check_if_stdout_and_err_will_be_captured(
                    capture_stdout in proptest::bool::ANY,
                    capture_stderr in proptest::bool::ANY
                ) {
                    let cmd = Command::new("foo", TestReturnSettings { capture_stdout, capture_stderr });
                    prop_assert_eq!(cmd.will_capture_stdout(), capture_stdout);
                    prop_assert_eq!(cmd.will_capture_stderr(), capture_stderr);
                }

                #[test]
                fn capture_hints_are_available_in_the_callback(
                    capture_stdout in proptest::bool::ANY,
                    capture_stderr in proptest::bool::ANY
                ) {
                    Command::new("foo", TestReturnSettings { capture_stdout, capture_stderr })
                        .with_exec_replacement_callback(move |_cmd, return_settings| {
                            assert_eq!(return_settings.capture_stdout(), capture_stdout);
                            assert_eq!(return_settings.capture_stderr(), capture_stderr);
                            Ok(ExecResult {
                                exit_status: 0.into(),
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
                    const NON_EXISTING_VAR_KEY: &'static str = "____MAPPED_COMMAND__THIS_SHOULD_NOT_EXIST_AS_ENV_VARIABLE____";
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

        mod exit_status_checking {
            use super::super::super::*;
            use proptest::prelude::*;

            #[test]
            fn by_default_the_expected_exit_status_is_0() {
                let cmd = Command::new("foo", ReturnNothing);
                assert_eq!(cmd.expected_exit_status(), 0);
            }

            #[test]
            fn by_default_exit_status_checking_is_enabled() {
                let cmd = Command::new("foo", ReturnNothing);
                assert_eq!(cmd.check_exit_status(), true);
            }

            #[test]
            fn setting_check_exit_status_to_false_disables_it() {
                Command::new("foo", ReturnNothing)
                    .with_check_exit_status(false)
                    .with_exec_replacement_callback(|_, _| {
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
                    cmd.expected_exit_status(),
                    ExitStatus::OsSpecific(OpaqueOsExitStatus::target_specific_default())
                );
            }

            #[test]
            fn setting_the_expected_exit_status_will_enable_checking() {
                let cmd = Command::new("foo", ReturnNothing)
                    .with_check_exit_status(false)
                    .with_expected_exit_status(0);

                assert_eq!(cmd.check_exit_status(), true);
            }

            proptest! {
                #[test]
                fn return_an_error_if_the_command_has_non_zero_exit_status(
                    cmd in any::<OsString>(),
                    exit_status in prop_oneof!(..0, 1..).prop_map(ExitStatus::from)
                ) {
                    let res = Command::new(cmd, ReturnNothing)
                        .with_exec_replacement_callback(move |_,_| {
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
                        .with_exec_replacement_callback(move |cmd,_| {
                            assert_eq!(cmd.expected_exit_status(), exit_status);
                            Ok(ExecResult {
                                exit_status: ExitStatus::from(exit_status + offset),
                                ..Default::default()
                            })
                        })
                        .run();

                    match res {
                        Err(CommandExecutionError::UnexpectedExitStatus {got, expected}) => {
                            assert_eq!(expected, exit_status);
                            assert_eq!(got, exit_status+offset);
                        },
                        _ => panic!("Unexpected Result: {:?}", res)
                    }
                }

                #[test]
                fn exit_status_checking_can_be_disabled_and_enabled(
                    change1 in proptest::bool::ANY,
                    change2 in proptest::bool::ANY,
                ) {
                    let cmd = Command::new("foo", ReturnNothing)
                        .with_check_exit_status(change1);

                    assert_eq!(cmd.check_exit_status(), change1);

                    let cmd = cmd.with_check_exit_status(change2);
                    assert_eq!(cmd.check_exit_status(), change2);
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
                            exit_status: 0.into(),
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

    mod ExitStatus {
        #![allow(non_snake_case)]

        mod display_fmt {
            use crate::{ExitStatus, OpaqueOsExitStatus};

            #[test]
            fn format_exit_status_as_hex() {
                let exit_status = ExitStatus::from(0x7Fi32);
                assert_eq!(&format!("{}", exit_status), "0x7F");
            }

            #[test]
            fn format_negative_exit_status_as_hex() {
                let exit_status = ExitStatus::Code(-1i32 as u32 as _);
                assert_eq!(&format!("{}", exit_status), "0xFFFFFFFF");
            }

            #[test]
            #[cfg(unix)]
            fn display_for_non_exit_code_on_unix() {
                let signal = OpaqueOsExitStatus::from_signal_number(9);
                assert_eq!(&format!("{}", signal), "signal(9)");
            }
        }

        mod new {
            use crate::ExitStatus;

            #[test]
            fn can_be_create_from_many_numbers() {
                let status = ExitStatus::from(12u8);
                assert_eq!(status, ExitStatus::Code(12));
                let status = ExitStatus::from(-12i8);
                assert_eq!(status, ExitStatus::Code(-12));
                let status = ExitStatus::from(12u16);
                assert_eq!(status, ExitStatus::Code(12));
                let status = ExitStatus::from(-12i16);
                assert_eq!(status, ExitStatus::Code(-12));
                let status = ExitStatus::from(u32::MAX);
                assert_eq!(status, ExitStatus::Code(u32::MAX as i64));
                let status = ExitStatus::from(-1i32);
                assert_eq!(status, ExitStatus::Code(-1));
                let status = ExitStatus::from(-13i64);
                assert_eq!(status, ExitStatus::Code(-13));
            }

            #[test]
            fn can_compare_to_many_numbers() {
                let status = ExitStatus::from(12u8);
                assert_eq!(status, 12u8);
                let status = ExitStatus::from(-12i8);
                assert_eq!(status, -12i8);
                let status = ExitStatus::from(12u16);
                assert_eq!(status, 12u16);
                let status = ExitStatus::from(-12i16);
                assert_eq!(status, -12i16);
                let status = ExitStatus::from(u32::MAX);
                assert_eq!(status, u32::MAX);
                let status = ExitStatus::from(-1i32);
                assert_eq!(status, -1i32);
                let status = ExitStatus::from(-13i64);
                assert_eq!(status, -13i64);
            }
        }
    }

    #[cfg(unix)]
    mod signal_number {
        use proptest::prelude::*;

        use crate::OpaqueOsExitStatus;

        proptest! {
            #[test]
            fn from_to_signal_number(
                nr in any::<i32>()
            ) {
                let exit_status = OpaqueOsExitStatus::from_signal_number(nr);
                assert_eq!(exit_status.signal_number(), nr);
            }

        }
    }
}
