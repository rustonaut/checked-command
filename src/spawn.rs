use std::{
    borrow::Cow,
    collections::HashMap,
    ffi::{OsStr, OsString},
    fmt::Debug,
    io,
    path::PathBuf,
};
use thiserror::Error;

use crate::{EnvChange, ExecResult, ProcessPipeSetting};

//TODO make public nominal type
type EnvMap = HashMap<OsString, EnvChange>;

/// The options used to spawn the sub-process.
///
/// Many getters and `&mut` based setters are provided through
/// dereferencing the [`SpawnOptions`] instance contained in
/// a [`Command`].
///
#[derive(Debug)]
pub struct SpawnOptions {
    /// The program to spawn
    pub program: OsString,

    /// The arguments to pass to the subprocess
    pub arguments: Vec<OsString>,

    /// The way the environment is updated for the subprocess.
    ///
    /// For every key in `env_updates` depending on `EnvChange`
    /// this will update the new processes environment to either
    /// remove the key (if it exists), set the key or make sure
    /// the key is inherited even if inheritance is disabled.
    ///
    /// See [`EnvChange`], [`SpawnOptions::inherit_env`].
    ///
    /// # Warning
    ///
    /// The keys of env variables will *not* be evaluated for syntactic validity,
    /// until actually spawning the subprocess.
    /// Setting a key invalid on given platform *might* cause the process spawning to
    /// fail (e.g. using a key lik `"="` or `""`). It also *might* also do other thinks
    /// like the env variable being passed in but being unaccessible or similar. It's completely
    /// dependent on the OS and the impl. of `std::process::Command` or whatever is used to
    /// execute the command.
    pub env_updates: EnvMap,

    /// Determines if the child inherits the parents environment.
    ///
    /// After inheriting (or creating a empty) environment for
    /// the new process it will be updated based on [`SpawnOptions.env_updates`].
    ///
    /// If inheritance is disabled specific env variables can still
    /// be inherited explicitly using [`EnvChange::Inherit`].
    pub inherit_env: bool,

    /// If `Some` the given path will be used as cwd for the new process.
    pub working_directory_override: Option<PathBuf>,

    /// Allows setting how the stdout pipe will be setup IFF it's not specified by the OutputMapping.
    ///
    /// **Warning: If [`OutputMapping::needs_captured_stdout()`] is true this field will be ignored**
    /// (in the default `SpawnImpl`).
    ///
    /// If `Some` given `Stdio` setting will be used.
    ///
    /// If `None` and [`OutputMapping::needs_captured_stdout()`] is `true` then
    /// it will be set to `Some(Stdio::pipe())` before executing.
    ///
    /// If `None` and [`OutputMapping::needs_captured_stdout()`] is `false` then
    /// it stays `None` which implies the rust std default should be used.
    ///
    /// If set to `Some(Stdio::pipe())` then by default output should be captured
    /// even if [`OutputMapping::needs_captured_stdout()`] is `false`. Non-standard
    /// `ExecImpl` should do so too, but might not. For example using mock `ExecImpl`
    /// might panic on unexpected settings.
    ///
    /// # Panics
    ///
    /// Be aware that if [`OutputMapping::needs_captured_stdout()`] is `true` but
    /// this is set to a `Stdio` which is not `Stdio::pipe()` this will lead to
    /// an panic.
    pub custom_stdout_setup: Option<ProcessPipeSetting>,

    /// Same as [`SpawnOptions::use_stdout_setup`] but for stderr.
    pub custom_stderr_setup: Option<ProcessPipeSetting>,

    /// Allows setting how the stdin pipe will be setup.
    ///
    /// If `Some` given `Stdio` setting will be used.
    ///
    /// If `None` it implies the rust std default should be used.
    ///
    pub stdin_setup: Option<ProcessPipeSetting>,
}

impl SpawnOptions {
    /// Create a new `SpawnOptions` instance.
    pub fn new(program: OsString) -> Self {
        Self {
            arguments: Vec::new(),
            env_updates: HashMap::new(),
            inherit_env: true,
            working_directory_override: None,
            stdin_setup: None,
            program,
            custom_stdout_setup: None,
            custom_stderr_setup: None,
        }
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
        crate::env::create_expected_env_iter(self.inherit_env, &self.env_updates)
    }
}

/// Trait through which implementors providing the functionality to spawn sub-processes.
///
/// Besides the default implementations which uses [`std::process::Command::spawn()`] internally
/// other implementations can be provided for:
///
/// - To mock a subprocess (for testing).
/// - Add special spawn argumentation.
/// - Add tracing/logging.
/// - Whatever you can come up with.
///
/// The main reason a `dyn SpawnImpl` is used is to enable better testing through mocking
/// subprocess calls.
///
pub trait SpawnImpl: 'static {
    /// Spawns a new sub-process based on given spawn options.
    ///
    /// In difference to [`Command`] this doesn't do any output mapping,
    /// capturing or similar. This is the mechanism internally used to
    /// spawn a child process on whose pipes further operations will
    /// be done.
    ///
    /// As you are expected to reuse a `SpawnImpl` instance this uses `&`.
    fn spawn(
        &self,
        options: SpawnOptions,
        capture_stdout: bool,
        capture_stderr: bool,
    ) -> Result<Box<dyn ChildHandle>, io::Error>;
}

/// Abstraction over [`std::process::ChildStdout`] and [`std::process::ChildStderr`].
///
/// In difference to std's `ChildStdout`/`ChildStderr` this can be mocked in a non
/// platform-specific way.
///
/// Note that while this does implement `io::Write` on `&mut self`
/// in difference to std's implementations this doesn't implement
/// `io::Read` on `&self`.
pub trait ProcessOutput: 'static + Send + Sync + io::Read + Debug + RawPipeRepr {
    //TODO cross cast for perf. optimization
}

/// Abstraction over [`std::process::ChildStdin`]
///
/// In difference to std's `ChildStdin` this can be mocked in a non platform-specific way.
///
/// Note that while this does implement `io::Write` on `&mut self`
/// in difference to std's implementations this doesn't implement
/// `io::Write` on `&self`.
pub trait ProcessInput: 'static + Send + Sync + io::Write + Debug + RawPipeRepr {
    //TODO cross cast for perf. optimization
}

/// Abstraction over an handle to a child process whose termination can be wait for.
pub trait ChildHandle: 'static {
    //TODO consider adding SpawnOptions::allow_stdout_extraction which is false if
    //   output mapping requires stdout capturing and will make take_stdout return None,
    //   if true then take_stdout can return Some depending on setup.

    /// Takes out the stdout pipe, if it is setup as pipe and was not taken out before.
    ///
    /// If taken out this prevents [`ChildHandle::wait_with_output()`] from capturing
    /// stdout.
    ///
    /// # Panic
    ///
    /// Incorrect usage of it can lead to panics *in other code*, in some situations where
    /// [`ChildHandle::wait_with_output()`] is expected to capture stdout (e.g. by the
    /// output mapping.).
    ///
    /// Only use this method if you use an output mapping which doesn't capture stdout but
    /// you setup stdout capturing anyway.
    fn take_stdout(&mut self) -> Option<Box<dyn ProcessOutput>>;

    /// See [`ChildHandle::take_stdout()`]. DO read the documentation.
    fn take_stderr(&mut self) -> Option<Box<dyn ProcessOutput>>;

    /// Takes out the stdin pipe, if it is setup as pipe and was not taken out before.
    fn take_stdin(&mut self) -> Option<Box<dyn ProcessInput>>;

    /// Waits on the child capturing the output.
    ///
    /// This will can capture stdout, stderr or both depending on if
    /// stdout/err has been setup as piped with the [`SpawnOptions`] and
    /// if the pipes have been taken out of the child handle before calling
    /// this function.
    ///
    /// # Why?
    ///
    /// Preferably we would just provide `wait` and implement the output
    /// capturing on demand, but as it turns out capturing both stdout
    /// and stderr at the same time requires either using platform specific
    /// non-std exposed functionality on `RawFd`'s or using an additional
    /// thread, both not very nice options.
    fn wait_with_output(self: Box<Self>) -> Result<ExecResult, io::Error>;

    //TODO try_wait?
}

/// Trait providing the os specific `as/into_raw_fd/handle` methods in a faille form.
///
/// The `AsRawFd`/`AsRawHandle` traits can't be implemented as this operations
/// can fail if the given implementations isn't backed by a fd/handle.
pub trait RawPipeRepr {
    #[cfg(unix)]
    fn try_as_raw_fd(&self) -> Result<std::os::unix::prelude::RawFd, NoRawRepr>;
    #[cfg(unix)]
    fn try_into_raw_fd(self: Box<Self>) -> Result<std::os::unix::prelude::RawFd, NoRawRepr>;

    //FIXME due to test limitations feat
    #[cfg(windows)]
    fn try_as_raw_handle(&self) -> Result<std::os::windows::io::RawHandle, NoRawRepr>;
    #[cfg(windows)]
    fn try_into_raw_handle(self: Box<Self>) -> Result<std::os::windows::io::RawHandle, NoRawRepr>;
}

#[derive(Debug, Error)]
#[error("The pipe abstraction isn't backed by any OS pipe.")]
pub struct NoRawRepr;

//TODO reconsider static bounds
