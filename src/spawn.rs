//! Types related to how spawning a process is implemented.
//!
use std::{ffi::OsString, fmt::Debug, io, path::PathBuf};

use crate::{
    env::EnvBuilder,
    pipe::{PipeSetup, ProcessInput, ProcessOutput},
    ExecResult,
};

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
    pub env_builder: EnvBuilder,

    /// If `Some` the given path will be used as cwd for the new process.
    pub working_directory_override: Option<PathBuf>,

    /// Allows setting how the stdout pipe will be setup IFF it's not specified by the OutputMapping.
    ///
    /// **Warning: If [`OutputMapping::needs_captured_stdout()`] is true this field will be ignored**
    /// (in the default [`Spawner`]).
    ///
    /// If a custom `Piped` setting is setup (and it's not ignored, see above) and the pipe is not
    /// taken out between a spawn and the following wait then it's [`Spawner`] specific what will
    /// happen when wait is called. The default implementation will drop the pipe closing it in
    /// effect.
    pub custom_stdout_setup: Option<PipeSetup>,

    /// Same as [`SpawnOptions::use_stdout_setup`] but for stderr.
    pub custom_stderr_setup: Option<PipeSetup>,

    /// Allows setting how the stdin pipe will be setup.
    ///
    /// If `Some` given `Stdio` setting will be used.
    ///
    /// If `None` it implies the rust std default should be used.
    ///
    pub custom_stdin_setup: Option<PipeSetup>,
}

impl SpawnOptions {
    /// Create a new `SpawnOptions` instance.
    pub fn new(program: OsString) -> Self {
        Self {
            arguments: Vec::new(),
            env_builder: Default::default(),
            working_directory_override: None,
            custom_stdin_setup: None,
            program,
            custom_stdout_setup: None,
            custom_stderr_setup: None,
        }
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
/// The main reason a `dyn Spawner` is used is to enable better testing through mocking
/// subprocess calls.
///
pub trait Spawner: Send + Sync {
    /// Spawns a new sub-process based on given spawn options.
    ///
    /// In difference to [`Command`] this doesn't do any output mapping,
    /// capturing or similar. This is the mechanism internally used to
    /// spawn a child process on whose pipes further operations will
    /// be done.
    ///
    /// As you are expected to reuse a `Spawner` instance this uses `&`.
    fn spawn(
        &self,
        options: SpawnOptions,
        capture_stdout: bool,
        capture_stderr: bool,
    ) -> Result<Box<dyn ChildHandle>, io::Error>;
}

/// Abstraction over an handle to a child process whose termination can be wait for.
pub trait ChildHandle: Send {
    /// Takes out the stdout pipe, if there is a "unused" pipe.
    ///
    /// - This will return `None` if no pipe was setup.
    /// - This will also return `None` if the `OutputMapping` will capture
    ///   the output.
    /// - So this will only return `Some` if the `OutputMapping` doesn't cause
    ///   stdout to be captured *and* a pipe was manually setup.
    ///
    /// A `ChildHandle` implementation must make sure that the stdout pipe setup
    /// for the `OutputMapping` capturing can not be "stolen".
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
}
