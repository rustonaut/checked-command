use std::{fmt::Debug, io};

use crate::{spawn::SpawnOptions, ExecResult};

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
//TODO update doc
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
    ) -> Result<Box<dyn ChildMock>, io::Error>;
}

//TODO update doc
/// Abstraction over an handle to a child process whose termination can be wait for.
pub trait ChildMock: Send {
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
    fn take_stdout(&mut self) -> Option<Box<dyn ProcessOutputMock>>;

    /// See [`ChildHandle::take_stdout()`]. DO read the documentation.
    fn take_stderr(&mut self) -> Option<Box<dyn ProcessOutputMock>>;

    /// Takes out the stdin pipe, if it is setup as pipe and was not taken out before.
    fn take_stdin(&mut self) -> Option<Box<dyn ProcessInputMock>>;

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

/// Abstraction over [`std::process::ChildStdout`] and [`std::process::ChildStderr`].
///
/// In difference to std's `ChildStdout`/`ChildStderr` this can be mocked in a non
/// platform-specific way.
///
/// Note that while this does implement `io::Write` on `&mut self`
/// in difference to std's implementations this doesn't implement
/// `io::Read` on `&self`.
//TODO update doc
pub trait ProcessOutputMock: Send + io::Read + Debug + RawPipeRepr {}

/// Abstraction over [`std::process::ChildStdin`]
///
/// In difference to std's `ChildStdin` this can be mocked in a non platform-specific way.
///
/// Note that while this does implement `io::Write` on `&mut self`
/// in difference to std's implementations this doesn't implement
/// `io::Write` on `&self`.
//TODO update doc
pub trait ProcessInputMock: Send + io::Write + Debug + RawPipeRepr {}

/// Trait enabling `AsRawFd`/`IntoRawFd`/`AsRawHandle`/`IntoRawHandle` on [`ProcessOutput`]/[`ProcessInput`]
///
/// # Panic
///
/// The default implementation will panic if the the raw fd is accessed, this makes sense as
/// *this is meant for mocking not alternative spawn implementations*.
///
/// Naturally the implementation using the default actual spawning functionality does properly
/// implement this methods without panicking.
///
/// Mock components which implement [`ProcessInputMock`]/[`ProcessOutputMock`] can just decide to
/// ignore this as long as no functionality is tested which uses the `RawFd`/`RawHandle`.
pub trait RawPipeRepr {
    #[cfg(unix)]
    fn as_raw_fd(&self) -> std::os::unix::prelude::RawFd {
        panic!("Mock isn't backed by RawFd.")
    }
    #[cfg(unix)]
    fn into_raw_fd(self: Box<Self>) -> std::os::unix::prelude::RawFd {
        panic!("Mock isn't backed by RawFd.")
    }

    //TODO test this
    #[cfg(windows)]
    fn as_raw_handle(&self) -> std::os::windows::io::RawHandle {
        panic!("Mock isn't backed by RawHandle.")
    }
    #[cfg(windows)]
    fn into_raw_handle(self: Box<Self>) -> std::os::windows::io::RawHandle {
        panic!("Mock isn't backed by RawHandle.")
    }
}
