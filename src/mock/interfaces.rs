use std::{fmt::Debug, io};

use crate::{spawn::SpawnOptions, ExecResult};

/// Trait used to provide spawning functionality if the `mocking` feature is enabled.
///
/// This should only be used for mocking, through technically you could wrap the
/// system spawner functionality adding e.g. spy functionality, which is also
/// part of mocking in the end.
///
/// See the module level documentation.
pub trait Spawner: Send + Sync {
    /// Spawns a new sub-process based on given spawn options.
    ///
    /// In difference to [`Command`] this doesn't do any output mapping,
    /// capturing or similar. This is the mechanism internally used to
    /// spawn a child process on whose pipes further operations will
    /// be done.
    ///
    /// As you might reuse a `Spawner` instance this uses `&`.
    fn spawn(
        &self,
        options: SpawnOptions,
        capture_stdout: bool,
        capture_stderr: bool,
    ) -> Result<Box<dyn ChildMock>, io::Error>;
}

/// Mock abstraction trait over an handle to a child process whose termination can be wait for.
///
/// If `mocking` is enabled this as `Box<dyn ChildMock>` is used internally by the [`crate::Child`]
/// type (if disabled this trait isn't even compiled in).
///
/// For non-mocked functionality this wraps [`std::process::Child`], for mocks this can be
/// an arbitrary implementation.
pub trait ChildMock: Send {
    /// Impl. of [`Child::take_stdout()`].
    ///
    /// A `ChildHandle` implementation must make sure that the stdout pipe setup
    /// for the `OutputMapping` capturing can not be "stolen". As this might
    /// lead to unexpected test results.
    ///
    fn take_stdout(&mut self) -> Option<Box<dyn ProcessOutputMock>>;

    /// See [`ChildHandle::take_stdout()`]. DO read the documentation.
    fn take_stderr(&mut self) -> Option<Box<dyn ProcessOutputMock>>;

    /// Takes out the stdin pipe, if it is setup as pipe and was not taken out before.
    fn take_stdin(&mut self) -> Option<Box<dyn ProcessInputMock>>;

    /// Waits on the child capturing the output.
    ///
    /// This is internally used when [`Child::wait_with_output()`] is called
    /// while the `mocking` feature is enabled.
    fn wait_with_output(self: Box<Self>) -> Result<ExecResult, io::Error>;
}

/// Mock abstraction over [`std::process::ChildStdout`] and [`std::process::ChildStderr`].
///
/// In difference to std's `ChildStdin` this can be mocked in a non platform-specific way.
///
/// But if you want to use [`InputPipeSetup::ExistingPipe`] or [`OutputPipeSetup::ExistingPipe`]
/// the pipes in question need to be backed by a `RawFd`/`RawHandle` and implement `RawPipeRepr`
/// properly.
///
/// Note that while this does implement `io::Write` on `&mut self`
/// in difference to std's implementations this doesn't implement
/// `io::Read` on `&self`.
///
pub trait ProcessOutputMock: Send + io::Read + Debug + RawPipeRepr {}

/// Mock abstraction over [`std::process::ChildStdin`]
///
/// In difference to std's `ChildStdin` this can be mocked in a non platform-specific way.
///
/// But if you want to use [`InputPipeSetup::ExistingPipe`] or [`OutputPipeSetup::ExistingPipe`]
/// the pipes in question need to be backed by a `RawFd`/`RawHandle` and implement `RawPipeRepr`
/// properly.
///
/// Note that while this does implement `io::Write` on `&mut self`
/// in difference to std's implementations this doesn't implement
/// `io::Write` on `&self`.
///
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
///
/// # Unsafe
///
/// If you implement methods of `RawPipeRepr` you must make sure that returned file descriptors
/// are valid in a context where you could use them for things like `Stdio::from_raw_fd()`.
///
/// Which mainly means they need to be a valid file descriptor on unix and handle on windows.
///
/// **It's always safe to use the default method implementations of this trait.**
///
/// Sadly specialization doesn't exists for now in rust so we can't "wild card" implement
/// it so that mocks which don't use `RawFd`/`RawHandle` don't need a unsafe impl.
pub unsafe trait RawPipeRepr {
    #[cfg(unix)]
    fn as_raw_fd(&self) -> std::os::unix::prelude::RawFd {
        panic!("Mock isn't backed by RawFd.")
    }
    #[cfg(unix)]
    fn into_raw_fd(self: Box<Self>) -> std::os::unix::prelude::RawFd {
        panic!("Mock isn't backed by RawFd.")
    }

    #[cfg(windows)]
    fn as_raw_handle(&self) -> std::os::windows::io::RawHandle {
        panic!("Mock isn't backed by RawHandle.")
    }
    #[cfg(windows)]
    fn into_raw_handle(self: Box<Self>) -> std::os::windows::io::RawHandle {
        panic!("Mock isn't backed by RawHandle.")
    }
}
