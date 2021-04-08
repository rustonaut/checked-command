//! Types related to the setup of stdout, stderr and stdin pipes.
use std::{
    any::Any,
    fmt::Debug,
    io,
    process::{ChildStderr, ChildStdin, ChildStdout, Stdio},
};
use thiserror::Error;
/// Specifies how a process pipe (stdout/err/in) will be setup.
///
/// This is similar to `Stdio` but less opaque which does make
/// some parts around testing, mocking, tracing and similar easier.
///
/// Normally stdout/err is implicitly setup base don the [`OutputMapping`]
/// and as such you most times don't need to set this up.
///
/// The main usage is to set pipes to null when output is not captured
/// or in rare cases to redirect to another child.
///
#[derive(Debug)]
pub enum PipeSetup {
    /// Create a pipe to stdout/err/in.
    ///
    /// In case of stdout/err this will lead to output being captured.
    ///
    /// In case of stdin this means that the parent process can write
    /// to the stdin of the child process.
    ///
    /// This corresponds to `Stdio::pipe()`
    Piped,

    /// The child inherits the pipe from the parent.
    ///
    /// In the rust standard library this is the default
    /// setting when spawning a new process.
    ///
    /// This corresponds to `Stdio::inherit()`
    Inherit,

    /// Connect the child stdout/err/in to null (i.e. /dev/null on unix).
    ///
    /// This corresponds to `Stdio::null()`
    Null,

    /// Connects the child's stdout/err/in to another pipe.
    ///
    /// For example connects the stdin of a newly spawned process
    /// to the stdout of a child process spawned just before spawning
    /// this one.
    ///
    Redirect(Redirect),
}

impl Default for PipeSetup {
    fn default() -> Self {
        PipeSetup::Inherit
    }
}

impl From<Redirect> for PipeSetup {
    fn from(redirect: Redirect) -> Self {
        PipeSetup::Redirect(redirect)
    }
}

impl From<ChildStdout> for PipeSetup {
    fn from(out: ChildStdout) -> Self {
        Self::from(Redirect::from(out))
    }
}

impl From<ChildStderr> for PipeSetup {
    fn from(err: ChildStderr) -> Self {
        Self::from(Redirect::from(err))
    }
}

impl From<ChildStdin> for PipeSetup {
    fn from(inp: ChildStdin) -> Self {
        Self::from(Redirect::from(inp))
    }
}

impl From<PipeSetup> for Stdio {
    fn from(pps: PipeSetup) -> Self {
        use self::PipeSetup::*;
        match pps {
            Piped => Stdio::piped(),
            Inherit => Stdio::inherit(),
            Null => Stdio::null(),
            Redirect(opaque) => opaque.inner,
        }
    }
}

/// Opaque type representing the `ProcessPipeSetting::Redirect` variant.
///
#[derive(Debug)]
pub struct Redirect {
    inner: Stdio,
}

impl Redirect {
    //TODO review doc once new post-spawn handling is finalized
    /// Creates a instance from a `Stdio` instance without any checks.
    ///
    /// **Warning: If used with `Stdio` instances which do not do redirects,
    /// especially `Stdio::piped()` this can cause bugs and inconsistent
    /// behavior.**
    ///
    /// The reason for this is that we can't know what kind of variant
    /// `Stdio` internally is. Because of this the used  spawning implementation
    /// might not be able to set things up properly. If the instance uses
    /// the normal child spawning functionality it will likely work, but
    /// no guarantees given.
    ///
    /// It still guarantees that this is safe.
    ///
    /// Still in some cases where you get a `Stdio` from child pipes
    /// or raw fds/handles this cna be a useful method.
    pub fn from_stdio_unchecked(inner: Stdio) -> Self {
        Redirect { inner }
    }
}

impl From<ChildStdout> for Redirect {
    fn from(out: ChildStdout) -> Self {
        Self {
            inner: Stdio::from(out),
        }
    }
}

impl From<ChildStderr> for Redirect {
    fn from(err: ChildStderr) -> Self {
        Self {
            inner: Stdio::from(err),
        }
    }
}

impl From<ChildStdin> for Redirect {
    fn from(inp: ChildStdin) -> Self {
        Self {
            inner: Stdio::from(inp),
        }
    }
}

#[cfg(unix)]
impl ::std::os::unix::io::FromRawFd for Redirect {
    unsafe fn from_raw_fd(fd: ::std::os::unix::io::RawFd) -> Self {
        Self {
            inner: Stdio::from_raw_fd(fd),
        }
    }
}

/// Abstraction over [`std::process::ChildStdout`] and [`std::process::ChildStderr`].
///
/// In difference to std's `ChildStdout`/`ChildStderr` this can be mocked in a non
/// platform-specific way.
///
/// Note that while this does implement `io::Write` on `&mut self`
/// in difference to std's implementations this doesn't implement
/// `io::Read` on `&self`.
pub trait ProcessOutput: Any + Send + io::Read + Debug + RawPipeRepr {
    //TODO cross cast for perf. optimization
}

/// Abstraction over [`std::process::ChildStdin`]
///
/// In difference to std's `ChildStdin` this can be mocked in a non platform-specific way.
///
/// Note that while this does implement `io::Write` on `&mut self`
/// in difference to std's implementations this doesn't implement
/// `io::Write` on `&self`.
pub trait ProcessInput: Any + Send + io::Write + Debug + RawPipeRepr {
    //TODO cross cast for perf. optimization
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

/// Error returned if there is no raw representation for given pipe.
///
/// In `unix` this means no underlying `RawFd` is accessible.
///
/// On `windows` this means no underlying `RawHandle` is accessible.
#[derive(Debug, Error)]
#[error("The pipe abstraction isn't backed by any OS pipe.")]
pub struct NoRawRepr;
