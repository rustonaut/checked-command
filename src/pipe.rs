//! Types related to the setup of stdout, stderr and stdin pipes.
use std::{
    any::Any,
    convert::TryFrom,
    fmt::Debug,
    fs::File,
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
            Redirect(opaque) => opaque.into(),
        }
    }
}

#[cfg(unix)]
use ::std::os::unix::io::{AsRawFd, FromRawFd, IntoRawFd, RawFd};
#[cfg(windows)]
use ::std::os::windows::io::{AsRawHandle, FromRawHandle, IntoRawHandle, RawHandle};

/// Opaque type representing the `ProcessPipeSetting::Redirect` variant.
///
/// Be aware that there is no type-system check preventing you from connecting
/// pipes in non operable ways, e.g. connecting two sub-process stdin's to
/// each other.
///
/// This is basically a wrapper around `RawFd`/`RawHandle` which can be constructed
/// from child pipes, files and `RawFd`/`RawHandle`.
///
/// Besides turning this into `RawFd`/`RawHandle` this can also be turned into `Stdio` for
/// all [`crate::spawn::Spawn`] implementations which use `std::process::Command` internally.
///
/// Be aware that all constructors are only available on `unix` and `windows`.
///
#[derive(Debug)]
pub struct Redirect {
    /// SAFETY: This must be a valid `RawFd` for this usage.
    #[cfg(unix)]
    inner: RawFd,
    /// SAFETY: This must be a valid `RawHandle` for this usage.
    #[cfg(windows)]
    inner: RawHandle,
    #[cfg(not(any(windows, unix)))]
    inner: (),
}

impl TryFrom<Box<dyn ProcessOutput>> for Redirect {
    type Error = NoRawRepr;

    fn try_from(value: Box<dyn ProcessOutput>) -> Result<Self, Self::Error> {
        #[cfg(unix)]
        {
            return Ok(Redirect {
                inner: value.try_into_raw_fd()?,
            });
        }
        #[cfg(windows)]
        {
            return Ok(Redirect {
                inner: value.try_into_raw_handle()?,
            });
        }
        #[cfg(not(any(unix, windows)))]
        {
            return Err(NoRawRepr);
        }
    }
}

impl TryFrom<Box<dyn ProcessInput>> for Redirect {
    type Error = NoRawRepr;

    fn try_from(value: Box<dyn ProcessInput>) -> Result<Self, Self::Error> {
        #[cfg(unix)]
        {
            return Ok(Redirect {
                inner: value.try_into_raw_fd()?,
            });
        }
        #[cfg(windows)]
        {
            return Ok(Redirect {
                inner: value.try_into_raw_handle()?,
            });
        }
        #[cfg(not(any(unix, windows)))]
        {
            return Err(NoRawRepr);
        }
    }
}

impl Into<Stdio> for Redirect {
    fn into(self) -> Stdio {
        // SAFE: Constructing `Redirect` implies this is safe
        // - either this is the inner repr of `File`/`ChildStdout`/`ChildStderr`/`ChildStdin` in
        //   which case this is safe
        // - or it was constructed using a unsafe method in which case the caller implicitly made sure
        //   this is safe
        unsafe {
            #[cfg(unix)]
            return Stdio::from_raw_fd(self.inner);
            #[cfg(windows)]
            return Stdio::from_raw_handle(self.inner);
            // Only constructors for windows/unix exists so this is unreachable
            #[cfg(not(any(windows, unix)))]
            unreachable!()
        }
    }
}

macro_rules! impl_from_child_pipes {
    ($($name:ident),*) => ($(
        #[cfg(unix)]
        impl From<$name> for Redirect {
            fn from(out: $name) -> Self {
                Self {
                    inner: out.into_raw_fd(),
                }
            }
        }

        #[cfg(windows)]
        impl From<$name> for Redirect {
            fn from(out: $name) -> Self {
                Self {
                    inner: out.into_raw_handle(),
                }
            }
        }
    )*);
}

impl_from_child_pipes!(ChildStdout, ChildStderr, ChildStdin);

#[cfg(unix)]
impl FromRawFd for Redirect {
    ///
    /// # Safety
    ///
    /// It must be a valid `RawFd` for this usage, basically it
    /// safe if `Stdio::from_raw_fd()` is safe.
    unsafe fn from_raw_fd(fd: RawFd) -> Self {
        Self { inner: fd }
    }
}

#[cfg(unix)]
impl IntoRawFd for Redirect {
    fn into_raw_fd(self) -> RawFd {
        self.inner
    }
}

#[cfg(unix)]
impl AsRawFd for Redirect {
    fn as_raw_fd(&self) -> RawFd {
        self.inner
    }
}

#[cfg(unix)]
impl From<File> for Redirect {
    fn from(file: File) -> Self {
        Redirect {
            inner: file.into_raw_fd(),
        }
    }
}

#[cfg(windows)]
impl From<File> for Redirect {
    fn from(file: File) -> Self {
        Redirect {
            inner: file.into_raw_handle(),
        }
    }
}

#[cfg(windows)]
impl FromRawHandle for Redirect {
    ///
    /// # Safety
    ///
    /// It must be a valid `RawHandle` for this usage, basically it
    /// safe if `Stdio::from_raw_handle()` is safe.
    unsafe fn from_raw_handle(handle: RawHandle) -> Self {
        Self { inner: handle }
    }
}

#[cfg(windows)]
impl IntoRawHandle for Redirect {
    fn into_raw_handle(self) -> RawHandle {
        self.inner
    }
}

#[cfg(windows)]
impl AsRawHandle for Redirect {
    fn as_raw_handle(&self) -> RawHandle {
        self.inner
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
///
/// By default all method return `Err(NoRawRepr)` but any custom implementation
/// which can return a raw value should do so.
pub trait RawPipeRepr {
    #[cfg(unix)]
    fn try_as_raw_fd(&self) -> Result<std::os::unix::prelude::RawFd, NoRawRepr> {
        Err(NoRawRepr)
    }
    #[cfg(unix)]
    fn try_into_raw_fd(self: Box<Self>) -> Result<std::os::unix::prelude::RawFd, NoRawRepr> {
        Err(NoRawRepr)
    }

    //TODO test this
    #[cfg(windows)]
    fn try_as_raw_handle(&self) -> Result<std::os::windows::io::RawHandle, NoRawRepr> {
        Err(NoRawRepr)
    }
    #[cfg(windows)]
    fn try_into_raw_handle(self: Box<Self>) -> Result<std::os::windows::io::RawHandle, NoRawRepr> {
        Err(NoRawRepr)
    }
}

/// Error returned if there is no raw representation for given pipe.
///
/// In `unix` this means no underlying `RawFd` is accessible.
///
/// On `windows` this means no underlying `RawHandle` is accessible.
#[derive(Debug, Error)]
#[error("The pipe abstraction isn't backed by any OS pipe.")]
pub struct NoRawRepr;
