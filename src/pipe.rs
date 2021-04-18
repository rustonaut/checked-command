//! Types related to the setup of stdout, stderr and stdin pipes.
use std::{
    any::Any,
    fmt::Debug,
    fs::File,
    io,
    process::{ChildStderr, ChildStdin, ChildStdout},
};
use thiserror::Error;

/// Indicates that a child processes pipe should be setup "piped".
///
/// This can be converted to [`InputPipeSetup`] and [`OutputPipeSetup`],
/// so that you can pass it to [`Command::with_custom_stdin_setup()`],
/// [`Command::with_custom_stdout_setup()`] and [`Command::with_custom_stderr_setup()`].
///
/// Normally this will default to have the same affect `Stdio::piped()`,
/// and like any call to `with_custom_std*_setup()` it might be ignored
/// depending on the used [`OutputMapping`] instance.
///
#[derive(Debug, Clone, Copy)]
pub struct Piped;
/// Indicates that a child processes pipe should be setup to pipe to "null".
///
/// This can be converted to [`InputPipeSetup`] and [`OutputPipeSetup`],
/// so that you can pass it to [`Command::with_custom_stdin_setup()`],
/// [`Command::with_custom_stdout_setup()`] and [`Command::with_custom_stderr_setup()`].
///
/// Normally this will default to have the same affect `Stdio::null()`,
/// and like any call to `with_custom_std*_setup()` it might be ignored
/// depending on the used [`OutputMapping`] instance.
///
#[derive(Debug, Clone, Copy)]
pub struct Null;

/// Indicates that a child processes pipe should be inherited from the parent.
///
/// This can be converted to [`InputPipeSetup`] and [`OutputPipeSetup`],
/// so that you can pass it to [`Command::with_custom_stdin_setup()`],
/// [`Command::with_custom_stdout_setup()`] and [`Command::with_custom_stderr_setup()`].
///
/// Normally this will default to have the same affect `Stdio::inherit()`,
/// and like any call to `with_custom_std*_setup()` it might be ignored
/// depending on the used [`OutputMapping`] instance.
///
#[derive(Debug, Clone, Copy)]
pub struct Inherit;
/// Specifies how a child process input pipe (stdin) will be setup.
///
/// This is similar to `Stdio` but less opaque which does make
/// some parts around testing, mocking, tracing and similar easier.
///
/// Normally you don't need to directly create this, instead you
/// pass values which can be converted into it to
/// [`Command::with_custom_stdin_setup()`]. Like e.g. the [`Piped`]
/// [`Null`] or [`Inherit`] utility types.
///
#[derive(Debug)]
pub enum InputPipeSetup {
    ExistingPipe(Box<dyn ProcessOutput>),
    File(File),
    ChildStdout(ChildStdout),
    ChildStderr(ChildStderr),
    Piped,
    Null,
    Inherit,
}

/// Specifies how a child process output pipe (stdout/stderr) will be setup.
///
/// This is similar to `Stdio` but less opaque which does make
/// some parts around testing, mocking, tracing and similar easier.
///
/// Normally you don't need to directly create this, instead you
/// pass values which can be converted into it to
/// [`Command::with_custom_stdin_setup()`]. Like e.g. the [`Piped`]
/// [`Null`] or [`Inherit`] utility types.
///
#[derive(Debug)]
pub enum OutputPipeSetup {
    ExistingPipe(Box<dyn ProcessInput>),
    File(File),
    ChildStdin(ChildStdin),
    Piped,
    Null,
    Inherit,
}

macro_rules! impl_from_for_pipe_setup {
    ($({$name:ident, $trait:ident}),*) => ($(
        impl From<Piped> for $name {
            fn from(_: Piped) -> Self {
                Self::Piped
            }
        }
        impl From<Inherit> for $name {
            fn from(_: Inherit) -> Self {
                Self::Inherit
            }
        }
        impl From<Null> for $name {
            fn from(_: Null) -> Self {
                Self::Null
            }
        }
        impl From<File> for $name {
            fn from(f: File) -> Self {
                Self::File(f)
            }
        }
        impl From<Box<dyn $trait>> for $name {
            fn from(ep: Box<dyn $trait>) -> Self {
                Self::ExistingPipe(ep)
            }
        }
    )*);
}

impl_from_for_pipe_setup! {
    // you can pipe and output pipe (ProcessOutput) to an input (new stdin)
    {InputPipeSetup, ProcessOutput},
    {OutputPipeSetup, ProcessInput}
}

impl From<ChildStdout> for InputPipeSetup {
    fn from(out: ChildStdout) -> Self {
        Self::ChildStdout(out)
    }
}

impl From<ChildStderr> for InputPipeSetup {
    fn from(err: ChildStderr) -> Self {
        Self::ChildStderr(err)
    }
}

impl From<ChildStdin> for OutputPipeSetup {
    fn from(inp: ChildStdin) -> Self {
        Self::ChildStdin(inp)
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
