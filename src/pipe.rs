//! Types related to the setup of stdout, stderr and stdin pipes.
use std::{
    fmt::Debug,
    fs::File,
    io,
    process::{ChildStderr, ChildStdin, ChildStdout},
};

#[cfg(unix)]
use std::os::unix::io::{AsRawFd, IntoRawFd, RawFd};
#[cfg(windows)]
use std::os::windows::io::{AsRawHandle, IntoRawHandle, RawHandle};

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
    ExistingPipe(ProcessOutput),
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
    ExistingPipe(ProcessInput),
    File(File),
    ChildStdin(ChildStdin),
    Piped,
    Null,
    Inherit,
}

macro_rules! impl_from_for_pipe_setup {
    ($({$name:ident, $ep:ident}),*) => ($(
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
        impl From<$ep> for $name {
            fn from(ep: $ep) -> Self {
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

#[derive(Debug)] //Todo rest of interface ;=)
pub struct ProcessInput {
    #[cfg(feature = "mocking")]
    inner: Box<dyn crate::mock::ProcessInputMock>,
    #[cfg(not(feature = "mocking"))]
    inner: ChildStdin,
}

impl io::Write for ProcessInput {
    #[inline(always)]
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.inner.write(buf)
    }

    #[inline(always)]
    fn flush(&mut self) -> io::Result<()> {
        self.inner.flush()
    }

    #[inline(always)]
    fn write_vectored(&mut self, bufs: &[io::IoSlice<'_>]) -> io::Result<usize> {
        self.inner.write_vectored(bufs)
    }

    #[inline(always)]
    fn write_all(&mut self, buf: &[u8]) -> io::Result<()> {
        self.inner.write_all(buf)
    }

    #[inline(always)]
    fn write_fmt(&mut self, fmt: std::fmt::Arguments<'_>) -> io::Result<()> {
        self.inner.write_fmt(fmt)
    }
}

impl From<ChildStdin> for ProcessInput {
    fn from(inner: ChildStdin) -> Self {
        #[cfg(feature = "mocking")]
        return ProcessInput {
            inner: Box::new(inner),
        };
        #[cfg(not(feature = "mocking"))]
        return ProcessInput { inner };
    }
}

#[cfg(feature = "mocking")]
impl From<Box<dyn crate::mock::ProcessInputMock>> for ProcessInput {
    fn from(inner: Box<dyn crate::mock::ProcessInputMock>) -> Self {
        ProcessInput { inner }
    }
}

macro_rules! forward {
    ($inner:expr, $fn_name:ident $(, $args:ident)*) => ({
        #[cfg(feature="mocking")]
        return {
            $inner.$fn_name($($args),*)
        };
        #[cfg(not(feature="mocking"))]
        return {
            match $inner {
                ChildStdoutOrErr::Out(inner) => inner.$fn_name($($args),*),
                ChildStdoutOrErr::Err(inner) => inner.$fn_name($($args),*)
            }
        };
    });
}

impl io::Read for ProcessOutput {
    #[inline]
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        forward!(&mut self.inner, read, buf)
    }

    #[inline(always)]
    fn read_vectored(&mut self, bufs: &mut [io::IoSliceMut<'_>]) -> io::Result<usize> {
        forward!(&mut self.inner, read_vectored, bufs)
    }

    #[inline(always)]
    fn read_to_end(&mut self, buf: &mut Vec<u8>) -> io::Result<usize> {
        forward!(&mut self.inner, read_to_end, buf)
    }

    #[inline(always)]
    fn read_to_string(&mut self, buf: &mut String) -> io::Result<usize> {
        forward!(&mut self.inner, read_to_string, buf)
    }

    #[inline(always)]
    fn read_exact(&mut self, buf: &mut [u8]) -> io::Result<()> {
        forward!(&mut self.inner, read_exact, buf)
    }
}

#[derive(Debug)]
pub struct ProcessOutput {
    #[cfg(feature = "mocking")]
    inner: Box<dyn crate::mock::ProcessOutputMock>,
    //Hint: We could optimize using RawFd/RawHandle but unsafe,
    #[cfg(not(feature = "mocking"))]
    inner: ChildStdoutOrErr,
}

impl From<ChildStdout> for ProcessOutput {
    fn from(inner: ChildStdout) -> Self {
        #[cfg(feature = "mocking")]
        return ProcessOutput {
            inner: Box::new(inner),
        };
        #[cfg(not(feature = "mocking"))]
        return ProcessOutput {
            inner: ChildStdoutOrErr::Out(inner),
        };
    }
}

impl From<ChildStderr> for ProcessOutput {
    fn from(inner: ChildStderr) -> Self {
        #[cfg(feature = "mocking")]
        return ProcessOutput {
            inner: Box::new(inner),
        };
        #[cfg(not(feature = "mocking"))]
        return ProcessOutput {
            inner: ChildStdoutOrErr::Err(inner),
        };
    }
}

#[cfg(feature = "mocking")]
impl From<Box<dyn crate::mock::ProcessOutputMock>> for ProcessOutput {
    fn from(inner: Box<dyn crate::mock::ProcessOutputMock>) -> Self {
        ProcessOutput { inner }
    }
}

#[cfg(not(feature = "mocking"))]
#[derive(Debug)]
enum ChildStdoutOrErr {
    Out(ChildStdout),
    Err(ChildStderr),
}

#[cfg(unix)]
impl AsRawFd for ProcessInput {
    fn as_raw_fd(&self) -> RawFd {
        self.inner.as_raw_fd()
    }
}

#[cfg(unix)]
impl IntoRawFd for ProcessInput {
    fn into_raw_fd(self) -> RawFd {
        self.inner.into_raw_fd()
    }
}

#[cfg(windows)]
impl AsRawHandle for ProcessInput {
    fn as_raw_handle(&self) -> RawHandle {
        self.inner.as_raw_handle()
    }
}

#[cfg(windows)]
impl IntoRawHandle for ProcessInput {
    fn into_raw_handle(self) -> RawHandle {
        self.inner.into_raw_handle()
    }
}

#[cfg(unix)]
impl AsRawFd for ProcessOutput {
    fn as_raw_fd(&self) -> RawFd {
        forward!(&self.inner, as_raw_fd)
    }
}

#[cfg(unix)]
impl IntoRawFd for ProcessOutput {
    fn into_raw_fd(self) -> RawFd {
        forward!(self.inner, into_raw_fd)
    }
}

#[cfg(windows)]
impl AsRawHandle for ProcessOutput {
    fn as_raw_handle(&self) -> RawHandle {
        forward!(&self.inner, as_raw_handle)
    }
}

#[cfg(windows)]
impl IntoRawHandle for ProcessOutput {
    fn into_raw_handle(self) -> RawHandle {
        forward!(self.inner, into_raw_handle)
    }
}

#[cfg(test)]
mod tests {
    use static_assertions::assert_impl_all;
    use std::io;

    use super::*;

    #[test]
    fn process_input_implements_the_right_interfaces() {
        assert_impl_all!(ProcessInput: Send, Debug, io::Write, From<ChildStdin>);
        #[cfg(feature = "mocking")]
        assert_impl_all!(ProcessInput: From<Box<dyn crate::mock::ProcessInputMock>>);
        #[cfg(unix)]
        assert_impl_all!(ProcessInput: AsRawFd, IntoRawFd);
        #[cfg(windows)]
        assert_impl_all!(ProcessInput: AsRawHandle, IntoRawHandle);
        // Debug, io::Write, Send, AsRawFd, IntoRawFd, AsRawHandle, IntoRawHandle
    }

    #[test]
    fn process_output_implements_the_right_interfaces() {
        assert_impl_all!(
            ProcessOutput: Send,
            Debug,
            io::Read,
            From<ChildStdout>,
            From<ChildStderr>
        );
        #[cfg(feature = "mocking")]
        assert_impl_all!(ProcessOutput: From<Box<dyn crate::mock::ProcessOutputMock>>);
        #[cfg(unix)]
        assert_impl_all!(ProcessOutput: AsRawFd, IntoRawFd);
        #[cfg(windows)]
        assert_impl_all!(ProcessOutput: AsRawHandle, IntoRawHandle);
    }
}
