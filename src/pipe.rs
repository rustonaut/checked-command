//! Types related to the setup of stdout, stderr and stdin pipes.
use std::{
    fmt::Debug,
    fs::File,
    io,
    process::{ChildStderr, ChildStdin, ChildStdout, Stdio},
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
#[non_exhaustive]
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
#[non_exhaustive]
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

#[derive(Debug)]
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

impl From<ProcessInput> for Stdio {
    fn from(p_in: ProcessInput) -> Self {
        #[cfg(not(feature = "mocking"))]
        return Stdio::from(p_in.inner);

        #[cfg(feature = "mocking")]
        {
            #[cfg(unix)]
            use std::os::unix::io::FromRawFd;
            #[cfg(windows)]
            use std::os::unix::io::FromRawHandle;

            //SAFE we take a "wrapped" Stdio convertible RawFd/Handle unwrap it and rewrap it
            unsafe {
                #[cfg(unix)]
                return Stdio::from_raw_fd(p_in.inner.into_raw_fd());
                #[cfg(windows)]
                return Stdio::from_raw_handle(p_in.inner.into_raw_handle());
            }
        }
    }
}

impl From<ProcessOutput> for Stdio {
    fn from(p_out: ProcessOutput) -> Self {
        #[cfg(not(feature = "mocking"))]
        return match p_out.inner {
            ChildStdoutOrErr::Out(inner) => inner.into(),
            ChildStdoutOrErr::Err(inner) => inner.into(),
        };

        #[cfg(feature = "mocking")]
        {
            #[cfg(unix)]
            use std::os::unix::io::FromRawFd;
            #[cfg(windows)]
            use std::os::unix::io::FromRawHandle;

            //SAFE we take a "wrapped" Stdio convertible RawFd/Handle unwrap it and rewrap it
            unsafe {
                #[cfg(unix)]
                return Stdio::from_raw_fd(p_out.inner.into_raw_fd());

                #[cfg(windows)]
                return Stdio::from_raw_handle(p_out.inner.into_raw_handle());
            }
        }
    }
}

impl From<OutputPipeSetup> for Stdio {
    fn from(setup: OutputPipeSetup) -> Self {
        match setup {
            OutputPipeSetup::ExistingPipe(ep) => ep.into(),
            OutputPipeSetup::File(f) => f.into(),
            OutputPipeSetup::ChildStdin(p) => p.into(),
            OutputPipeSetup::Piped => Stdio::piped(),
            OutputPipeSetup::Null => Stdio::null(),
            OutputPipeSetup::Inherit => Stdio::inherit(),
        }
    }
}

impl From<InputPipeSetup> for Stdio {
    fn from(setup: InputPipeSetup) -> Self {
        match setup {
            InputPipeSetup::ExistingPipe(ep) => ep.into(),
            InputPipeSetup::File(f) => f.into(),
            InputPipeSetup::ChildStdout(p) => p.into(),
            InputPipeSetup::ChildStderr(p) => p.into(),
            InputPipeSetup::Piped => Stdio::piped(),
            InputPipeSetup::Null => Stdio::null(),
            InputPipeSetup::Inherit => Stdio::inherit(),
        }
    }
}

impl PartialEq<Piped> for InputPipeSetup {
    fn eq(&self, _: &Piped) -> bool {
        if let Self::Piped = self {
            true
        } else {
            false
        }
    }
}

impl PartialEq<Inherit> for InputPipeSetup {
    fn eq(&self, _: &Inherit) -> bool {
        if let Self::Inherit = self {
            true
        } else {
            false
        }
    }
}

impl PartialEq<Null> for InputPipeSetup {
    fn eq(&self, _: &Null) -> bool {
        if let Self::Null = self {
            true
        } else {
            false
        }
    }
}

impl PartialEq<Piped> for OutputPipeSetup {
    fn eq(&self, _: &Piped) -> bool {
        if let Self::Piped = self {
            true
        } else {
            false
        }
    }
}

impl PartialEq<Inherit> for OutputPipeSetup {
    fn eq(&self, _: &Inherit) -> bool {
        if let Self::Inherit = self {
            true
        } else {
            false
        }
    }
}

impl PartialEq<Null> for OutputPipeSetup {
    fn eq(&self, _: &Null) -> bool {
        if let Self::Null = self {
            true
        } else {
            false
        }
    }
}

#[cfg(test)]
mod tests {
    use static_assertions::assert_impl_all;
    use std::io;

    use super::*;

    #[test]
    fn process_input_implements_the_right_interfaces() {
        assert_impl_all!(Stdio: From<ProcessInput>);
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
        assert_impl_all!(Stdio: From<ProcessOutput>);
        assert_impl_all!(
            ProcessOutput: Send,
            Debug,
            io::Read,
            From<ChildStdout>,
            From<ChildStderr>,
        );
        #[cfg(feature = "mocking")]
        assert_impl_all!(ProcessOutput: From<Box<dyn crate::mock::ProcessOutputMock>>);
        #[cfg(unix)]
        assert_impl_all!(ProcessOutput: AsRawFd, IntoRawFd);
        #[cfg(windows)]
        assert_impl_all!(ProcessOutput: AsRawHandle, IntoRawHandle);
    }

    mod InputPipeSetup {
        #![allow(non_snake_case)]

        use static_assertions::assert_impl_all;

        use super::super::*;

        #[test]
        fn impls_the_right_simple_traits() {
            assert_impl_all!(Stdio: From<InputPipeSetup>);
            assert_impl_all!(
                InputPipeSetup: Debug,
                From<File>,
                From<ChildStdout>,
                From<ChildStderr>,
                From<ProcessOutput>
            );
        }

        mod Eq {
            use super::super::super::*;

            #[test]
            fn to_piped() {
                assert_eq!(InputPipeSetup::Piped, Piped);
                assert_ne!(InputPipeSetup::Inherit, Piped);
                assert_ne!(InputPipeSetup::Null, Piped);
            }

            #[test]
            fn to_inherit() {
                assert_ne!(InputPipeSetup::Piped, Inherit);
                assert_eq!(InputPipeSetup::Inherit, Inherit);
                assert_ne!(InputPipeSetup::Null, Inherit);
            }

            #[test]
            fn to_null() {
                assert_ne!(InputPipeSetup::Piped, Null);
                assert_ne!(InputPipeSetup::Inherit, Null);
                assert_eq!(InputPipeSetup::Null, Null);
            }
        }
    }

    mod OutputPipeSetup {
        #![allow(non_snake_case)]

        use static_assertions::assert_impl_all;

        use super::super::*;

        #[test]
        fn impls_the_right_simple_traits() {
            assert_impl_all!(Stdio: From<OutputPipeSetup>);
            assert_impl_all!(
                OutputPipeSetup: Debug,
                From<File>,
                From<ChildStdin>,
                From<ProcessInput>
            );
        }

        mod Eq {
            use super::super::super::*;

            #[test]
            fn to_piped() {
                assert_eq!(OutputPipeSetup::Piped, Piped);
                assert_ne!(OutputPipeSetup::Inherit, Piped);
                assert_ne!(OutputPipeSetup::Null, Piped);
            }

            #[test]
            fn to_inherit() {
                assert_ne!(OutputPipeSetup::Piped, Inherit);
                assert_eq!(OutputPipeSetup::Inherit, Inherit);
                assert_ne!(OutputPipeSetup::Null, Inherit);
            }

            #[test]
            fn to_null() {
                assert_ne!(OutputPipeSetup::Piped, Null);
                assert_ne!(OutputPipeSetup::Inherit, Null);
                assert_eq!(OutputPipeSetup::Null, Null);
            }
        }
    }
}
