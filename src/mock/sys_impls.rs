use crate::{spawn::SpawnOptions, sys::SysChild, ExecResult};
use once_cell::sync::OnceCell;
use std::{io, sync::Arc};

use super::{ChildMock, ProcessInputMock, ProcessOutputMock, RawPipeRepr, Spawner};

/// Returns a instance of the default spawn implementations.
pub fn default_spawner_impl() -> Arc<dyn Spawner> {
    static DEFAULT_IMPL: OnceCell<Arc<dyn Spawner>> = OnceCell::new();
    DEFAULT_IMPL.get_or_init(|| Arc::new(SysSpawner)).clone()
}

#[derive(Debug)]
pub struct SysSpawner;

impl Spawner for SysSpawner {
    fn spawn(
        &self,
        options: SpawnOptions,
        capture_stdout: bool,
        capture_stderr: bool,
    ) -> Result<Box<dyn ChildMock>, io::Error> {
        crate::sys::spawn(options, capture_stdout, capture_stderr).map(|v| Box::new(v) as _)
    }
}

impl ChildMock for SysChild {
    fn take_stdout(&mut self) -> Option<Box<dyn ProcessOutputMock>> {
        SysChild::take_stdout(self).map(|p| Box::new(p) as _)
    }

    fn take_stderr(&mut self) -> Option<Box<dyn ProcessOutputMock>> {
        SysChild::take_stderr(self).map(|p| Box::new(p) as _)
    }

    fn take_stdin(&mut self) -> Option<Box<dyn ProcessInputMock>> {
        SysChild::take_stdin(self).map(|p| Box::new(p) as _)
    }

    fn wait_with_output(self: Box<Self>) -> Result<ExecResult, io::Error> {
        SysChild::wait_with_output(*self)
    }
}

macro_rules! impl_raw_pipe_repr {
    ($name:ty) => {
        //Safe: This is just re-wrapping a valid to use RawFd/RawHandle.
        unsafe impl RawPipeRepr for $name {
            #[cfg(unix)]
            fn as_raw_fd(&self) -> std::os::unix::prelude::RawFd {
                std::os::unix::io::AsRawFd::as_raw_fd(self)
            }
            #[cfg(unix)]
            fn into_raw_fd(self: Box<Self>) -> std::os::unix::prelude::RawFd {
                std::os::unix::io::IntoRawFd::into_raw_fd(*self)
            }

            #[cfg(windows)]
            fn as_raw_handle(&self) -> std::os::windows::io::RawHandle {
                std::os::unix::io::AsRawHandle::as_raw_handle(self)
            }
            #[cfg(windows)]
            fn into_raw_handle(self: Box<Self>) -> std::os::windows::io::RawHandle {
                std::os::unix::io::IntoRawHandle::into_raw_handle(*self)
            }
        }
    };
}

impl ProcessOutputMock for std::process::ChildStdout {}

impl_raw_pipe_repr!(std::process::ChildStdout);

impl ProcessOutputMock for std::process::ChildStderr {}

impl_raw_pipe_repr!(std::process::ChildStderr);

impl ProcessInputMock for std::process::ChildStdin {}

impl_raw_pipe_repr!(std::process::ChildStdin);
