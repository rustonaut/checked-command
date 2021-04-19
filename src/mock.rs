//! This module provides mocking functionality for `Command`.
//!
//! If the `mocking` feature is enabled this module will be available and
//! some internal implementations of `Command` will change so that they
//! can be mocked.
//!
//! Using [`Command::with_mock_spawner()`] it is possible to override the default
//! child process spawning code. Methods like [`Command::with_mock_result()`]
//! call it internally.
//!
//! If `mocking` is used a `Command` instance will contain a `Arc<dyn Spawner>`
//! which can be changed. When `Command::spawn()` is called it will call
//! the [`Spawner::spawn()`] internally and which returns a `Box<dyn ChildMock>`
//! on success mocking the child handle.  Furthermore [`ProcessInput`] and
//! [`ProcessOutput`] will also use trait objects internally if `mocking` is
//! enabled. (*Note that if the `mocking` feature is not enabled no trait objects will be
//! used at any palace.*)
//!
//! This trait objects allow to mock most parts of command execution, including
//! I/O.
//!
//! For example a custom mock could spawn a thread on wait wait for that thread
//! to terminate and from that thread read/write to the mocked stdout/stderr/stdinn.
//!
//! Some simple mocks are provided out of the box like e.g. `MockResult`.
//!
//! TODO exampls here <-->
//!
//! Through there are some things you need to be aware of.
//!
//! Where mocking stdout/stderr/stdin touches `RawFd` or `RawHandle`
//! it can be hard or even impossible to mock them (especially without
//! unsafe and/or platform specific code). [`ProcessOutputMock`] and
//! [`ProcessInputMock`] implement [`RawPipeRepr`] which allows accessing
//! the `RawFd`/`RawHandle` but the default implementations for mocks
//! will panic as there is no underlying raw representation to return.
//!
//! Which means that most mocks which mock stdout/stdin/stderr won't
//! work if you want to use it in a context where you use
//! [`InputPipeSetup::ExistingPipe`]/[`OutputPipeSetup::ExistingPipe`]
//! to connect pipes of parallel running children.
//!
//! It is possible to even mock this parts, but you will need a
//! `RawFd`/`RawHandle` which generally requires unsafe code!
//!
//! But everything else can be done much simpler, especially with the
//! provided mock helpers.
//!

mod interfaces;
mod mock_impls;
mod sys_impls;

use std::{io, sync::Arc};

pub use interfaces::*;
pub use mock_impls::*;
pub use sys_impls::*;

use crate::{spawn::SpawnOptions, utils::NoDebug, Command, ExecResult, UnexpectedExitStatus};

impl<Output, Error> Command<Output, Error>
where
    Output: 'static,
    Error: From<io::Error> + From<UnexpectedExitStatus> + 'static,
{
    /// Replaces the default spawn implementation.
    ///
    /// This is used by [`Command::with_mock_result()`] and
    /// similar.
    ///
    /// Besides mocking this can also be used to argument the
    /// spawning of an process, e.g. by logging or different
    /// handling of malformed environment variable names.
    #[cfg(feature = "mocking")]
    pub fn with_mock_spawner(mut self, custom_spawner: Arc<dyn Spawner>) -> Self {
        self.custom_spawner = NoDebug(custom_spawner);
        self
    }

    /// Syntax short form for `.with_mock_spawner(crate::mock::mock_result(func))`
    #[cfg(feature = "mocking")]
    pub fn with_mock_result(
        self,
        func: impl 'static + Send + Sync + Fn(SpawnOptions, bool, bool) -> Result<ExecResult, io::Error>,
    ) -> Self {
        self.with_mock_spawner(mock_result(func))
    }

    /// Syntax short form for `.with_mock_spawner(crate::mock::mock_result_once(func))`
    #[cfg(feature = "mocking")]
    pub fn with_mock_result_once(
        self,
        func: impl 'static + Send + FnOnce(SpawnOptions, bool, bool) -> Result<ExecResult, io::Error>,
    ) -> Self {
        self.with_mock_spawner(mock_result_once(func))
    }

    /// Returns true if [`OutputMapping::needs_captured_stdout()`] returns true.
    #[cfg(feature = "mocking")]
    pub fn will_capture_stdout(&self) -> bool {
        self.output_mapping.needs_captured_stdout()
    }

    /// Returns true if [`OutputMapping::needs_captured_stderr()`] returns true.
    #[cfg(feature = "mocking")]
    pub fn will_capture_stderr(&self) -> bool {
        self.output_mapping.needs_captured_stderr()
    }
}
