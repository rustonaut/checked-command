//! This module provides utilities for mocking commands through custom [`SpawnImp`] instances.
//TODO all the doc about using Spawner etc.

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
