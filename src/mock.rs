//! This module provides utilities for mocking commands through custom [`SpawnImp`] instances.

use std::{io, sync::Mutex};

use crate::{
    pipe::{ProcessInput, ProcessOutput},
    spawn::{ChildHandle, SpawnImpl, SpawnOptions},
    ExecResult,
};

/// Mocks the result.
///
/// This will:
///
/// - when spawn is called call the passed in function to get
///   the mock result.
///
/// - check if [`SpawnOptions`] match the mock result
/// - return a mocked [`ChildHandle`] which will return the
///   already produced mock output when [`ChildHandle::wait_with_output()`] is called.
///
pub fn mock_result(
    func: impl 'static + Send + Sync + Fn(SpawnOptions, bool, bool) -> Result<ExecResult, io::Error>,
) -> Box<dyn SpawnImpl> {
    MockSpawn::new(move |options, capture_stdout, capture_stderr| {
        Ok(MockResult::new(func(
            options,
            capture_stdout,
            capture_stderr,
        )))
    })
}

/// Mocks the result.
///
/// This will:
///
/// - when spawn is called call the passed in function to get
///   the mock result.
///
/// - check if [`SpawnOptions`] match the mock result
/// - return a mocked [`ChildHandle`] which will return the
///   already produced mock output when [`ChildHandle::wait_with_output()`] is called.
///
/// # Panic
///
/// If the returned [`SpawnImpl`] is used twice this will panic.
///
pub fn mock_result_once(
    func: impl 'static + Send + FnOnce(SpawnOptions, bool, bool) -> Result<ExecResult, io::Error>,
) -> Box<dyn SpawnImpl> {
    let func = Mutex::new(Some(func));
    MockSpawn::new(move |options, capture_stdout, capture_stderr| {
        let func = func.lock().unwrap().take();
        let func = func.expect("SpawnImpl of mock_result_once was used twice");
        Ok(MockResult::new(func(
            options,
            capture_stdout,
            capture_stderr,
        )))
    })
}

/// A mock implementation of `SpawnImpl` which calls a passed in callback on [`SpawnImpl::spawn()`].
#[derive(Debug)]
pub struct MockSpawn<F>
where
    F: 'static
        + Send
        + Sync
        + Fn(SpawnOptions, bool, bool) -> Result<Box<dyn ChildHandle>, io::Error>,
{
    func: F,
}

impl<F> MockSpawn<F>
where
    F: 'static
        + Send
        + Sync
        + Fn(SpawnOptions, bool, bool) -> Result<Box<dyn ChildHandle>, io::Error>,
{
    /// Creates a new instance returning it as a boxed trait object.
    pub fn new(func: F) -> Box<dyn SpawnImpl> {
        Box::new(MockSpawn { func })
    }
}

impl<F> SpawnImpl for MockSpawn<F>
where
    F: 'static
        + Send
        + Sync
        + Fn(SpawnOptions, bool, bool) -> Result<Box<dyn ChildHandle>, io::Error>,
{
    fn spawn(
        &self,
        options: SpawnOptions,
        capture_stdout: bool,
        capture_stderr: bool,
    ) -> Result<Box<dyn ChildHandle>, io::Error> {
        (self.func)(options, capture_stdout, capture_stderr)
    }
}

/// A mock impl of [`ChildHandle`] with a pre-determined capture result.
///
/// This will return it's result once [`ChildHandle::wait_with_output()`] is
/// called.
///
/// Calls to [`ChildHandle::take_stdout()`], [`ChildHandle::take_stderr()`] and
/// [`ChildHandle::take_stdin()`] are currently not emulated and will panic.
///
#[derive(Debug)]
pub struct MockResult {
    result: Result<ExecResult, io::Error>,
}

impl MockResult {
    /// Creates a new instance returning it as a boxed trait object.
    pub fn new(result: Result<ExecResult, io::Error>) -> Box<dyn ChildHandle> {
        Box::new(MockResult { result })
    }
}

impl ChildHandle for MockResult {
    fn take_stdout(&mut self) -> Option<Box<dyn ProcessOutput>> {
        panic!("take_stdout not emulated by MockResult")
    }

    fn take_stderr(&mut self) -> Option<Box<dyn ProcessOutput>> {
        panic!("take_stderr not emulated by MockResult")
    }

    fn take_stdin(&mut self) -> Option<Box<dyn ProcessInput>> {
        panic!("take_stdin not emulated by MockResult")
    }

    fn wait_with_output(self: Box<Self>) -> Result<ExecResult, io::Error> {
        self.result
    }
}

/// A mock impl of [`ChildHandle`] with a pre-determined capture result.
///
/// This will return it's result once [`ChildHandle::wait_with_output()`] is
/// called.
///
/// Calls to [`ChildHandle::take_stdout()`], [`ChildHandle::take_stderr()`] and
/// [`ChildHandle::take_stdin()`] are currently not emulated and will panic.
///
#[derive(Debug)]
pub struct MockResultFn<F>
where
    F: 'static + Send + FnOnce() -> Result<ExecResult, io::Error>,
{
    func: F,
}

impl<F> MockResultFn<F>
where
    F: 'static + Send + FnOnce() -> Result<ExecResult, io::Error>,
{
    /// Creates a new instance returning it as a boxed trait object.
    pub fn new(func: F) -> Box<dyn ChildHandle> {
        Box::new(MockResultFn { func })
    }
}

impl<F> ChildHandle for MockResultFn<F>
where
    F: 'static + Send + FnOnce() -> Result<ExecResult, io::Error>,
{
    fn take_stdout(&mut self) -> Option<Box<dyn ProcessOutput>> {
        panic!("take_stdout not emulated by MockResult")
    }

    fn take_stderr(&mut self) -> Option<Box<dyn ProcessOutput>> {
        panic!("take_stderr not emulated by MockResult")
    }

    fn take_stdin(&mut self) -> Option<Box<dyn ProcessInput>> {
        panic!("take_stdin not emulated by MockResult")
    }

    fn wait_with_output(self: Box<Self>) -> Result<ExecResult, io::Error> {
        (self.func)()
    }
}
