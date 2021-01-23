use std::string::FromUtf8Error;

use super::{CommandExecutionError, ReturnSettings};
use crate::ExitStatus;
use thiserror::Error;

/// Return `()` if the program successfully exits.
#[derive(Debug)]
pub struct ReturnNothing;

impl ReturnSettings for ReturnNothing {
    type Output = ();
    type Error = CommandExecutionError;

    fn capture_stdout(&self) -> bool {
        false
    }

    fn capture_stderr(&self) -> bool {
        false
    }

    fn map_output(
        self: Box<Self>,
        _stdout: Option<Vec<u8>>,
        _stderr: Option<Vec<u8>>,
        _exit_status: ExitStatus,
    ) -> Result<Self::Output, Self::Error> {
        Ok(())
    }
}

/// Returns a `Vec<u8>` of the captured stdout if the process exits successfully.
#[derive(Debug)]
pub struct ReturnStdout;

impl ReturnSettings for ReturnStdout {
    type Output = Vec<u8>;
    type Error = CommandExecutionError;

    fn capture_stdout(&self) -> bool {
        true
    }

    fn capture_stderr(&self) -> bool {
        false
    }

    fn map_output(
        self: Box<Self>,
        stdout: Option<Vec<u8>>,
        _stderr: Option<Vec<u8>>,
        _exit_status: ExitStatus,
    ) -> Result<Self::Output, Self::Error> {
        Ok(stdout.unwrap())
    }
}

/// Returns a `Vec<u8>` of the captured stderr if the process exits successfully.
#[derive(Debug)]
pub struct ReturnStderr;

impl ReturnSettings for ReturnStderr {
    type Output = Vec<u8>;
    type Error = CommandExecutionError;

    fn capture_stdout(&self) -> bool {
        false
    }

    fn capture_stderr(&self) -> bool {
        true
    }

    fn map_output(
        self: Box<Self>,
        _stdout: Option<Vec<u8>>,
        stderr: Option<Vec<u8>>,
        _exit_status: ExitStatus,
    ) -> Result<Self::Output, Self::Error> {
        Ok(stderr.unwrap())
    }
}

/// Returns a `Vec<u8>` of the captured stderr and stdout if the process exits successfully.
#[derive(Debug)]
pub struct ReturnStdoutAndErr;

impl ReturnSettings for ReturnStdoutAndErr {
    type Output = CapturedStdoutAndErr;
    type Error = CommandExecutionError;

    fn capture_stdout(&self) -> bool {
        true
    }

    fn capture_stderr(&self) -> bool {
        true
    }

    fn map_output(
        self: Box<Self>,
        stdout: Option<Vec<u8>>,
        stderr: Option<Vec<u8>>,
        _exit_status: ExitStatus,
    ) -> Result<Self::Output, Self::Error> {
        Ok(CapturedStdoutAndErr {
            stdout: stdout.unwrap(),
            stderr: stderr.unwrap(),
        })
    }
}

/// The captured stdout and stderr.
#[derive(Debug)]
pub struct CapturedStdoutAndErr {
    pub stdout: Vec<u8>,
    pub stderr: Vec<u8>,
}

/// Maps the captured stdout with given function if the process exited successfully.
#[derive(Debug)]
pub struct MapStdout<O, E, F>(pub F)
where
    F: FnMut(Vec<u8>) -> Result<O, E> + 'static,
    E: From<CommandExecutionError> + 'static,
    O: 'static;

impl<O, E, F> ReturnSettings for MapStdout<O, E, F>
where
    F: FnMut(Vec<u8>) -> Result<O, E>,
    E: From<CommandExecutionError>,
{
    type Output = O;
    type Error = E;

    fn capture_stdout(&self) -> bool {
        true
    }

    fn capture_stderr(&self) -> bool {
        false
    }

    fn map_output(
        mut self: Box<Self>,
        stdout: Option<Vec<u8>>,
        _stderr: Option<Vec<u8>>,
        _exit_status: ExitStatus,
    ) -> Result<Self::Output, Self::Error> {
        (self.0)(stdout.unwrap())
    }
}

/// Maps the captured stderr with given function if the process exited successfully.
#[derive(Debug)]
pub struct MapStderr<O, E, F>(pub F)
where
    F: FnMut(Vec<u8>) -> Result<O, E> + 'static,
    E: From<CommandExecutionError> + 'static,
    O: 'static;

impl<O, E, F> ReturnSettings for MapStderr<O, E, F>
where
    F: FnMut(Vec<u8>) -> Result<O, E>,
    E: From<CommandExecutionError>,
{
    type Output = O;
    type Error = E;

    fn capture_stdout(&self) -> bool {
        false
    }

    fn capture_stderr(&self) -> bool {
        true
    }

    fn map_output(
        mut self: Box<Self>,
        _stdout: Option<Vec<u8>>,
        stderr: Option<Vec<u8>>,
        _exit_status: ExitStatus,
    ) -> Result<Self::Output, Self::Error> {
        (self.0)(stderr.unwrap())
    }
}

/// Maps the captured stdout and stderr with given function if the process exited successfully.
#[derive(Debug)]
pub struct MapStdoutAndErr<O, E, F>(pub F)
where
    F: FnMut(CapturedStdoutAndErr) -> Result<O, E> + 'static,
    E: From<CommandExecutionError> + 'static,
    O: 'static;

impl<O, E, F> ReturnSettings for MapStdoutAndErr<O, E, F>
where
    F: FnMut(CapturedStdoutAndErr) -> Result<O, E>,
    E: From<CommandExecutionError>,
{
    type Output = O;
    type Error = E;

    fn capture_stdout(&self) -> bool {
        true
    }

    fn capture_stderr(&self) -> bool {
        true
    }

    fn map_output(
        mut self: Box<Self>,
        stdout: Option<Vec<u8>>,
        stderr: Option<Vec<u8>>,
        _exit_status: ExitStatus,
    ) -> Result<Self::Output, Self::Error> {
        (self.0)(CapturedStdoutAndErr {
            stdout: stdout.unwrap(),
            stderr: stderr.unwrap(),
        })
    }
}

/// Error from running a command which maps (some) outputs to strings.
#[derive(Debug, Error)]
pub enum CommandExecutionWithStringOutputError {
    /// Spawning failed or bad exit code.
    #[error(transparent)]
    ExecError(#[from] CommandExecutionError),

    /// Utf8 validation failed.
    #[error("Output pipe contained non utf8 characters: {}", _0)]
    Utf8Error(#[from] FromUtf8Error),
}

/// Map a stdout/err output (Vec<u8>) to an string.
///
/// This is a thin wrapper around [`String::from_utf8()`] which
/// maps the error to `CommandExecutionWithStringOutputError`.
fn output_to_string(output: Vec<u8>) -> Result<String, CommandExecutionWithStringOutputError> {
    Ok(String::from_utf8(output)?)
}

/// Returns the captured stdout as string, if the process succeeds.
#[derive(Debug)]
pub struct ReturnStdoutString;

impl ReturnSettings for ReturnStdoutString {
    type Output = String;
    type Error = CommandExecutionWithStringOutputError;

    fn capture_stdout(&self) -> bool {
        true
    }

    fn capture_stderr(&self) -> bool {
        false
    }

    fn map_output(
        self: Box<Self>,
        stdout: Option<Vec<u8>>,
        _stderr: Option<Vec<u8>>,
        _exit_status: ExitStatus,
    ) -> Result<Self::Output, Self::Error> {
        Ok(output_to_string(stdout.unwrap())?)
    }
}

/// Returns the captured stderr as string, if the process succeeds.
#[derive(Debug)]
pub struct ReturnStderrString;

impl ReturnSettings for ReturnStderrString {
    type Output = String;
    type Error = CommandExecutionWithStringOutputError;

    fn capture_stdout(&self) -> bool {
        false
    }

    fn capture_stderr(&self) -> bool {
        true
    }

    fn map_output(
        self: Box<Self>,
        _stdout: Option<Vec<u8>>,
        stderr: Option<Vec<u8>>,
        _exit_status: ExitStatus,
    ) -> Result<Self::Output, Self::Error> {
        Ok(output_to_string(stderr.unwrap())?)
    }
}

/// Returns the captured stdout and stderr as strings, if the process succeeds.
#[derive(Debug)]
pub struct ReturnStdoutAndErrStrings;

impl ReturnSettings for ReturnStdoutAndErrStrings {
    type Output = CapturedStdoutAndErrStrings;
    type Error = CommandExecutionWithStringOutputError;

    fn capture_stdout(&self) -> bool {
        true
    }

    fn capture_stderr(&self) -> bool {
        true
    }

    fn map_output(
        self: Box<Self>,
        stdout: Option<Vec<u8>>,
        stderr: Option<Vec<u8>>,
        _exit_status: ExitStatus,
    ) -> Result<Self::Output, Self::Error> {
        Ok(CapturedStdoutAndErrStrings {
            stdout: output_to_string(stdout.unwrap())?,
            stderr: output_to_string(stderr.unwrap())?,
        })
    }
}

/// Capturing of stdout/err converted from bytes to strings.
#[derive(Debug)]
pub struct CapturedStdoutAndErrStrings {
    pub stdout: String,
    pub stderr: String,
}

/// Like [`MapStdout`] but converts the captured stdout to an string before mapping.
#[derive(Debug)]
pub struct MapStdoutString<O, E, F>(pub F)
where
    F: FnMut(String) -> Result<O, E> + 'static,
    E: From<CommandExecutionWithStringOutputError> + 'static,
    O: 'static;

impl<O, E, F> ReturnSettings for MapStdoutString<O, E, F>
where
    F: FnMut(String) -> Result<O, E>,
    E: From<CommandExecutionWithStringOutputError>,
{
    type Output = O;
    type Error = E;

    fn capture_stdout(&self) -> bool {
        true
    }

    fn capture_stderr(&self) -> bool {
        false
    }

    fn map_output(
        mut self: Box<Self>,
        stdout: Option<Vec<u8>>,
        _stderr: Option<Vec<u8>>,
        _exit_status: ExitStatus,
    ) -> Result<Self::Output, Self::Error> {
        (self.0)(output_to_string(stdout.unwrap())?)
    }
}

/// Like [`MapStderr`] but converts the captured stdout to an string before mapping.
#[derive(Debug)]
pub struct MapStderrString<O, E, F>(pub F)
where
    F: FnMut(String) -> Result<O, E> + 'static,
    E: From<CommandExecutionWithStringOutputError> + 'static,
    O: 'static;

impl<O, E, F> ReturnSettings for MapStderrString<O, E, F>
where
    F: FnMut(String) -> Result<O, E>,
    E: From<CommandExecutionWithStringOutputError>,
{
    type Output = O;
    type Error = E;

    fn capture_stdout(&self) -> bool {
        false
    }

    fn capture_stderr(&self) -> bool {
        true
    }

    fn map_output(
        mut self: Box<Self>,
        _stdout: Option<Vec<u8>>,
        stderr: Option<Vec<u8>>,
        _exit_status: ExitStatus,
    ) -> Result<Self::Output, Self::Error> {
        (self.0)(output_to_string(stderr.unwrap())?)
    }
}

/// Like [`MapStdoutAndErr`] but converts the captured stdout and stderr to strings before mapping.
#[derive(Debug)]
pub struct MapStdoutAndErrStrings<O, E, F>(pub F)
where
    F: FnMut(CapturedStdoutAndErrStrings) -> Result<O, E> + 'static,
    E: From<CommandExecutionWithStringOutputError> + 'static,
    O: 'static;

impl<O, E, F> ReturnSettings for MapStdoutAndErrStrings<O, E, F>
where
    F: FnMut(CapturedStdoutAndErrStrings) -> Result<O, E>,
    E: From<CommandExecutionWithStringOutputError>,
{
    type Output = O;
    type Error = E;

    fn capture_stdout(&self) -> bool {
        true
    }

    fn capture_stderr(&self) -> bool {
        true
    }

    fn map_output(
        mut self: Box<Self>,
        stdout: Option<Vec<u8>>,
        stderr: Option<Vec<u8>>,
        _exit_status: ExitStatus,
    ) -> Result<Self::Output, Self::Error> {
        (self.0)(CapturedStdoutAndErrStrings {
            stdout: output_to_string(stdout.unwrap())?,
            stderr: output_to_string(stderr.unwrap())?,
        })
    }
}

#[cfg(test)]
mod tests {
    #![allow(non_snake_case)]

    use super::{output_to_string, CommandExecutionWithStringOutputError};

    mod ReturnNothing {
        use super::super::*;
        use crate::{Command, ExecResult};

        #[test]
        fn captures_stdout_returns_true() {
            assert_eq!(ReturnNothing.capture_stdout(), false);
        }

        #[test]
        fn captures_stderr_returns_false() {
            assert_eq!(ReturnNothing.capture_stderr(), false);
        }

        #[test]
        fn returns_nothing() {
            let _: () = Command::new("foo", ReturnNothing)
                .with_exec_replacement_callback(move |_, _| {
                    Ok(ExecResult {
                        exit_status: 0.into(),
                        stdout: None,
                        stderr: None,
                    })
                })
                .run()
                .unwrap();
        }
    }

    mod ReturnStdout {
        use super::super::*;
        use crate::{Command, ExecResult};
        use proptest::prelude::*;

        #[test]
        fn captures_stdout_returns_true() {
            assert_eq!(ReturnStdout.capture_stdout(), true);
        }

        #[test]
        fn captures_stderr_returns_false() {
            assert_eq!(ReturnStdout.capture_stderr(), false);
        }

        proptest! {
            #[test]
            fn returns_only_captured_std_out_but_not_err(
                stdout in any::<Vec<u8>>(),
            ) {
                let stdout_ = stdout.clone();
                let out = Command::new("foo", ReturnStdout)
                    .with_exec_replacement_callback(move |_,_| {
                        Ok(ExecResult {
                            exit_status: 0.into(),
                            stdout: Some(stdout_),
                            stderr: None
                        })
                    })
                    .run()
                    .unwrap();

                assert_eq!(out, stdout);
            }
        }
    }

    mod ReturnStderr {
        use super::super::*;
        use crate::{Command, ExecResult};
        use proptest::prelude::*;

        #[test]
        fn captures_stdout_returns_true() {
            assert_eq!(ReturnStderr.capture_stdout(), false);
        }

        #[test]
        fn captures_stderr_returns_false() {
            assert_eq!(ReturnStderr.capture_stderr(), true);
        }

        proptest! {
            #[test]
            fn returns_only_captured_std_err_but_not_out(
                stderr in any::<Vec<u8>>()
            ) {
                let stderr_ = stderr.clone();
                let out = Command::new("foo", ReturnStderr)
                    .with_exec_replacement_callback(move |_,_| {
                        Ok(ExecResult {
                            exit_status: 0.into(),
                            stdout: None,
                            stderr: Some(stderr_)
                        })
                    })
                    .run()
                    .unwrap();

                assert_eq!(out, stderr);
            }
        }
    }

    mod ReturnStdoutAndErr {
        use super::super::*;
        use crate::{Command, ExecResult};
        use proptest::prelude::*;

        #[test]
        fn captures_stdout_returns_true() {
            assert_eq!(ReturnStdoutAndErr.capture_stdout(), true);
        }

        #[test]
        fn captures_stderr_returns_false() {
            assert_eq!(ReturnStdoutAndErr.capture_stderr(), true);
        }

        proptest! {
            #[test]
            fn returns_captured_std_out_and_err(
                stdout in any::<Vec<u8>>(),
                stderr in any::<Vec<u8>>()
            ) {
                let stdout_ = stdout.clone();
                let stderr_ = stderr.clone();
                let out: CapturedStdoutAndErr = Command::new("foo", ReturnStdoutAndErr)
                    .with_exec_replacement_callback(move |_,_| {
                        Ok(ExecResult {
                            exit_status: 0.into(),
                            stdout: Some(stdout_),
                            stderr: Some(stderr_)
                        })
                    })
                    .run()
                    .unwrap();

                assert_eq!(out.stdout, stdout);
                assert_eq!(out.stderr, stderr);
            }
        }
    }

    mod MapStdout {
        use super::super::*;
        use crate::{Command, ExecResult};

        #[test]
        fn maps_stdout_to_a_result() {
            let res = Command::new(
                "foo",
                MapStdout(|out| -> Result<u32, Box<dyn std::error::Error>> {
                    Ok(String::from_utf8(out)?.parse()?)
                }),
            )
            .with_exec_replacement_callback(|_, _| {
                Ok(ExecResult {
                    exit_status: 0.into(),
                    stdout: Some("3241".into()),
                    stderr: None,
                })
            })
            .run()
            .unwrap();

            assert_eq!(res, 3241u32);
        }

        #[test]
        fn mapping_stdout_to_a_result_can_fail() {
            Command::new(
                "foo",
                MapStdout(|out| -> Result<u32, Box<dyn std::error::Error>> {
                    Ok(String::from_utf8(out)?.parse()?)
                }),
            )
            .with_exec_replacement_callback(|_, _| {
                Ok(ExecResult {
                    exit_status: 0.into(),
                    stdout: Some("abcd".into()),
                    stderr: None,
                })
            })
            .run()
            .unwrap_err();
        }
    }

    mod MapStderr {
        use super::super::*;
        use crate::{Command, ExecResult};

        #[test]
        fn maps_stderr_to_a_result() {
            let res = Command::new(
                "foo",
                MapStderr(|err| -> Result<u32, Box<dyn std::error::Error>> {
                    Ok(String::from_utf8(err)?.parse()?)
                }),
            )
            .with_exec_replacement_callback(|_, _| {
                Ok(ExecResult {
                    exit_status: 0.into(),
                    stderr: Some("3241".into()),
                    stdout: None,
                })
            })
            .run()
            .unwrap();

            assert_eq!(res, 3241u32);
        }

        #[test]
        fn mapping_stderr_to_a_result_can_fail() {
            Command::new(
                "foo",
                MapStderr(|err| -> Result<u32, Box<dyn std::error::Error>> {
                    Ok(String::from_utf8(err)?.parse()?)
                }),
            )
            .with_exec_replacement_callback(|_, _| {
                Ok(ExecResult {
                    exit_status: 0.into(),
                    stdout: None,
                    stderr: Some("abcd".into()),
                })
            })
            .run()
            .unwrap_err();
        }
    }

    mod MapStdoutAndErr {
        use super::super::*;
        use crate::{Command, ExecResult};

        #[test]
        fn maps_stdout_to_a_result() {
            let res = Command::new(
                "foo",
                MapStdoutAndErr(|cap| -> Result<(u32, u32), Box<dyn std::error::Error>> {
                    let out_res = String::from_utf8(cap.stdout)?.parse()?;
                    let err_res = String::from_utf8(cap.stderr)?.parse()?;
                    Ok((out_res, err_res))
                }),
            )
            .with_exec_replacement_callback(|_, _| {
                Ok(ExecResult {
                    exit_status: 0.into(),
                    stdout: Some("3241".into()),
                    stderr: Some("1242".into()),
                })
            })
            .run()
            .unwrap();

            assert_eq!(res, (3241u32, 1242u32));
        }

        #[test]
        fn mapping_stdout_to_a_result_can_fail() {
            Command::new(
                "foo",
                MapStdoutAndErr(|_| -> Result<u32, Box<dyn std::error::Error>> {
                    Err("yes this fails")?
                }),
            )
            .with_exec_replacement_callback(|_, _| {
                Ok(ExecResult {
                    exit_status: 0.into(),
                    stdout: Some(Vec::new()),
                    stderr: Some(Vec::new()),
                })
            })
            .run()
            .unwrap_err();
        }
    }

    fn is_utf8_error(err: &CommandExecutionWithStringOutputError) -> bool {
        if let CommandExecutionWithStringOutputError::Utf8Error(_) = err {
            true
        } else {
            false
        }
    }

    #[test]
    fn output_to_string_fails_on_bad_strings() {
        let err = output_to_string(vec![0xFF, 0xFF, 0xFF]).unwrap_err();

        if !is_utf8_error(&err) {
            panic!("unexpected error: {:?}", err);
        }
    }

    #[test]
    fn output_to_string_converts_utf8_bytes_to_string() {
        let out = output_to_string("hy".to_owned().into_bytes()).unwrap();
        assert_eq!(out, "hy");
    }

    mod ReturnStdoutString {
        use super::super::*;
        use crate::{Command, ExecResult};
        use proptest::prelude::*;
        use tests::is_utf8_error;

        #[test]
        fn captures_stdout_returns_true() {
            assert_eq!(ReturnStdoutString.capture_stdout(), true);
        }

        #[test]
        fn captures_stderr_returns_false() {
            assert_eq!(ReturnStdoutString.capture_stderr(), false);
        }

        proptest! {
            #[test]
            fn returns_only_captured_std_out_but_not_err(
                stdout in any::<Vec<u8>>(),
            ) {
                let stdout_ = stdout.clone();
                let res = Command::new("foo", ReturnStdoutString)
                    .with_exec_replacement_callback(move |_,_| {
                        Ok(ExecResult {
                            exit_status: 0.into(),
                            stdout: Some(stdout_),
                            stderr: None
                        })
                    })
                    .run();

                let expected = output_to_string(stdout);

                match expected {
                    Ok(expected) => {
                        let got = res.unwrap();
                        assert_eq!(expected, got);
                    },
                    Err(error) => {
                        assert!(is_utf8_error(&error));
                    }
                }
            }
        }
    }

    mod ReturnStderrString {
        use super::{super::*, is_utf8_error};
        use crate::{Command, ExecResult};
        use proptest::prelude::*;

        #[test]
        fn captures_stdout_returns_true() {
            assert_eq!(ReturnStderrString.capture_stdout(), false);
        }

        #[test]
        fn captures_stderr_returns_false() {
            assert_eq!(ReturnStderrString.capture_stderr(), true);
        }

        proptest! {
            #[test]
            fn returns_only_captured_std_err_but_not_out(
                stderr in any::<Vec<u8>>()
            ) {
                let stderr_ = stderr.clone();
                let res = Command::new("foo", ReturnStderrString)
                    .with_exec_replacement_callback(move |_,_| {
                        Ok(ExecResult {
                            exit_status: 0.into(),
                            stdout: None,
                            stderr: Some(stderr_)
                        })
                    })
                    .run();

                let expected = output_to_string(stderr);

                match expected {
                    Ok(expected) => {
                        let got = res.unwrap();
                        assert_eq!(expected, got);
                    },
                    Err(error) => {
                        assert!(is_utf8_error(&error));
                    }
                }
            }
        }
    }

    mod ReturnStdoutAndErrStrings {
        use super::{super::*, is_utf8_error};
        use crate::{Command, ExecResult};
        use proptest::prelude::*;

        #[test]
        fn captures_stdout_returns_true() {
            assert_eq!(ReturnStdoutAndErrStrings.capture_stdout(), true);
        }

        #[test]
        fn captures_stderr_returns_false() {
            assert_eq!(ReturnStdoutAndErrStrings.capture_stderr(), true);
        }

        proptest! {
            #[test]
            fn returns_captured_std_out_and_err(
                stdout in any::<Vec<u8>>(),
                stderr in any::<Vec<u8>>()
            ) {
                let stdout_ = stdout.clone();
                let stderr_ = stderr.clone();
                let res = Command::new("foo", ReturnStdoutAndErrStrings)
                    .with_exec_replacement_callback(move |_,_| {
                        Ok(ExecResult {
                            exit_status: 0.into(),
                            stdout: Some(stdout_),
                            stderr: Some(stderr_)
                        })
                    })
                    .run();

                let expected = output_to_string(stdout)
                    .and_then(|stdout| Ok((stdout, output_to_string(stderr)?)));

                match expected {
                    Ok((expected_stdout, expected_stderr)) => {
                        let got = res.unwrap();
                        assert_eq!(expected_stdout, got.stdout);
                        assert_eq!(expected_stderr, got.stderr);
                    },
                    Err(error) => {
                        assert!(is_utf8_error(&error));
                    }
                }
            }
        }
    }

    mod MapStdoutStrings {
        use super::super::*;
        use crate::{Command, ExecResult};

        #[test]
        fn maps_stdout_to_a_result() {
            let res = Command::new(
                "foo",
                MapStdoutString(|out| -> Result<u32, Box<dyn std::error::Error>> {
                    Ok(out.parse()?)
                }),
            )
            .with_exec_replacement_callback(|_, _| {
                Ok(ExecResult {
                    exit_status: 0.into(),
                    stdout: Some("3241".into()),
                    stderr: None,
                })
            })
            .run()
            .unwrap();

            assert_eq!(res, 3241u32);
        }

        #[test]
        fn mapping_stdout_to_a_result_can_fail() {
            Command::new(
                "foo",
                MapStdoutString(|out| -> Result<u32, Box<dyn std::error::Error>> {
                    Ok(out.parse()?)
                }),
            )
            .with_exec_replacement_callback(|_, _| {
                Ok(ExecResult {
                    exit_status: 0.into(),
                    stdout: Some("abcd".into()),
                    stderr: None,
                })
            })
            .run()
            .unwrap_err();
        }
    }

    mod MapStderrString {
        use super::super::*;
        use crate::{Command, ExecResult};

        #[test]
        fn maps_stderr_to_a_result() {
            let res = Command::new(
                "foo",
                MapStderrString(|err| -> Result<u32, Box<dyn std::error::Error>> {
                    Ok(err.parse()?)
                }),
            )
            .with_exec_replacement_callback(|_, _| {
                Ok(ExecResult {
                    exit_status: 0.into(),
                    stderr: Some("3241".into()),
                    stdout: None,
                })
            })
            .run()
            .unwrap();

            assert_eq!(res, 3241u32);
        }

        #[test]
        fn mapping_stderr_to_a_result_can_fail() {
            Command::new(
                "foo",
                MapStderrString(|err| -> Result<u32, Box<dyn std::error::Error>> {
                    Ok(err.parse()?)
                }),
            )
            .with_exec_replacement_callback(|_, _| {
                Ok(ExecResult {
                    exit_status: 0.into(),
                    stdout: None,
                    stderr: Some("abcd".into()),
                })
            })
            .run()
            .unwrap_err();
        }
    }

    mod MapStdoutAndErrStrings {
        use super::super::*;
        use crate::{Command, ExecResult};

        #[test]
        fn maps_stdout_to_a_result() {
            let res = Command::new(
                "foo",
                MapStdoutAndErrStrings(|cap| -> Result<(u32, u32), Box<dyn std::error::Error>> {
                    let out_res = cap.stdout.parse()?;
                    let err_res = cap.stderr.parse()?;
                    Ok((out_res, err_res))
                }),
            )
            .with_exec_replacement_callback(|_, _| {
                Ok(ExecResult {
                    exit_status: 0.into(),
                    stdout: Some("3241".into()),
                    stderr: Some("1242".into()),
                })
            })
            .run()
            .unwrap();

            assert_eq!(res, (3241u32, 1242u32));
        }

        #[test]
        fn mapping_stdout_to_a_result_can_fail() {
            Command::new(
                "foo",
                MapStdoutAndErrStrings(|_| -> Result<u32, Box<dyn std::error::Error>> {
                    Err("yes this fails")?
                }),
            )
            .with_exec_replacement_callback(|_, _| {
                Ok(ExecResult {
                    exit_status: 0.into(),
                    stdout: Some(Vec::new()),
                    stderr: Some(Vec::new()),
                })
            })
            .run()
            .unwrap_err();
        }
    }

    //TODO proptest against string parsing failure in Map* ReturnSettings
}
