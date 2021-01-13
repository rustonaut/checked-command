use super::{CommandExecutionError, ReturnSettings};

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
        _exit_code: i32,
    ) -> Result<Self::Output, Self::Error> {
        Ok(())
    }
}

#[derive(Debug)]
pub struct ReturnStdout;

impl ReturnSettings for ReturnStdout {
    type Output = CapturedStdout;
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
        _exit_code: i32,
    ) -> Result<Self::Output, Self::Error> {
        Ok(CapturedStdout {
            stdout: stdout.unwrap(),
        })
    }
}

#[derive(Debug)]
pub struct CapturedStdout {
    pub stdout: Vec<u8>,
}

#[derive(Debug)]
pub struct ReturnStderr;

impl ReturnSettings for ReturnStderr {
    type Output = CapturedStderr;
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
        _exit_code: i32,
    ) -> Result<Self::Output, Self::Error> {
        Ok(CapturedStderr {
            stderr: stderr.unwrap(),
        })
    }
}

#[derive(Debug)]
pub struct CapturedStderr {
    pub stderr: Vec<u8>,
}

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
        _exit_code: i32,
    ) -> Result<Self::Output, Self::Error> {
        Ok(CapturedStdoutAndErr {
            stdout: stdout.unwrap(),
            stderr: stderr.unwrap(),
        })
    }
}

#[derive(Debug)]
pub struct CapturedStdoutAndErr {
    pub stdout: Vec<u8>,
    pub stderr: Vec<u8>,
}

#[derive(Debug)]
pub struct MapStdout<F, O, E>(pub F)
where
    F: FnMut(CapturedStdout) -> Result<O, E> + 'static,
    E: From<CommandExecutionError> + 'static,
    O: 'static;


impl<F, O, E> ReturnSettings for MapStdout<F, O, E>
where
    F: FnMut(CapturedStdout) -> Result<O, E>,
    E: From<CommandExecutionError>
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
        _exit_code: i32,
    ) -> Result<Self::Output, Self::Error> {
        (self.0)(CapturedStdout { stdout: stdout.unwrap()})
    }
}

#[derive(Debug)]
pub struct MapStderr<F, O, E>(pub F)
where
    F: FnMut(CapturedStderr) -> Result<O, E> + 'static,
    E: From<CommandExecutionError> + 'static,
    O: 'static;

impl<F, O, E> ReturnSettings for MapStderr<F, O, E>
where
    F: FnMut(CapturedStderr) -> Result<O, E>,
    E: From<CommandExecutionError>
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
        _exit_code: i32,
    ) -> Result<Self::Output, Self::Error> {
        (self.0)(CapturedStderr { stderr: stderr.unwrap()})
    }
}

#[derive(Debug)]
pub struct MapStdoutAndErr<F, O, E>(pub F)
where
    F: FnMut(CapturedStdoutAndErr) -> Result<O, E> + 'static,
    E: From<CommandExecutionError> + 'static,
    O: 'static;

impl<F, O, E> ReturnSettings for MapStdoutAndErr<F, O, E>
where
    F: FnMut(CapturedStdoutAndErr) -> Result<O, E>,
    E: From<CommandExecutionError>
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
        _exit_code: i32,
    ) -> Result<Self::Output, Self::Error> {
        (self.0)(CapturedStdoutAndErr { stdout: stdout.unwrap(), stderr: stderr.unwrap()})
    }
}

#[derive(Debug)]
pub struct MapExitCode<F, O, E>(pub F)
where
    F: FnMut(i32) -> Result<O, E> + 'static,
    E: From<CommandExecutionError> + 'static,
    O: 'static;

impl<F, O, E> ReturnSettings for MapExitCode<F, O, E>
where
    F: FnMut(i32) -> Result<O, E>,
    E: From<CommandExecutionError>
{
    type Output = O;
    type Error = E;

    fn capture_stdout(&self) -> bool {
        false
    }

    fn capture_stderr(&self) -> bool {
        false
    }

    fn map_output(
        mut self: Box<Self>,
        _stdout: Option<Vec<u8>>,
        _stderr: Option<Vec<u8>>,
        exit_code: i32,
    ) -> Result<Self::Output, Self::Error> {
        (self.0)(exit_code)
    }
}

#[cfg(test)]
mod tests {
    #![allow(non_snake_case)]

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
                .with_exec_replacement_callback(move |_,_| {
                    Ok(ExecResult {
                        exit_code: 0,
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
                stdout in ".*".prop_map(Vec::from),
            ) {
                let stdout_ = stdout.clone();
                let out: CapturedStdout = Command::new("foo", ReturnStdout)
                    .with_exec_replacement_callback(move |_,_| {
                        Ok(ExecResult {
                            exit_code: 0,
                            stdout: Some(stdout_),
                            stderr: None
                        })
                    })
                    .run()
                    .unwrap();

                assert_eq!(out.stdout, stdout);
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
                stderr in ".*".prop_map(Vec::from)
            ) {
                let stderr_ = stderr.clone();
                let out: CapturedStderr = Command::new("foo", ReturnStderr)
                    .with_exec_replacement_callback(move |_,_| {
                        Ok(ExecResult {
                            exit_code: 0,
                            stdout: None,
                            stderr: Some(stderr_)
                        })
                    })
                    .run()
                    .unwrap();

                assert_eq!(out.stderr, stderr);
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
                stdout in ".*".prop_map(Vec::from),
                stderr in ".*".prop_map(Vec::from)
            ) {
                let stdout_ = stdout.clone();
                let stderr_ = stderr.clone();
                let out: CapturedStdoutAndErr = Command::new("foo", ReturnStdoutAndErr)
                    .with_exec_replacement_callback(move |_,_| {
                        Ok(ExecResult {
                            exit_code: 0,
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
            let res = Command
                ::new("foo", MapStdout(|out| -> Result<u32, Box<dyn std::error::Error>> {
                    Ok(String::from_utf8(out.stdout)?.parse()?)
                }))
                .with_exec_replacement_callback(|_,_| {
                    Ok(ExecResult {
                        exit_code: 0,
                        stdout: Some("3241".into()),
                        stderr: None
                    })
                })
                .run()
                .unwrap();

            assert_eq!(res, 3241u32);
        }

        #[test]
        fn mapping_stdout_to_a_result_can_fail() {
            Command
                ::new("foo", MapStdout(|out| -> Result<u32, Box<dyn std::error::Error>> {
                    Ok(String::from_utf8(out.stdout)?.parse()?)
                }))
                .with_exec_replacement_callback(|_,_| {
                    Ok(ExecResult {
                        exit_code: 0,
                        stdout: Some("abcd".into()),
                        stderr: None
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
            let res = Command
                ::new("foo", MapStderr(|err| -> Result<u32, Box<dyn std::error::Error>> {
                    Ok(String::from_utf8(err.stderr)?.parse()?)
                }))
                .with_exec_replacement_callback(|_,_| {
                    Ok(ExecResult {
                        exit_code: 0,
                        stderr: Some("3241".into()),
                        stdout: None
                    })
                })
                .run()
                .unwrap();

            assert_eq!(res, 3241u32);
        }

        #[test]
        fn mapping_stderr_to_a_result_can_fail() {
            Command
                ::new("foo", MapStderr(|err| -> Result<u32, Box<dyn std::error::Error>> {
                    Ok(String::from_utf8(err.stderr)?.parse()?)
                }))
                .with_exec_replacement_callback(|_,_| {
                    Ok(ExecResult {
                        exit_code: 0,
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
            let res = Command
                ::new("foo", MapStdoutAndErr(|cap| -> Result<(u32, u32), Box<dyn std::error::Error>> {
                    let out_res = String::from_utf8(cap.stdout)?.parse()?;
                    let err_res = String::from_utf8(cap.stderr)?.parse()?;
                    Ok((out_res, err_res))
                }))
                .with_exec_replacement_callback(|_,_| {
                    Ok(ExecResult {
                        exit_code: 0,
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
            Command
                ::new("foo", MapStdoutAndErr(|_| -> Result<u32, Box<dyn std::error::Error>> {
                    Err("yes this fails")?
                }))
                .with_exec_replacement_callback(|_,_| {
                    Ok(ExecResult {
                        exit_code: 0,
                        stdout: Some(Vec::new()),
                        stderr: Some(Vec::new())
                    })
                })
                .run()
                .unwrap_err();
        }
    }

    mod MapExitCode {
        use super::super::*;
        use crate::{Command, ExecResult};

        #[test]
        fn maps_stdout_to_a_result() {
            let res = Command
                ::new("foo", MapExitCode(|exit_code| -> Result<bool, Box<dyn std::error::Error>> {
                    Ok(exit_code == 3)
                }))
                .with_check_exit_code(false)
                .with_exec_replacement_callback(|_,_| {
                    Ok(ExecResult {
                        exit_code: 3,
                        ..Default::default()
                    })
                })
                .run()
                .unwrap();

            assert_eq!(res, true);
        }

        #[test]
        fn mapping_stdout_to_a_result_can_fail() {
            Command
                ::new("foo", MapExitCode(|_| -> Result<bool, Box<dyn std::error::Error>> {
                    Err("yes this fails")?
                }))
                .with_exec_replacement_callback(|_,_| {
                    Ok(ExecResult {
                        exit_code: 0,
                        ..Default::default()
                    })
                })
                .run()
                .unwrap_err();
        }
    }
}
