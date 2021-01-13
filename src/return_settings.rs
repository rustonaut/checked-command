use super::{CommandExecutionError, ReturnSettings};

#[derive(Debug)]
pub struct ReturnExitSuccess;

impl ReturnSettings for ReturnExitSuccess {
    type Output = ();
    type Error = CommandExecutionError;

    fn capture_stdout(&self) -> bool {
        false
    }

    fn capture_stderr(&self) -> bool {
        false
    }

    fn map_output(
        &self,
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
        &self,
        stdout: Option<Vec<u8>>,
        _stderr: Option<Vec<u8>>,
        exit_code: i32,
    ) -> Result<Self::Output, Self::Error> {
        Ok(CapturedStdout {
            exit_code,
            stdout: stdout.unwrap(),
        })
    }
}

#[derive(Debug)]
pub struct CapturedStdout {
    pub exit_code: i32,
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
        &self,
        _stdout: Option<Vec<u8>>,
        stderr: Option<Vec<u8>>,
        exit_code: i32,
    ) -> Result<Self::Output, Self::Error> {
        Ok(CapturedStderr {
            exit_code,
            stderr: stderr.unwrap(),
        })
    }
}

#[derive(Debug)]
pub struct CapturedStderr {
    pub exit_code: i32,
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
        &self,
        stdout: Option<Vec<u8>>,
        stderr: Option<Vec<u8>>,
        exit_code: i32,
    ) -> Result<Self::Output, Self::Error> {
        Ok(CapturedStdoutAndErr {
            exit_code,
            stdout: stdout.unwrap(),
            stderr: stderr.unwrap(),
        })
    }
}

#[derive(Debug)]
pub struct CapturedStdoutAndErr {
    pub exit_code: i32,
    pub stdout: Vec<u8>,
    pub stderr: Vec<u8>,
}

#[cfg(test)]
mod tests {
    #![allow(non_snake_case)]

    mod ReturnExitSuccess {
        use super::super::*;
        use crate::Command;
        use proptest::prelude::*;

        #[test]
        fn captures_stdout_returns_true() {
            assert_eq!(ReturnExitSuccess.capture_stdout(), false);
        }

        #[test]
        fn captures_stderr_returns_false() {
            assert_eq!(ReturnExitSuccess.capture_stderr(), false);
        }

        proptest! {
            #[test]
            fn returns_nothing(
                stdout in ".*".prop_map(Vec::from),
                stderr in ".*".prop_map(Vec::from)
            ) {
                let stdout_ = stdout.clone();
                let stderr_ = stderr.clone();
                let _: () = Command::new("foo", ReturnExitSuccess)
                    .with_exec_replacement_callback(move |_| {
                        Ok(CapturedStdoutAndErr {
                            exit_code: 0,
                            stdout: stdout_,
                            stderr: stderr_
                        })
                    })
                    .run()
                    .unwrap();
            }
        }
    }

    mod ReturnStdout {
        use super::super::*;
        use crate::Command;
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
                stderr in ".*".prop_map(Vec::from)
            ) {
                let stdout_ = stdout.clone();
                let stderr_ = stderr.clone();
                let out: CapturedStdout = Command::new("foo", ReturnStdout)
                    .with_exec_replacement_callback(move |_| {
                        Ok(CapturedStdoutAndErr {
                            exit_code: 0,
                            stdout: stdout_,
                            stderr: stderr_
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
        use crate::Command;
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
                stdout in ".*".prop_map(Vec::from),
                stderr in ".*".prop_map(Vec::from)
            ) {
                let stdout_ = stdout.clone();
                let stderr_ = stderr.clone();
                let out: CapturedStderr = Command::new("foo", ReturnStderr)
                    .with_exec_replacement_callback(move |_| {
                        Ok(CapturedStdoutAndErr {
                            exit_code: 0,
                            stdout: stdout_,
                            stderr: stderr_
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
        use crate::Command;
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
                    .with_exec_replacement_callback(move |_| {
                        Ok(CapturedStdoutAndErr {
                            exit_code: 0,
                            stdout: stdout_,
                            stderr: stderr_
                        })
                    })
                    .run()
                    .unwrap();

                assert_eq!(out.stdout, stdout);
                assert_eq!(out.stderr, stderr);
            }
        }
    }
}
