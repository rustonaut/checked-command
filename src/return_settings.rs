use super::{ReturnSettings, CommandExecutionError};

#[derive(Debug)]
pub struct ReturnExitSuccess;

impl ReturnSettings for ReturnExitSuccess {
    type Output = ();
    type Error = CommandExecutionError;

    fn map_output(
        &self,
        _stdout: Option<Vec<u8>>,
        _stderr: Option<Vec<u8>>,
        _exit_code: i32
    ) -> Result<Self::Output, Self::Error> {
        Ok(())
    }

}


#[derive(Debug)]
pub struct ReturnStdout;

impl ReturnSettings for ReturnStdout {
    type Output = CapturedStdout;
    type Error = CommandExecutionError;

    fn map_output(
        &self,
        stdout: Option<Vec<u8>>,
        _stderr: Option<Vec<u8>>,
        exit_code: i32
    ) -> Result<Self::Output, Self::Error> {
        Ok(CapturedStdout {
            exit_code,
            stdout: stdout.unwrap()
        })
    }
}

#[derive(Debug)]
pub struct CapturedStdout {
    pub exit_code: i32,
    pub stdout: Vec<u8>
}


#[derive(Debug)]
pub struct ReturnStderr;

impl ReturnSettings for ReturnStderr {
    type Output = CapturedStderr;
    type Error = CommandExecutionError;

    fn map_output(
        &self,
        _stdout: Option<Vec<u8>>,
        stderr: Option<Vec<u8>>,
        exit_code: i32
    ) -> Result<Self::Output, Self::Error> {
        Ok(CapturedStderr {
            exit_code,
            stderr: stderr.unwrap()
        })
    }
}

#[derive(Debug)]
pub struct CapturedStderr {
    pub exit_code: i32,
    pub stderr: Vec<u8>
}

#[derive(Debug)]
pub struct ReturnStdoutAndErr;

impl ReturnSettings for ReturnStdoutAndErr {
    type Output = CapturedStdoutAndErr;
    type Error = CommandExecutionError;

    fn map_output(
        &self,
        stdout: Option<Vec<u8>>,
        stderr: Option<Vec<u8>>,
        exit_code: i32
    ) -> Result<Self::Output, Self::Error> {
        Ok(CapturedStdoutAndErr {
            exit_code,
            stdout: stdout.unwrap(),
            stderr: stderr.unwrap()
        })
    }
}

#[derive(Debug)]
pub struct CapturedStdoutAndErr {
    pub exit_code: i32,
    pub stdout: Vec<u8>,
    pub stderr: Vec<u8>
}


#[cfg(test)]
mod tests {
    #![allow(non_snake_case)]

    mod ReturnStdout {
        use proptest::prelude::*;
        use super::super::*;
        use crate::Command;

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

    mod ReturnStdoutAndErr {
        use proptest::prelude::*;
        use super::super::*;
        use crate::Command;

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