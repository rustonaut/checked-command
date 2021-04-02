use crate::{
    ChildHandle, ExecResult, ExitStatus, NoRawRepr, OpaqueOsExitStatus, ProcessInput,
    ProcessOutput, ProcessPipeSetting, RawPipeRepr, SpawnOptions,
};
use std::{
    io,
    process::{self, Output},
};

/// Default implementation of [`crate::SpawnImpl`] which internally uses [`std::process::Command`].
#[derive(Debug)]
pub struct SpawnImpl;

impl crate::SpawnImpl for SpawnImpl {
    fn spawn(
        &self,
        options: SpawnOptions,
        capture_stdout: bool,
        capture_stderr: bool,
    ) -> Result<Box<dyn ChildHandle>, io::Error> {
        let mut sys_cmd = process::Command::new(&options.program);

        // This might not be the fasted thing, but it is the most consistent thing
        // because now we always will have the environment variables returned by
        // `.create_expected_env_iter()` *which we can  properly test to work correctly*.
        sys_cmd.env_clear();
        sys_cmd.envs(options.create_expected_env_iter());

        sys_cmd.args(options.arguments);

        if let Some(wd_override) = &options.working_directory_override {
            sys_cmd.current_dir(wd_override);
        }

        if capture_stdout {
            sys_cmd.stdout(ProcessPipeSetting::Piped);
        } else if let Some(stdout) = options.custom_stdout_setup {
            sys_cmd.stdout(stdout);
        }

        if capture_stderr {
            sys_cmd.stderr(ProcessPipeSetting::Piped);
        } else if let Some(stderr) = options.custom_stderr_setup {
            sys_cmd.stderr(stderr);
        }

        let child = sys_cmd.spawn()?;

        Ok(Box::new(child) as _)
    }
}

impl ChildHandle for std::process::Child {
    fn take_stdout(&mut self) -> Option<Box<dyn ProcessOutput>> {
        self.stdout.take().map(|p| Box::new(p) as _)
    }

    fn take_stderr(&mut self) -> Option<Box<dyn ProcessOutput>> {
        self.stderr.take().map(|p| Box::new(p) as _)
    }

    fn take_stdin(&mut self) -> Option<Box<dyn ProcessInput>> {
        self.stdin.take().map(|p| Box::new(p) as _)
    }

    fn wait_with_output(self: Box<Self>) -> Result<ExecResult, io::Error> {
        let captures_stdout = self.stdout.is_some();
        let captures_stderr = self.stderr.is_some();

        let Output {
            status,
            stdout,
            stderr,
        } = std::process::Child::wait_with_output(*self)?;

        Ok(ExecResult {
            exit_status: map_std_exit_status(status),
            stdout: if captures_stdout {
                Some(stdout)
            } else {
                debug_assert!(stdout.is_empty());
                None
            },
            stderr: if captures_stderr {
                Some(stderr)
            } else {
                debug_assert!(stderr.is_empty());
                None
            },
        })
    }
}

macro_rules! impl_raw_pipe_repr {
    ($name:ty) => {
        impl RawPipeRepr for $name {
            #[cfg(unix)]
            fn try_as_raw_fd(&self) -> Result<std::os::unix::prelude::RawFd, NoRawRepr> {
                use std::os::unix::io::AsRawFd;
                Ok(self.as_raw_fd())
            }
            #[cfg(unix)]
            fn try_into_raw_fd(
                self: Box<Self>,
            ) -> Result<std::os::unix::prelude::RawFd, NoRawRepr> {
                use std::os::unix::io::IntoRawFd;
                Ok((*self).into_raw_fd())
            }

            //FIXME due to test limitations feat
            #[cfg(windows)]
            fn try_as_raw_handle(&self) -> Result<std::os::windows::io::RawHandle, NoRawRepr> {
                use std::os::windows::io::AsRawHandle;
                Ok(self.as_raw_handle())
            }
            #[cfg(windows)]
            fn try_into_raw_handle(
                self: Box<Self>,
            ) -> Result<std::os::windows::io::RawHandle, NoRawRepr> {
                use std::os::windows::io::IntoRawHandle;
                Ok((*self).into_raw_handle())
            }
        }
    };
}

impl ProcessOutput for std::process::ChildStdout {}

impl_raw_pipe_repr!(std::process::ChildStdout);

impl ProcessOutput for std::process::ChildStderr {}

impl_raw_pipe_repr!(std::process::ChildStderr);

impl ProcessInput for std::process::ChildStdin {}

impl_raw_pipe_repr!(std::process::ChildStdin);

fn map_std_exit_status(exit_status: std::process::ExitStatus) -> ExitStatus {
    if let Some(code) = exit_status.code() {
        let code = cast_exit_code(code);
        ExitStatus::Code(code)
    } else {
        #[cfg(unix)]
        {
            use std::os::unix::process::ExitStatusExt;
            // All unixes have a exit signal number if they didn't exit normally
            //
            // Normally `WIFEXITED(s) != WIFSIGNALED(s)` should hold but at least
            // with some targets (OpenBSD) they can be both `false` if the process was stopped
            // BUT wait should only report that way on stopped processes if `waitpid`
            // was called with the `WUNTRACED` flag, which it should not be.
            //
            // So we should be able to call `unwrap`. But for the (I think but can't guarantee
            // unreachable) case of still ending up with `None` we just pretend we saw the
            // signal `0x7F` which is normally used to indicate stopped processes. I just
            // really really don't want to maybe kill a production system because on some
            // unexpected unix like system you can run into this if the subprocess was
            // terminated in a unlikely fashion.
            let signal = exit_status.signal().unwrap_or(0x7F);
            let signal = signal;
            return OpaqueOsExitStatus::from_signal_number(signal).into();
        }
        #[cfg(not(unix))]
        unreachable!("run on unsupported target family, please open issue on github");
    }
}

fn cast_exit_code(code: i32) -> i64 {
    #[cfg(windows)]
    {
        windows_cast_exit_code(code)
    }
    #[cfg(not(windows))]
    {
        code as i64
    }
}

#[cfg(any(windows, test))]
fn windows_cast_exit_code(code: i32) -> i64 {
    code as u32 as i64
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    //HINT: See tests/linux.rs for integration tests

    #[test]
    fn special_windows_exit_code_cast() {
        assert_eq!(windows_cast_exit_code(-1), u32::MAX as i64);
    }

    #[test]
    #[cfg(unix)]
    fn on_non_windows_cast_exit_code_normally() {
        assert_eq!(cast_exit_code(-1), -1);
    }

    #[test]
    #[cfg(not(unix))]
    fn on_windows_cast_exit_code_specially() {
        assert_eq!(cast_exit_code(-1), u32::MAX as i64);
    }

    proptest! {
        #[test]
        #[cfg(windows)]
        fn mapping_windows_exit_code_to_status(
            exit_code in any::<u32>()
        ) {
            use std::os::windows::process::ExitStatusExt;
            let exit_status = std::process::ExitStatus::from_raw(exit_code);
            let exit_status = map_std_exit_status(exit_status);
            assert_eq!(exit_status, ExitStatus::Code(exit_code.into()));
        }

        #[test]
        #[cfg(unix)]
        fn mapping_unix_exit_code_to_status(
            raw_status in any::<i32>()
        ) {
            use std::os::unix::process::ExitStatusExt;
            let exit_status = std::process::ExitStatus::from_raw(raw_status);
            let exit_status = map_std_exit_status(exit_status);
            if libc::WIFEXITED(raw_status) {
                let code= libc::WEXITSTATUS(raw_status);
                assert_eq!(exit_status, ExitStatus::Code(code as i64));
            } else {
                //Note: raw_status might be !WIFEXITED, !WIFSIGNALED and !WIFSTOPPED and !WIFCONTINUED
                //      if it has a signal as if stopped and the coredumped flag set. So we can't do an
                //      assert(libc::WIFSIGNALED(raw_status) || libc::WIFSTOPPED(raw_status))
                let signal = libc::WTERMSIG(raw_status);
                assert_eq!(exit_status, ExitStatus::OsSpecific(OpaqueOsExitStatus::from_signal_number(signal).into()));
            }
        }
    }
}
