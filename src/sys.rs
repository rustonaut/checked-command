//! The default implementation for spawning a sub process
use std::{
    io,
    process::{self, ChildStderr, ChildStdin, ChildStdout, Output, Stdio},
};

use crate::{
    env::ApplyChildEnv,
    pipe::{InputPipeSetup, OutputPipeSetup},
    spawn::SpawnOptions,
    ExecResult, ExitStatus, OpaqueOsExitStatus,
};

/// Default spawn implementation.
pub(crate) fn spawn(
    options: SpawnOptions,
    capture_stdout: bool,
    capture_stderr: bool,
) -> Result<SysChild, io::Error> {
    let mut sys_cmd = process::Command::new(options.program);
    sys_cmd.args(options.arguments);

    options.env_builder.build_on(&mut sys_cmd);

    if let Some(wd_override) = &options.working_directory_override {
        sys_cmd.current_dir(wd_override);
    }

    if capture_stdout {
        sys_cmd.stdout(Stdio::piped());
    } else if let Some(stdout) = options.custom_stdout_setup {
        sys_cmd.stdout(output_pipe_setup_to_stdio(stdout));
    }

    if capture_stderr {
        sys_cmd.stderr(Stdio::piped());
    } else if let Some(stderr) = options.custom_stderr_setup {
        sys_cmd.stderr(output_pipe_setup_to_stdio(stderr));
    }

    if let Some(stdin) = options.custom_stdin_setup {
        sys_cmd.stdin(input_pipe_setup_to_stdio(stdin));
    }

    let child = sys_cmd.spawn()?;

    Ok(SysChild::new(child, capture_stdout, capture_stderr))
}

impl ApplyChildEnv for std::process::Command {
    fn do_inherit_env(&mut self, inherit_env: bool) {
        if !inherit_env {
            self.env_clear();
        }
    }

    fn set_var(&mut self, var: std::ffi::OsString, value: std::ffi::OsString) {
        self.env(var, value);
    }

    fn remove_var(&mut self, var: &std::ffi::OsStr) {
        self.env_remove(var);
    }

    fn explicitly_inherit_var(&mut self, name: std::ffi::OsString) {
        if let Some(value) = std::env::var_os(&name) {
            self.env(name, value);
        }
    }
}

fn output_pipe_setup_to_stdio(setup: OutputPipeSetup) -> Stdio {
    match setup {
        OutputPipeSetup::ExistingPipe(ep) => {
            #[cfg(unix)]
            use std::os::unix::io::{FromRawFd, IntoRawFd};
            #[cfg(windows)]
            use std::os::windows::io::{FromRawHandle, IntoRawHandle};
            //TODO change this, we don't want unsafe
            unsafe {
                #[cfg(unix)]
                return Stdio::from_raw_fd(ep.into_raw_fd());
                #[cfg(windows)]
                return Stdio::from_raw_handle(ep.into_raw_handle());
                #[cfg(not(any(unix, windows)))]
                panic!("currently pipe redirects are only supported on windows and unix")
            }
        }
        OutputPipeSetup::File(f) => Stdio::from(f),
        OutputPipeSetup::ChildStdin(p) => Stdio::from(p),
        OutputPipeSetup::Piped => Stdio::piped(),
        OutputPipeSetup::Null => Stdio::null(),
        OutputPipeSetup::Inherit => Stdio::inherit(),
    }
}

fn input_pipe_setup_to_stdio(setup: InputPipeSetup) -> Stdio {
    match setup {
        InputPipeSetup::ExistingPipe(ep) => {
            #[cfg(unix)]
            use std::os::unix::io::{FromRawFd, IntoRawFd};
            #[cfg(windows)]
            use std::os::windows::io::{FromRawHandle, IntoRawHandle};
            //TODO change this, we don't want unsafe
            unsafe {
                #[cfg(unix)]
                return Stdio::from_raw_fd(ep.into_raw_fd());
                #[cfg(windows)]
                return Stdio::from_raw_handle(ep.into_raw_handle());
                #[cfg(not(any(unix, windows)))]
                panic!("currently pipe redirects are only supported on windows and unix")
            }
        }
        InputPipeSetup::File(f) => Stdio::from(f),
        InputPipeSetup::ChildStdout(p) => Stdio::from(p),
        InputPipeSetup::ChildStderr(p) => Stdio::from(p),
        InputPipeSetup::Piped => Stdio::piped(),
        InputPipeSetup::Null => Stdio::null(),
        InputPipeSetup::Inherit => Stdio::inherit(),
    }
}

/// ChildHandle implementation returned by the `SpawnImp`
#[derive(Debug)]
pub(super) struct SysChild {
    child: std::process::Child,
    capture_stdout: bool,
    capture_stderr: bool,
}

impl SysChild {
    /// Creates a new `Box<dyn ChildHandle>`
    pub fn new(child: std::process::Child, capture_stdout: bool, capture_stderr: bool) -> Self {
        SysChild {
            child,
            capture_stdout,
            capture_stderr,
        }
    }

    pub(crate) fn take_stdout(&mut self) -> Option<ChildStdout> {
        if self.capture_stdout {
            None
        } else {
            self.child.stdout.take()
        }
    }

    pub(crate) fn take_stderr(&mut self) -> Option<ChildStderr> {
        if self.capture_stderr {
            None
        } else {
            self.child.stderr.take()
        }
    }

    pub(crate) fn take_stdin(&mut self) -> Option<ChildStdin> {
        self.child.stdin.take()
    }

    pub(crate) fn wait_with_output(mut self) -> Result<ExecResult, io::Error> {
        if !self.capture_stdout && self.child.stdout.is_some() {
            drop(self.child.stdout.take());
        }
        if !self.capture_stderr && self.child.stderr.is_some() {
            drop(self.child.stderr.take());
        }

        let Output {
            status,
            stdout,
            stderr,
        } = self.child.wait_with_output()?;

        Ok(ExecResult {
            exit_status: map_std_exit_status(status),
            stdout: if self.capture_stdout {
                Some(stdout)
            } else {
                debug_assert!(stdout.is_empty());
                None
            },
            stderr: if self.capture_stderr {
                Some(stderr)
            } else {
                debug_assert!(stderr.is_empty());
                None
            },
        })
    }
}

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
