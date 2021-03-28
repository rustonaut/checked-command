use crate::{ExecResult, ExitStatus, OpaqueOsExitStatus, SpawnOptions};
use std::{io, process};

/// This method is a `exec_replacement_callback` but it actually executes the process.
pub(super) fn actual_exec_exec_replacement_callback(
    options: SpawnOptions,
) -> Result<ExecResult, io::Error> {
    let mut sys_cmd = process::Command::new(&options.program);
    sys_cmd.args(&options.arguments);

    // This might not be the fasted thing, but it is the most consistent thing
    // because now we always will have the environment variables returned by
    // `.create_expected_env_iter()` *which we can  properly test to work correctly*.
    sys_cmd.env_clear();
    sys_cmd.envs(options.create_expected_env_iter());

    if let Some(wd_override) = options.working_directory_override {
        sys_cmd.current_dir(wd_override);
    }

    if let Some(stdout) = options.override_stdout {
        sys_cmd.stdout(stdout);
    }

    if let Some(stderr) = options.override_stderr {
        sys_cmd.stderr(stderr);
    }

    let child = sys_cmd.spawn()?;

    let does_capture_stdout = child.stdout.is_some();
    let does_capture_stderr = child.stderr.is_some();

    // `wait_with_output` will only parse stdout/stderr if it was setup with Stdio::piped()
    // As we only setup `Stdio::piped()` if we need capturing this only captures when we want
    // it to capture. (Non captured stdout/stderr will produce an empty vector).
    let process::Output {
        stdout,
        stderr,
        status: exit_status,
    } = child.wait_with_output()?;

    let exit_status = map_std_exit_status(exit_status);

    let stdout = if does_capture_stdout {
        Some(stdout)
    } else {
        debug_assert!(stdout.is_empty());
        None
    };

    let stderr = if does_capture_stderr {
        Some(stderr)
    } else {
        debug_assert!(stderr.is_empty());
        None
    };

    Ok(ExecResult {
        exit_status,
        stdout,
        stderr,
    })
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
    use crate::{Command, ReturnStderr, ReturnStdout};
    use proptest::prelude::*;

    #[cfg(target_os = "linux")]
    #[test]
    fn with_arguments() {
        let cap = Command::new("echo", ReturnStdout)
            .with_arguments(vec!["hy", "there"])
            .run()
            .unwrap();

        assert_eq!(String::from_utf8_lossy(&*cap), "hy there\n");
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn can_run_failing_program_without_failing() {
        let cap = Command::new("cp", ReturnStderr)
            .with_arguments(vec!["/"])
            .with_expected_exit_status(1)
            .run()
            .unwrap();

        assert!(!cap.is_empty());
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn with_env() {
        let out = Command::new("bash", ReturnStdout)
            .with_arguments(&["-c", "echo $MAPPED_COMMAND_ENV_TEST"])
            .with_inherit_env(false)
            .with_env_update("MAPPED_COMMAND_ENV_TEST", "yoyo")
            .run()
            .unwrap();

        assert_eq!(String::from_utf8_lossy(&out), "yoyo\n");
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn with_working_dir() {
        let out = Command::new("pwd", ReturnStdout)
            .with_working_directory_override(Some("/"))
            .run()
            .unwrap();

        assert_eq!(String::from_utf8_lossy(&out), "/\n");
    }

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
