use crate::{Command, CommandExecutionError, ExecResult, ExitCode, ReturnSettings};
use std::{io, process};

/// This method is a `exec_replacement_callback` but it actually executes the process.
pub(super) fn actual_exec_exec_replacement_callback<O, E>(
    cmd: Command<O, E>,
    return_settings: &dyn ReturnSettings<Output = O, Error = E>,
) -> Result<ExecResult, io::Error>
where
    E: From<CommandExecutionError>,
{
    let mut sys_cmd = process::Command::new(cmd.program());
    sys_cmd.args(cmd.arguments());

    // This might not be the fasted thing, but it is the most consistent thing
    // because now we always will have the environment variables returned by
    // `.create_expected_env_iter()` *which we can  properly test to work correctly*.
    sys_cmd.env_clear();
    sys_cmd.envs(cmd.create_expected_env_iter());

    if let Some(wd_override) = cmd.working_directory_override() {
        sys_cmd.current_dir(wd_override);
    }

    let capture_stdout = return_settings.capture_stdout();
    let capture_stderr = return_settings.capture_stderr();

    if capture_stdout {
        sys_cmd.stdout(process::Stdio::piped());
    }

    if capture_stderr {
        sys_cmd.stderr(process::Stdio::piped());
    }

    let child = sys_cmd.spawn()?;

    // `wait_with_output` will only parse stdout/stderr if it was setup with Stdio::piped()
    // As we only setup `Stdio::piped()` if we need capturing this only captures when we want
    // it to capture. (Non captured stdout/stderr will produce an empty vector).
    let process::Output {
        stdout,
        stderr,
        status,
    } = child.wait_with_output()?;

    let exit_code = if let Some(code) = status.code() {
        ExitCode::Some(code)
    } else {
        ExitCode::ProcessTerminatedBeforeExiting
    };

    let stdout = if capture_stdout {
        Some(stdout)
    } else {
        debug_assert!(stdout.is_empty());
        None
    };

    let stderr = if capture_stderr {
        Some(stderr)
    } else {
        debug_assert!(stderr.is_empty());
        None
    };

    Ok(ExecResult {
        exit_code,
        stdout,
        stderr,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ReturnStderr, ReturnStdout};

    #[cfg(target_os = "linux")]
    #[test]
    fn with_arguments() {
        let cap = Command::new("echo", ReturnStdout)
            .with_arguments(vec!["hy", "there"])
            .run()
            .unwrap();

        assert_eq!(String::from_utf8_lossy(&*cap.stdout), "hy there\n");
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn can_run_failing_program_without_failing() {
        let cap = Command::new("cp", ReturnStderr)
            .with_arguments(vec!["/"])
            .with_expected_exit_code(1)
            .run()
            .unwrap();

        assert!(!cap.stderr.is_empty());
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn with_env() {
        let out = Command::new("bash", ReturnStdout)
            .with_arguments(&["-c", "echo $CHECKED_COMMAND_ENV_TEST"])
            .with_inherit_env(false)
            .with_env_update("CHECKED_COMMAND_ENV_TEST", "yoyo")
            .run()
            .unwrap();

        assert_eq!(String::from_utf8_lossy(&out.stdout), "yoyo\n");
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn with_working_dir() {
        let out = Command::new("pwd", ReturnStdout)
            .with_working_directory_override(Some("/"))
            .run()
            .unwrap();

        assert_eq!(String::from_utf8_lossy(&out.stdout), "/\n");
    }
}
