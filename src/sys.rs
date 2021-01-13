use crate::{Command, CommandExecutionError, ExecResult, ExitCode, ReturnSettings};
use std::{io, process};

/// This method is a `exec_replacement_callback` but it actually executes the process.
pub(super) fn actual_exec_exec_replacement_callback<O, E>(
    cmd: Command<O, E>,
    return_settings: &dyn ReturnSettings<Output=O, Error=E>
) -> Result<ExecResult, io::Error>
where
    E: From<CommandExecutionError>
{

    let mut sys_cmd = process::Command::new(cmd.program());
    sys_cmd.args(cmd.arguments());
    sys_cmd.envs(cmd.env_updates());
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
    let process::Output { stdout, stderr, status} = child.wait_with_output()?;

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
        stderr
    })
}