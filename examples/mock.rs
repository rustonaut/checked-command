use std::error::Error;

use mapped_command::{Command, ExecResult, MapStdoutAndErrStrings, MapStdoutString};
use proptest::{prelude::*, test_runner::FileFailurePersistence};

/// Creates a command calling "foobar".
///
/// If magic is true the stdout is parsed as u32,
/// if it is false stdout and stderr are both parsed
/// as u32 and added together (yes I know that is stupid ;=) )
fn create_foobar_command(magic: bool) -> Command<u32, Box<dyn Error>> {
    // Command is only generic over the output/error type
    // so you can multiplex over different commands with
    // different mappings and arguments
    if magic {
        Command::new("foobar1", MapStdoutString(|stdout| Ok(stdout.parse()?)))
            .with_argument("--auto-sum")
    } else {
        Command::new(
            "foobar.legacy",
            // This could be a complex parsing function which you dine
            // *and test* separately! In some cases implementing a custom
            // ReturnSetting instance (like MapMyStuff) can also be useful.
            MapStdoutAndErrStrings(|captured| {
                let a: u32 = captured.stdout.parse()?;
                let b: u32 = captured.stderr.parse()?;
                Ok(a + b)
            }),
        )
    }
}

proptest! {
    #![proptest_config(ProptestConfig {
        failure_persistence: Some(Box::new(FileFailurePersistence::Off)),
        ..ProptestConfig::default()
    })]


    fn test_mocked_foobar(
        magic in any::<bool>(),
        stdout_num in any::<u32>(),
        stderr_num in any::<u32>()
    ) {
        prop_assume!(stdout_num as u64 + stderr_num as u64 <= u32::MAX as u64);

        // create it somewhere
        let num = create_foobar_command(magic)
            //mock the command and only return stdout/err if it would be captured
            .with_exec_replacement_callback(move |_cmd, ret_set| {
                let stdout = if ret_set.capture_stdout() {
                    Some(stdout_num.to_string().into())
                } else {
                    None
                };
                let stderr = if ret_set.capture_stderr() {
                    Some(stderr_num.to_string().into())
                } else {
                    None
                };
                Ok(ExecResult {
                    exit_status: 0.into(),
                    stdout,
                    stderr,
                })
            })
            .run()
            .unwrap();

        if magic {
            assert_eq!(num, stdout_num);
        } else {
            assert_eq!(num, stdout_num+stderr_num);
        }
    }
}

//overhead from proptest, normally we would use `proptest` but we do want to run main.
fn main() {
    test_mocked_foobar()
}
