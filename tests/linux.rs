#![cfg(target_os = "linux")]

use mapped_command::{
    CapturedStdoutAndErrStrings, Command, ReturnStderr, ReturnStdout, ReturnStdoutAndErrStrings,
};

#[test]
fn arguments_are_passed_in() {
    let cap = Command::new("echo", ReturnStdout)
        .with_arguments(vec!["hy", "there"])
        .run()
        .unwrap();

    assert_eq!(String::from_utf8_lossy(&*cap), "hy there\n");
}

#[test]
fn can_run_failing_program_without_failing() {
    let cap = Command::new("cp", ReturnStderr)
        .with_arguments(vec!["/"])
        .with_expected_exit_status(1)
        .run()
        .unwrap();

    assert!(!cap.is_empty());
}

#[test]
fn env_is_set() {
    // Note we already tested if we create the right env, we just didn't test
    // if we do actually apply it, so this test is good enough for now.
    let out = Command::new("bash", ReturnStdout)
        .with_arguments(&["-c", "echo $MAPPED_COMMAND_ENV_TEST"])
        .with_inherit_env(false)
        .with_env_update("MAPPED_COMMAND_ENV_TEST", "yoyo")
        .run()
        .unwrap();

    assert_eq!(String::from_utf8_lossy(&out), "yoyo\n");
}
#[test]
fn working_dir_is_set() {
    let out = Command::new("pwd", ReturnStdout)
        .with_working_directory_override(Some("/"))
        .run()
        .unwrap();

    assert_eq!(String::from_utf8_lossy(&out), "/\n");
}

#[test]
fn can_capture_stdout_and_stderr_concurrent() {
    // This test is a bit iffy.
    // Basically what we want to test is that the implementation (more or less) parses stdout and stderr concurrently.
    // Through what we test is that it doesn't parse stdout and then stderr (or the other way around). Which isn't
    // exactly the same.
    //
    // The problem with testing that is that stdout and stderr are buffered to some degree and we can't use
    // stdin feedback loops here. So we need to make sure to write more the is ever buffered. Now we can
    // use stdbuf to set the buffer to 0. Sadly this isn't *actually* zero, it just means that output written
    // to stdout/stderr is more or less directly flushed. *But* the pipe mechanism itself does some form of
    // buffering (and in general this is grate). And we have no idea how big that is, so we write *more then*
    // 64KiB which happens to work.
    //
    // Now this still means this is a bad test for 2 reasons:
    // - If we don't write enough it silently will always succeed ...
    // - If the test fails it hangs for ever (TODO fix this by using timeout and kill).
    //
    // Still given how this library is **currently** designed this is more or less testing
    // `std::process::Child::wait_with_output()` so I guess what we do here is good enough *for now*.
    let CapturedStdoutAndErrStrings { stdout, stderr } =
        Command::new("stdbuf", ReturnStdoutAndErrStrings)
            .with_arguments(&[
                "-o0",
                "-e0",
                "bash",
                "-c",
                r#"
                function output_byte_stream() {
                    local COUNT=$1
                    local PIPE=$2
                    while ((COUNT-- > 0)); do
                        echo -n $((COUNT % 9)) >&$PIPE;
                    done
                    echo E >&$PIPE
                }
                output_byte_stream 65546 1
                output_byte_stream 65546 2
                echo stdout
                echo stderr >&2
                "#,
            ])
            .run()
            .unwrap();

    assert!(stdout.ends_with("stdout\n"));
    assert!(stderr.ends_with("stderr\n"));
    assert_eq!(stdout.len(), 65555);
    assert_eq!(stderr.len(), 65555);
}
