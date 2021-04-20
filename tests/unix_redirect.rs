#![cfg(unix)]
//! Integration tests for the redirect functionality

use mapped_command::prelude::*;
use std::{fs, path::PathBuf};

#[test]
fn redirect_into_file() {
    let mut path = PathBuf::from(file!());
    path.set_extension("test_file");
    let file = fs::File::create(&path).unwrap();

    Command::new("bash", ReturnNothing)
        .with_arguments(&["-c", "echo abcde"])
        .with_custom_stdout_setup(file)
        .run()
        .unwrap();

    let data = fs::read_to_string(&path).unwrap();
    assert_eq!(data, "abcde\n");

    fs::remove_file(path).unwrap();
}

#[ignore = "currently not supported"]
#[test]
fn redirect_from_raw_fd() {
    panic!("not yet implemented");
    // let mut path = PathBuf::from(file!());
    // path.set_extension("test_file2");
    // let file = fs::File::create(&path).unwrap();
    // let redirect = unsafe { RawPipeSetup::from_raw_fd(file.into_raw_fd()) };

    // Command::new("bash", ReturnNothing)
    //     .with_arguments(&["-c", "echo abcde"])
    //     .with_custom_stdout_setup(redirect)
    //     .run()
    //     .unwrap();

    // let data = fs::read_to_string(&path).unwrap();
    // assert_eq!(data, "abcde\n");

    // fs::remove_file(path).unwrap();
}

mod connect_input_and_output_of_two_sub_processes {
    use std::io::Write;

    use super::*;

    #[test]
    fn out_to_in() {
        let mut child = Command::new("bash", ReturnNothing)
            .with_arguments(&["-c", r#"sleep 0.1; read line; echo "<$line>""#])
            .with_custom_stdin_setup(Piped)
            .with_custom_stdout_setup(Piped)
            .spawn()
            .unwrap();

        let child_output = child.take_stdout().unwrap();
        let mut child_input = child.take_stdin().unwrap();

        let other_child = Command::new("bash", ReturnStdoutString)
            .with_arguments(&["-c", r#"read line; echo ".$line.""#])
            .with_custom_stdin_setup(child_output)
            .spawn()
            .unwrap();

        child_input.write_all(b"hyhohe\n").unwrap();
        child_input.flush().unwrap();

        let received = other_child.wait().unwrap();
        let () = child.wait().unwrap();

        assert_eq!(received, ".<hyhohe>.\n")
    }

    #[test]
    fn in_to_out() {
        let mut child = Command::new("bash", ReturnStdoutString)
            .with_arguments(&["-c", r#"read line; echo ".$line.""#])
            .with_custom_stdin_setup(Piped)
            .spawn()
            .unwrap();

        let child_input = child.take_stdin().unwrap();

        let mut other_child = Command::new("bash", ReturnNothing)
            .with_arguments(&["-c", r#"sleep 0.1; read line; echo "<$line>""#])
            .with_custom_stdout_setup(child_input)
            .with_custom_stdin_setup(Piped)
            .spawn()
            .unwrap();

        let mut child_input = other_child.take_stdin().unwrap();

        child_input.write_all(b"hyhohe\n").unwrap();
        child_input.flush().unwrap();

        let received = child.wait().unwrap();
        let () = other_child.wait().unwrap();

        assert_eq!(received, ".<hyhohe>.\n")
    }
}
