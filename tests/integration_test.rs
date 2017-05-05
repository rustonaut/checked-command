extern crate checked_command;

#[cfg(feature="enable_integration_tests")]
mod integration {
    use std::io::{Write, Read};
    use std::process::Stdio;
    use std::path::PathBuf;
    use std::env::temp_dir;

    use checked_command::{CheckedCommand, Error};

    fn feedback_path() -> PathBuf {
        PathBuf::from(concat!(env!("OUT_DIR"),"/feedback"))
    }

    #[test]
    fn stdio() {
        const LINE: &str = "random line 123\n";

        let mut child = CheckedCommand::new(feedback_path())
            .arg("echo_line")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .unwrap();

        write!(child.stdin().as_mut().unwrap(), "{}", LINE).unwrap();
        let mut buf = String::new();
        //this only works because we
        child.stdout().as_mut().unwrap().read_to_string(&mut buf).unwrap();
        assert_eq!(LINE, buf);
        buf.clear();
        child.stderr().as_mut().unwrap().read_to_string(&mut buf).unwrap();
        assert_eq!(LINE.to_string() + LINE, buf);
    }


    #[test]
    fn output_with_err() {
        let result = CheckedCommand::new(feedback_path())
            .arg("output_with_err")
            .stdout(Stdio::piped())
            .output();

        match result {
            Err(Error::Failure(ex, Some(output))) => {
                assert_eq!(Some(100), ex.code());
                assert_eq!("test123\n", String::from_utf8_lossy(&*output.stdout));
            },
            e => panic!("unexpected result {:?}", e)
        }
    }

    #[test]
    fn wait_with_output() {
        let child = CheckedCommand::new(feedback_path())
            .arg("print")
            .stdout(Stdio::piped())
            .spawn()
            .unwrap();

        match child.wait_with_output() {
            Ok(output) => {
                assert_eq!("test123\n", String::from_utf8_lossy(&*output.stdout));
            },
            e => panic!("unexpected result {:?}", e)
        }
    }

    #[test]
    #[cfg(feature = "process_try_wait")]
    fn try_wait() {
        let mut child = CheckedCommand::new(feedback_path())
            .arg("noop")
            .spawn()
            .expect("spawn failed");

        let mut res = child.try_wait();
        while let &Ok(false) = &res {
            res = child.try_wait();
        }
        match child.try_wait() {
            Ok(true) => {},
            e => panic!("unexpected result {:?}", e)
        }
    }

    #[test]
    #[cfg(unix)]
    fn kill_wait() {
        let mut child = CheckedCommand::new(feedback_path())
            .arg("hang")
            .spawn()
            .expect("spawn failed");

        child.kill().expect("kill failed");

        match child.wait() {
            Err(Error::Failure(ex, None)) => {
                assert_eq!(None, ex.code());
            }
            e => panic!("unexpected result {:?}", e)
        }
    }

    #[test]
    #[cfg(feature = "process_try_wait")]
    #[cfg(unix)]
    fn kill_try_wait() {
        let mut child = CheckedCommand::new(feedback_path())
            .arg("hang")
            .spawn()
            .expect("spawn failed");

        match child.try_wait() {
            Ok(false) => {},
            e => panic!("unexpected result {:?}", e)
        }

        child.kill().expect("kill failed");

        match child.wait() {
            Err(Error::Failure(ex, None)) => {
                assert_eq!(None, ex.code());
            }
            e => panic!("unexpected result {:?}", e)
        }
    }


    #[test]
    fn check() {
        let current_dir = temp_dir();
        let mut cmd = CheckedCommand::new(feedback_path());
        cmd.arg("check")
            .arg(&current_dir)
            .args(&["one", "two", "three"])
            .current_dir(&current_dir)
            .stdout(Stdio::piped())
            .stdin(Stdio::piped())
            .env_clear()
            .env("key1", "one");

        #[cfg(feature = "command_envs")]
        {
            cmd.envs(vec![("key2", "two"), ("key3", "three")]);
        }
        #[cfg(not(feature = "command_envs"))]
        {
            cmd.env("key2", "two").env("key3", "three");
        }

        match cmd.output() {
            Ok(_) => {},
            Err(Error::Io(io_err)) => panic!(io_err),
            Err(Error::Failure(ex, output)) => {
                panic!("check failed with exit code {:?} and output\n{}",
                       ex.code(),
                       String::from_utf8_lossy(&*output.unwrap().stderr)
                )
            }
        }
    }

    #[test]
    fn noop_err() {
        let result = CheckedCommand::new(feedback_path())
            .arg("noop_err")
            .status();

        match result {
            Err(Error::Failure(ex, None)) => {
                assert_eq!(Some(113), ex.code());
            },
            e => panic!("unexpected result {:?}", e)
        }
    }
}