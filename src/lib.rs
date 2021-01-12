use std::{
    ffi::{OsStr, OsString},
    io
};
use thiserror::Error;
pub use self::return_settings::*;

mod return_settings;

//TODO env
//TODO working dir
//TODO with additional argument (just one add arg)
//TODO exit code is Option
//TODO expect non zero exit code
//TODO allow not expecting anything about the exit code
//TODO consume result settings on map_output
//TODO return setting indicate what needs to be captured
//TODO ↑ is also checked in the callback!
//TODO types for MapStdout, MapStderr, MapStdoutAndErr which take a closure

pub struct Command<Output, Error>
where
    Output: 'static,
    Error: From<CommandExecutionError> + 'static
{
    program: OsString,
    arguments: Vec<OsString>,
    //FIXME use CapturedStdout, CapturedStderr
    return_settings: Option<Box<dyn ReturnSettings<Output=Output, Error=Error>>>,
    run_callback: Option<Box<dyn FnOnce(Self) -> Result<CapturedStdoutAndErr, io::Error>>>,
}

impl<Output, Error> Command<Output, Error>
where
    Output: 'static,
    Error: From<CommandExecutionError> + 'static
{

    /// Create a new command.
    pub fn new(program: impl Into<OsString>, return_settings: impl ReturnSettings<Output=Output, Error=Error>)  -> Self {
        Command {
            program: program.into(),
            return_settings: Some(Box::new(return_settings) as _),
            arguments: Vec::new(),
            run_callback: None,
        }
    }

    /// Return the program the command will run.
    pub fn program(&self) -> &OsStr {
        &*self.program
    }

    /// Returns the arguments passed the the program when run.
    pub fn arguments(&self) -> &[OsString] {
        &self.arguments
    }

    /// Returns this command with all arguments replaced with the new arguments
    pub fn with_arguments<T>(mut self, args: impl IntoIterator<Item=T>) -> Self
    where
        T: Into<OsString>
    {
        self.arguments = args.into_iter().map(|v| v.into()).collect();
        self
    }

    /// Run the command, blocking until completion
    pub fn run(mut self) -> Result<Output, Error> {
        let return_settings = self.return_settings.take()
            .expect("run recursively called in exec replacing callback");
        let result = if let Some(callback) = self.run_callback.take() {
            callback(self)
        } else {
            todo!()
        };

        let result = result.map_err(|err| CommandExecutionError::SpawningProcessFailed(err))?;

        if result.exit_code != 0 {
            Err(Error::from(CommandExecutionError::UnexpectedExitCode { got: result.exit_code, expected: 0 }))
        } else {
            return_settings.map_output(
                Some(result.stdout),
                Some(result.stderr),
                result.exit_code
            )
        }
    }

    /// Sets a callback which is called instead of executing the command when running the command.
    pub fn with_exec_replacement_callback(
        mut self,
        callback: impl FnOnce(Self) -> Result<CapturedStdoutAndErr, io::Error> + 'static
    ) -> Self {
        self.run_callback = Some(Box::new(callback));
        self
    }
}

/// Trait used to configure what [`Command::run()`] returns.
pub trait ReturnSettings: 'static {
    type Output: 'static;
    type Error: 'static;

    fn map_output(
        &self,
        stdout: Option<Vec<u8>>,
        stderr: Option<Vec<u8>>,
        exit_code: i32
    ) -> Result<Self::Output, Self::Error>;
}

#[derive(Debug, Error)]
pub enum CommandExecutionError {
    #[error("Spawning process failed: {}", _0)]
    SpawningProcessFailed(io::Error),

    #[error("Unexpected exit code. Got: {got}, Expected: {expected}")]
    UnexpectedExitCode { got: i32, expected: u32 }
}




#[cfg(test)]
mod tests {
    use super::*;
    use thiserror::Error;
    use std::{
        collections::HashSet,
        rc::Rc,
        cell::RefCell
    };
    use proptest::prelude::*;


    #[test]
    fn comp_can_be_created_using_str_string_osstr_or_osstring() {
        Command::new("ls", ReturnExitSuccess);
        Command::new("ls".to_owned(), ReturnExitSuccess);
        Command::new(OsString::from("ls"), ReturnExitSuccess);
        Command::new(OsStr::new("ls"), ReturnExitSuccess);
    }

    #[test]
    fn default_arguments_to_empty_list() {
        let cmd = Command::new("dos", ReturnExitSuccess);
        assert_eq!(cmd.arguments(), &[] as &[OsString])
    }

    #[test]
    fn comp_arguments_can_be_set_from_iterables() {
        Command::new("foo", ReturnExitSuccess).with_arguments(Vec::<OsString>::new());
        Command::new("foo", ReturnExitSuccess).with_arguments(HashSet::<OsString>::new());
        Command::new("foo", ReturnExitSuccess).with_arguments(&[] as &[OsString]);
    }

    #[test]
    fn comp_when_creating_command_all_capture_modes_can_be_used() {
        Command::new("foo", ReturnExitSuccess);
        Command::new("foo", ReturnStdout);
        Command::new("foo", ReturnStderr);
        Command::new("foo", ReturnStdoutAndErr);
    }

    #[test]
    fn run_can_lead_to_and_io_error() {
        let res = Command::new("foo", ReturnExitSuccess)
            .with_exec_replacement_callback(|_| {
                Err(io::Error::new(io::ErrorKind::Other, "random"))
            })
            .run();

        res.unwrap_err();
    }

    #[test]
    fn return_no_error_if_the_command_has_zero_exit_status() {
        let res = Command::new("foo", ReturnExitSuccess)
            .with_exec_replacement_callback(move |_| {
                Ok(CapturedStdoutAndErr {
                    exit_code: 0,
                    stdout: Vec::new(),
                    stderr: Vec::new()
                })
            })
            .run();

        res.unwrap();
    }

    #[test]
    fn comp_command_must_only_be_generic_over_the_output() {
        if false {
            let mut _cmd = Command::new("foo", ReturnExitSuccess);
            _cmd = Command::new("foo", ReturnExitSuccessAlt);
        }

        //---
        struct ReturnExitSuccessAlt;
        impl ReturnSettings for ReturnExitSuccessAlt {
            type Output = ();
            type Error = CommandExecutionError;
            fn map_output(
                &self,
                _stdout: Option<Vec<u8>>,
                _stderr: Option<Vec<u8>>,
                _exit_code: i32
            ) -> Result<Self::Output, Self::Error> {
                unimplemented!()
            }
        }
    }

    #[test]
    fn allow_custom_errors() {
        let _result: MyError = Command::new("foo", ReturnError)
            .with_exec_replacement_callback(|_| {
                Ok(CapturedStdoutAndErr {
                    exit_code: 0,
                    stdout: Vec::new(),
                    stderr: Vec::new()
                })
            })
            .run()
            .unwrap_err();

        //------------
        struct ReturnError;
        impl ReturnSettings for ReturnError {
            type Output = ();
            type Error=MyError;
            fn map_output(
                &self,
                _stdout: Option<Vec<u8>>,
                _stderr: Option<Vec<u8>>,
                _exit_code: i32
            ) -> Result<Self::Output, Self::Error> {
                Err(MyError::Barfoot)
            }
        }
        #[derive(Debug, Error)]
        enum MyError {
            #[error("FooBar")]
            Barfoot,

            #[error(transparent)]
            CommandExecutionError(#[from] CommandExecutionError)
        }
    }

    proptest! {
        #[test]
        fn the_used_program_can_be_queried(s in ".*") {
            let s = OsStr::new(&*s);
            let cmd = Command::new(s, ReturnExitSuccess);
            prop_assert_eq!(&*cmd.program(), s)
        }

        #[test]
        fn set_arguments_can_be_retrieved_and_replace_previous_arguments(
            cmd_str in ".*",
            arguments in proptest::collection::vec(".*".prop_map(OsString::from), 0..5),
            arguments2 in proptest::collection::vec(".*".prop_map(OsString::from), 0..5)
        ) {
            let cmd_str = OsStr::new(&*cmd_str);
            let cmd = Command::new(cmd_str, ReturnExitSuccess).with_arguments(&arguments);
            prop_assert_eq!(cmd.arguments(), arguments);
            let cmd = cmd.with_arguments(&arguments2);
            prop_assert_eq!(cmd.arguments(), arguments2);
        }

        #[test]
        fn program_execution_can_be_replaced_with_an_callback(
            cmd in ".*".prop_map(OsString::from)
        ) {
            let cmd_ = cmd.clone();
            let was_run = Rc::new(RefCell::new(false));
            let was_run_  = was_run.clone();
            let cmd = Command::new(cmd, ReturnStdoutAndErr)
                .with_exec_replacement_callback(move |for_cmd| {
                    *(*was_run_).borrow_mut() = true;
                    assert_eq!(&*for_cmd.program(), cmd_);
                    Ok(CapturedStdoutAndErr {
                        exit_code: 0,
                        stdout: "result=12".to_owned().into(),
                        stderr: Vec::new()
                    })
                });

            let res = cmd.run().unwrap();
            assert_eq!(*was_run.borrow_mut(), true);
            assert_eq!(&*res.stdout, "result=12".as_bytes());
            assert_eq!(&*res.stderr, "".as_bytes());
        }

        #[test]
        fn return_an_error_if_the_command_has_non_zero_exit_status(
            exit_code in prop_oneof!(..0, 1..)
        ) {
            let res = Command::new("foo", ReturnExitSuccess)
                .with_exec_replacement_callback(move |_| {
                    Ok(CapturedStdoutAndErr {
                        exit_code,
                        stdout: Vec::new(),
                        stderr: Vec::new()
                    })
                })
                .run();

            res.unwrap_err();
        }
    }
}