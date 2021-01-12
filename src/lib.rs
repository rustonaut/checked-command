use std::ffi::{OsStr, OsString};

#[derive(Debug)]
pub struct Command {
    program: OsString,
    arguments: Vec<OsString>
}

impl Command {

    /// Create a new command.
    pub fn new(program: impl Into<OsString>)  -> Self {
        Command {
            program: program.into(),
            arguments: Vec::new()
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashSet;
    use proptest::prelude::*;


    #[test]
    fn can_be_created_using_str_string_osstr_or_osstring() {
        Command::new("ls");
        Command::new("ls".to_owned());
        Command::new(OsString::from("ls"));
        Command::new(OsStr::new("ls"));
    }

    #[test]
    fn default_arguments_to_empty_list() {
        let cmd = Command::new("dos");
        assert_eq!(cmd.arguments(), &[] as &[OsString])
    }

    #[test]
    fn arguments_can_be_set_from_iterables() {
        Command::new("foo").with_arguments(Vec::<OsString>::new());
        Command::new("foo").with_arguments(HashSet::<OsString>::new());
        Command::new("foo").with_arguments(&[] as &[OsString]);
    }

    #[test]
    fn when_creating_command_all_capture_modes_can_be_used() {
        Command::new("foo");
        Command::new("foo");
        Command::new("foo");
    }

    proptest! {
        #[test]
        fn the_used_program_can_be_queried(s in ".*") {
            let s = OsStr::new(&*s);
            let cmd = Command::new(s);
            prop_assert_eq!(&*cmd.program(), s)
        }

        #[test]
        fn set_arguments_can_be_retrived_and_replace_previous_arguments(
            cmd_str in ".*",
            arguments in proptest::collection::vec(".*".prop_map(OsString::from), 0..5),
            arguments2 in proptest::collection::vec(".*".prop_map(OsString::from), 0..5)
        ) {
            let cmd_str = OsStr::new(&*cmd_str);
            let cmd = Command::new(cmd_str).with_arguments(&arguments);
            prop_assert_eq!(cmd.arguments(), arguments);
            let cmd = cmd.with_arguments(&arguments2);
            prop_assert_eq!(cmd.arguments(), arguments2);
        }
    }
}