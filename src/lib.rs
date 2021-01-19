pub use self::return_settings::*;
use std::{collections::HashMap, env::{self, VarsOs}, ffi::{OsStr, OsString}, fmt, fmt::Display, io, path::{Path, PathBuf}, borrow::Cow};
use thiserror::Error;

#[macro_use]
mod utils;
mod return_settings;
mod sys;

/*TODO
   new method design
   command
       .with_mapped_arguments(|args| -> args)
       .with_mapped_env_updates(|map| -> map)
       .with_inherit_env(bool)             !! should env be inherited? (updates are applied after inheriting)
       .with_delete_inherited_env_key(str) !! even if you inherit delete following keys
       ??                                  !! whitelist which keys are inherited?? No to unnecessary complex

*/
//TODO allow stderr/stdout suppression if not captured (instead of inherited)
pub struct Command<Output, Error>
where
    Output: 'static,
    Error: From<CommandExecutionError> + 'static,
{
    program: OsString,
    arguments: Vec<OsString>,
    env_updates: HashMap<OsString, EnvChange>,
    working_directory_override: Option<PathBuf>,
    expected_exit_code: ExitCode,
    check_exit_code: bool,
    return_settings: Option<Box<dyn ReturnSettings<Output = Output, Error = Error>>>,
    run_callback: Option<
        Box<
            dyn FnOnce(
                Self,
                &dyn ReturnSettings<Output = Output, Error = Error>,
            ) -> Result<ExecResult, io::Error>,
        >,
    >,
}

impl<Output, Error> Command<Output, Error>
where
    Output: 'static,
    Error: From<CommandExecutionError> + 'static,
{
    /// Create a new command.
    pub fn new(
        program: impl Into<OsString>,
        return_settings: impl ReturnSettings<Output = Output, Error = Error>,
    ) -> Self {
        Command {
            program: program.into(),
            arguments: Vec::new(),
            env_updates: HashMap::new(),
            check_exit_code: true,
            expected_exit_code: ExitCode::Some(0),
            return_settings: Some(Box::new(return_settings) as _),
            working_directory_override: None,
            run_callback: Some(Box::new(sys::actual_exec_exec_replacement_callback)),
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

    /// Returns this command with new arguments added to the end of the argument list
    pub fn with_arguments<T>(mut self, args: impl IntoIterator<Item = T>) -> Self
    where
        T: Into<OsString>,
    {
        self.arguments.extend(args.into_iter().map(|v| v.into()));
        self
    }

    /// Returns this command with a new argument added to the end of the argument list
    pub fn with_argument(mut self, arg: impl Into<OsString>) -> Self {
        self.arguments.push(arg.into());
        self
    }

    /// Return a map of all env variables which will be set/overwritten in the subprocess.
    pub fn env_updates(&self) -> &HashMap<OsString, EnvChange> {
        &self.env_updates
    }

    /// Returns a map with all env variables the sub-process spawned by this command would have.
    pub fn create_expected_env_iter(&self) -> impl Iterator<Item=(Cow<OsStr>, Cow<OsStr>)> {
        return ExpectedEnvIter {
            self_: self,
            env_source: Some(env::vars_os()),
            update_source: Some(self.env_updates.iter()),
        };

        //FIXME[rust/generators] use yield base iterator
        struct ExpectedEnvIter<'a, Output, Error>
        where
            Output: 'static,
            Error: From<CommandExecutionError> + 'static
        {
            self_: &'a Command<Output, Error>,
            env_source: Option<VarsOs>,
            update_source: Option<std::collections::hash_map::Iter<'a, OsString, EnvChange>>
        }

        impl<'a,O,E> Iterator for ExpectedEnvIter<'a,O,E>
        where
            O: 'static,
            E: From<CommandExecutionError> + 'static
        {
            type Item =(Cow<'a, OsStr>, Cow<'a, OsStr>);

            fn next(&mut self) -> Option<Self::Item> {
                loop {
                    fused_opt_iter_next!(&mut self.env_source, |(key, val)| {
                        match self.self_.env_updates.get(&key) {
                            Some(_) => continue,
                            None => return Some((Cow::Owned(key), Cow::Owned(val)))
                        }
                    });
                    fused_opt_iter_next!(&mut self.update_source, |(key, change)| {
                        match change {
                            EnvChange::Set(val) => return Some((Cow::Borrowed(&key), Cow::Borrowed(&val))),
                            EnvChange::Remove => continue,
                        }
                    });
                    return None;
                }
            }
        }
    }

    /// Returns this command with the map of env updates updated by given iterator of key value pairs.
    ///
    /// If any key from the new map already did exist in the current updates it will
    /// replace the old key & value.
    ///
    /// # Example
    ///
    /// TODO: hashmap
    pub fn with_env_updates<K, V>(mut self, map: impl IntoIterator<Item = (K, V)>) -> Self
    where
        K: Into<OsString>,
        V: Into<EnvChange>,
    {
        self.env_updates
            .extend(map.into_iter().map(|(k, v)| (k.into(), v.into())));
        self
    }

    /// Returns this command with the map of env updates updated by one key value pari.
    ///
    /// If the new key already did exist in the current updates it will replace that
    /// old key & value.
    pub fn with_env_update(mut self, key: impl Into<OsString>, value: impl Into<EnvChange>) -> Self {
        self.env_updates.insert(key.into(), value.into());
        self
    }

    /// Return the working directory which will be used instead of the current working directory.
    ///
    /// If `None` is returned it means no override is set and the working directory will be inherited
    /// from the spawning process.
    pub fn working_directory_override(&self) -> Option<&Path> {
        self.working_directory_override.as_ref().map(|s| &**s)
    }

    /// Replaces the working directory override.
    ///
    /// Setting it to `None` will unset the override making the spawned
    /// process inherit the working directory from the spawning process.
    pub fn with_working_directory_override(
        mut self,
        wd_override: Option<impl Into<PathBuf>>,
    ) -> Self {
        self.working_directory_override = wd_override.map(Into::into);
        self
    }

    /// Return which exit code is treated as success.
    pub fn expected_exit_code(&self) -> ExitCode {
        self.expected_exit_code
    }

    /// Set which exit code is treated as successful.
    ///
    /// **This enables exit code checking even if it
    ///   was turned of before.**
    pub fn with_expected_exit_code(self, exit_code: impl Into<ExitCode>) -> Self {
        let mut cmd = self.with_check_exit_code(true);
        cmd.expected_exit_code = exit_code.into();
        cmd
    }

    /// Returns true if the exit code is checked before mapping the output(s).
    pub fn check_exit_code(&self) -> bool {
        self.check_exit_code
    }

    /// Sets if the exit code is checked before mapping the output(s).
    pub fn with_check_exit_code(mut self, val: bool) -> Self {
        self.check_exit_code = val;
        self
    }

    /// Returns true if stdout will be captured.
    ///
    /// # Panics
    ///
    /// **If called in a `exec_replacement_callback` this will panic.
    pub fn will_capture_stdout(&self) -> bool {
        self.return_settings
            .as_ref()
            .expect("Can not be called in a exec_replacement_callback.")
            .capture_stdout()
    }

    /// Returns true if stderr will be captured.
    ///
    /// # Panics
    ///
    /// **If called in a `exec_replacement_callback` this will panic.
    pub fn will_capture_stderr(&self) -> bool {
        self.return_settings
            .as_ref()
            .expect("Can not be called in a exec_replacement_callback.")
            .capture_stderr()
    }

    /// Run the command, blocking until completion
    ///
    /// # Panics
    ///
    /// **This will panic if called in a exec_replacement_callback.**
    pub fn run(mut self) -> Result<Output, Error> {
        let expected_exit_code = self.expected_exit_code;
        let check_exit_code = self.check_exit_code;
        let return_settings = self
            .return_settings
            .take()
            .expect("run recursively called in exec replacing callback");
        let run_callback = self
            .run_callback
            .take()
            .expect("run recursively called in exec replacing callback");

        let result = run_callback(self, &*return_settings)
            .map_err(|err| CommandExecutionError::SpawningProcessFailed(err))?;

        if check_exit_code && result.exit_code != expected_exit_code {
            Err(Error::from(CommandExecutionError::UnexpectedExitCode {
                got: result.exit_code,
                expected: expected_exit_code,
            }))
        } else {
            let stdout = if return_settings.capture_stdout() {
                result.stdout
            } else {
                debug_assert!(result.stdout.is_none());
                None
            };
            let stderr = if return_settings.capture_stderr() {
                result.stderr
            } else {
                debug_assert!(result.stderr.is_none());
                None
            };
            let exit_code = result.exit_code;
            return_settings.map_output(stdout, stderr, exit_code)
        }
    }

    /// Sets a callback which is called instead of executing the command when running the command.
    ///
    /// While the callback get a instance of this type some fields have been extracted which means
    /// that some method can not be called inside of the callback and will panic if you do so:
    ///
    /// - [`Self.run()`], recursively calling run will not work.
    /// - [`Self.will_capture_stdout()`], use the passed in return settings [`ReturnSetting.capture_stdout()`] method instead.
    /// - [`Self.will_capture_stderr()`], use the passed in return settings [`ReturnSetting.capture_stderr()`] method instead.
    ///
    pub fn with_exec_replacement_callback(
        mut self,
        callback: impl FnOnce(
                Self,
                &dyn ReturnSettings<Output = Output, Error = Error>,
            ) -> Result<ExecResult, io::Error>
            + 'static,
    ) -> Self {
        self.run_callback = Some(Box::new(callback));
        self
    }
}

/// Trait used to configure what [`Command::run()`] returns.
pub trait ReturnSettings: 'static {
    type Output: 'static;
    type Error: 'static;

    fn capture_stdout(&self) -> bool;
    fn capture_stderr(&self) -> bool;

    fn map_output(
        self: Box<Self>,
        stdout: Option<Vec<u8>>,
        stderr: Option<Vec<u8>>,
        exit_code: ExitCode,
    ) -> Result<Self::Output, Self::Error>;
}

#[derive(Debug, Error)]
pub enum CommandExecutionError {
    #[error("Spawning process failed: {}", _0)]
    SpawningProcessFailed(io::Error),

    #[error("Unexpected exit code. Got: {got}, Expected: {expected}")]
    UnexpectedExitCode { got: ExitCode, expected: ExitCode },
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ExitCode {
    Some(i32),
    ProcessTerminatedBeforeExiting,
}

impl Display for ExitCode {
    fn fmt(&self, fter: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Some(code) => Display::fmt(code, fter),
            Self::ProcessTerminatedBeforeExiting => fter.write_str("terminated before exit"),
        }
    }
}

impl Default for ExitCode {
    fn default() -> Self {
        Self::Some(0)
    }
}

impl From<i32> for ExitCode {
    fn from(code: i32) -> Self {
        ExitCode::Some(code)
    }
}

impl PartialEq<i32> for ExitCode {
    fn eq(&self, other: &i32) -> bool {
        match self {
            Self::Some(code) => code == other,
            Self::ProcessTerminatedBeforeExiting => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum EnvChange {
    Remove,
    // Inherit,
    Set(OsString)
}

impl From<&Self> for EnvChange {
    fn from(borrow: &Self) -> Self {
        borrow.clone()
    }
}
impl From<&OsString> for EnvChange {
    fn from(val: &OsString) -> Self {
        EnvChange::Set(val.clone())
    }
}

impl From<OsString> for EnvChange {
    fn from(val: OsString) -> Self {
        EnvChange::Set(val)
    }
}

impl From<&OsStr> for EnvChange {
    fn from(val: &OsStr) -> Self {
        EnvChange::Set(val.into())
    }
}

impl From<String> for EnvChange {
    fn from(val: String) -> Self {
        EnvChange::Set(val.into())
    }
}

impl From<&str> for EnvChange {
    fn from(val: &str) -> Self {
        EnvChange::Set(val.into())
    }
}


#[derive(Debug, Default)]
pub struct ExecResult {
    pub exit_code: ExitCode,
    pub stdout: Option<Vec<u8>>,
    pub stderr: Option<Vec<u8>>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;
    use std::{cell::RefCell, collections::HashSet, env, iter, rc::Rc};
    use thiserror::Error;

    #[derive(Debug)]
    enum TestCommandError {
        Lib(CommandExecutionError),
        Prop(TestCaseError),
    }

    impl From<CommandExecutionError> for TestCommandError {
        fn from(cee: CommandExecutionError) -> Self {
            Self::Lib(cee)
        }
    }

    impl From<TestCaseError> for TestCommandError {
        fn from(cee: TestCaseError) -> Self {
            Self::Prop(cee)
        }
    }

    impl TestCommandError {
        pub fn unwrap_prop(self) -> TestCaseError {
            match self {
                Self::Lib(err) => panic!("unexpected error: {:?}", err),
                Self::Prop(prop_err) => return prop_err,
            }
        }
    }

    struct TestReturnSetting {
        capture_stdout: bool,
        capture_stderr: bool,
    }

    impl ReturnSettings for TestReturnSetting {
        type Output = bool;
        type Error = TestCommandError;

        fn capture_stdout(&self) -> bool {
            self.capture_stdout
        }
        fn capture_stderr(&self) -> bool {
            self.capture_stderr
        }

        fn map_output(
            self: Box<Self>,
            stdout: Option<Vec<u8>>,
            stderr: Option<Vec<u8>>,
            _exit_code: ExitCode,
        ) -> Result<Self::Output, Self::Error> {
            (|| {
                prop_assert_eq!(stdout.is_some(), self.capture_stdout());
                prop_assert_eq!(stderr.is_some(), self.capture_stderr());
                Ok(())
            })()?;
            Ok(true)
        }
    }

    #[test]
    fn comp_can_be_created_using_str_string_osstr_or_osstring() {
        Command::new("ls", ReturnNothing);
        Command::new("ls".to_owned(), ReturnNothing);
        Command::new(OsString::from("ls"), ReturnNothing);
        Command::new(OsStr::new("ls"), ReturnNothing);
    }

    #[test]
    fn default_arguments_to_empty_list() {
        let cmd = Command::new("dos", ReturnNothing);
        assert_eq!(cmd.arguments(), &[] as &[OsString])
    }

    #[test]
    fn comp_arguments_can_be_set_from_iterables() {
        Command::new("foo", ReturnNothing).with_arguments(Vec::<OsString>::new());
        Command::new("foo", ReturnNothing).with_arguments(HashSet::<OsString>::new());
        Command::new("foo", ReturnNothing).with_arguments(&[] as &[OsString]);
    }

    #[test]
    fn comp_when_creating_command_all_capture_modes_can_be_used() {
        Command::new("foo", ReturnNothing);
        Command::new("foo", ReturnStdout);
        Command::new("foo", ReturnStderr);
        Command::new("foo", ReturnStdoutAndErr);
    }

    #[test]
    fn run_can_lead_to_and_io_error() {
        let res = Command::new("foo", ReturnNothing)
            .with_exec_replacement_callback(|_, _| {
                Err(io::Error::new(io::ErrorKind::Other, "random"))
            })
            .run();

        res.unwrap_err();
    }

    #[test]
    fn return_no_error_if_the_command_has_zero_exit_status() {
        let res = Command::new("foo", ReturnNothing)
            .with_exec_replacement_callback(move |_, _| {
                Ok(ExecResult {
                    exit_code: 0.into(),
                    ..Default::default()
                })
            })
            .run();

        res.unwrap();
    }

    #[test]
    fn comp_command_must_only_be_generic_over_the_output() {
        if false {
            let mut _cmd = Command::new("foo", ReturnNothing);
            _cmd = Command::new("foo", ReturnNothingAlt);
        }

        //---
        struct ReturnNothingAlt;
        impl ReturnSettings for ReturnNothingAlt {
            type Output = ();
            type Error = CommandExecutionError;
            fn capture_stdout(&self) -> bool {
                false
            }
            fn capture_stderr(&self) -> bool {
                false
            }
            fn map_output(
                self: Box<Self>,
                _stdout: Option<Vec<u8>>,
                _stderr: Option<Vec<u8>>,
                _exit_code: ExitCode,
            ) -> Result<Self::Output, Self::Error> {
                unimplemented!()
            }
        }
    }

    #[test]
    fn allow_custom_errors() {
        let _result: MyError = Command::new("foo", ReturnError)
            .with_exec_replacement_callback(|_, _| {
                Ok(ExecResult {
                    exit_code: 0.into(),
                    ..Default::default()
                })
            })
            .run()
            .unwrap_err();

        //------------
        struct ReturnError;
        impl ReturnSettings for ReturnError {
            type Output = ();
            type Error = MyError;
            fn capture_stdout(&self) -> bool {
                false
            }
            fn capture_stderr(&self) -> bool {
                false
            }
            fn map_output(
                self: Box<Self>,
                _stdout: Option<Vec<u8>>,
                _stderr: Option<Vec<u8>>,
                _exit_code: ExitCode,
            ) -> Result<Self::Output, Self::Error> {
                Err(MyError::Barfoot)
            }
        }
        #[derive(Debug, Error)]
        enum MyError {
            #[error("FooBar")]
            Barfoot,

            #[error(transparent)]
            CommandExecutionError(#[from] CommandExecutionError),
        }
    }

    #[test]
    fn by_default_no_environment_is_updated() {
        let cmd = Command::new("foo", ReturnNothing);
        assert!(cmd.env_updates().is_empty());
    }

    #[test]
    fn by_default_no_explicit_working_directory_is_set() {
        let cmd = Command::new("foo", ReturnNothing);
        assert_eq!(cmd.working_directory_override(), None);
    }

    //FIXME proptest
    #[test]
    fn replacing_the_working_dir_override() {
        let cmd =
            Command::new("foo", ReturnNothing).with_working_directory_override(Some("/foo/bar"));

        assert_eq!(
            cmd.working_directory_override(),
            Some(Path::new("/foo/bar"))
        );

        let cmd = cmd.with_working_directory_override(Some(Path::new("/bar/foot")));
        assert_eq!(
            cmd.working_directory_override(),
            Some(Path::new("/bar/foot"))
        );
    }

    #[test]
    fn by_default_the_expected_exit_code_is_0() {
        let cmd = Command::new("foo", ReturnNothing);
        assert_eq!(cmd.expected_exit_code(), 0);
    }

    #[test]
    fn you_can_expect_no_exit_code_to_be_returned() {
        let cmd = Command::new("foo", ReturnNothing)
            .with_expected_exit_code(ExitCode::ProcessTerminatedBeforeExiting);

        assert_eq!(
            cmd.expected_exit_code(),
            ExitCode::ProcessTerminatedBeforeExiting
        );
    }

    #[test]
    fn by_default_exit_code_checking_is_enabled() {
        let cmd = Command::new("foo", ReturnNothing);
        assert_eq!(cmd.check_exit_code(), true);
    }

    #[test]
    fn setting_check_exit_code_to_false_disables_it() {
        Command::new("foo", ReturnNothing)
            .with_check_exit_code(false)
            .with_exec_replacement_callback(|_, _| {
                Ok(ExecResult {
                    exit_code: 1.into(),
                    ..Default::default()
                })
            })
            .run()
            .unwrap();
    }

    #[should_panic]
    #[test]
    fn returning_stdout_which_should_not_be_captured_triggers_a_debug_assertion() {
        let _ = Command::new("foo", ReturnNothing)
            .with_check_exit_code(false)
            .with_exec_replacement_callback(|_, _| {
                Ok(ExecResult {
                    exit_code: 1.into(),
                    stdout: Some(Vec::new()),
                    ..Default::default()
                })
            })
            .run();
    }

    #[should_panic]
    #[test]
    fn returning_stderr_which_should_not_be_captured_triggers_a_debug_assertion() {
        let _ = Command::new("foo", ReturnNothing)
            .with_check_exit_code(false)
            .with_exec_replacement_callback(|_, _| {
                Ok(ExecResult {
                    exit_code: 1.into(),
                    stderr: Some(Vec::new()),
                    ..Default::default()
                })
            })
            .run();
    }

    #[cfg(unix)]
    #[test]
    fn can_run_the_echo_program() {
        let cap = Command::new("echo", ReturnStdout)
            .with_arguments(vec!["hy", "there"])
            .run()
            .unwrap();

        assert_eq!(String::from_utf8_lossy(&*cap.stdout), "hy there\n");
    }

    #[cfg(unix)]
    #[test]
    fn can_run_failing_program_without_failing() {
        let cap = Command::new("cp", ReturnStderr)
            .with_arguments(vec!["/"])
            .with_expected_exit_code(1)
            .run()
            .unwrap();

        assert!(!cap.stderr.is_empty());
    }

    #[test]
    fn setting_the_expected_exit_code_will_enable_checking() {
        let cmd = Command::new("foo", ReturnNothing)
            .with_check_exit_code(false)
            .with_expected_exit_code(0);

        assert_eq!(cmd.check_exit_code(), true);
    }

    #[test]
    fn create_expected_env_iter_includes_the_current_env_by_default() {
        let process_env = env::vars_os().into_iter().map(|(k,v)| (Cow::Owned(k), Cow::Owned(v)))
            .collect::<HashMap<_,_>>();
        let cmd = Command::new("foo", ReturnNothing);
        //TODO _env_iter!
        let created_map = cmd.create_expected_env_iter().collect::<HashMap<_,_>>();
        assert_eq!(process_env, created_map);
    }

    proptest! {
        #[test]
        fn the_used_program_can_be_queried(s in ".*") {
            let s = OsStr::new(&*s);
            let cmd = Command::new(s, ReturnNothing);
            prop_assert_eq!(&*cmd.program(), s)
        }

        #[test]
        fn new_arguments_can_be_added(
            cmd in ".*",
            argument in ".*".prop_map(OsString::from),
            arguments in proptest::collection::vec(".*".prop_map(OsString::from), 0..5),
            arguments2 in proptest::collection::vec(".*".prop_map(OsString::from), 0..5)
        ) {
            let cmd = OsStr::new(&*cmd);
            let cmd = Command::new(cmd, ReturnNothing)
                .with_arguments(&arguments);
            prop_assert_eq!(cmd.arguments(), &arguments);
            let cmd = cmd.with_argument(&argument);
            prop_assert_eq!(
                cmd.arguments().iter().collect::<Vec<_>>(),
                arguments.iter().chain(iter::once(&argument)).collect::<Vec<_>>()
            );
            let cmd = cmd.with_arguments(&arguments2);
            prop_assert_eq!(
                cmd.arguments().iter().collect::<Vec<_>>(),
                arguments.iter()
                    .chain(iter::once(&argument))
                    .chain(arguments2.iter())
                    .collect::<Vec<_>>()
            );
        }

        //TODO
        // #[test]
        // #[allow(unreachable_code)]
        // fn the_argument_vector_can_be_mapped(
        //     cmd in ".*"
        // ) {
        //     let cmd = Command::new(cmd, ReturnNothing);
        //     todo!()
        // }

        // #[test]
        // #[allow(unreachable_code)]
        // fn the_env_updates_map_can_be_mapped(
        //     cmd in ".*"
        // ) {
        //     let cmd = Command::new(cmd, ReturnNothing);
        //     todo!()
        // }

        #[test]
        fn new_env_variables_can_be_added(
            cmd in ".*",
            variable in ".*".prop_map(OsString::from),
            value in ".*".prop_map(OsString::from),
            map1 in proptest::collection::hash_map(
                ".*".prop_map(OsString::from),
                ".*".prop_map(|s| EnvChange::Set(OsString::from(s))),
                0..4
            ),
            map2 in proptest::collection::hash_map(
                ".*".prop_map(OsString::from),
                ".*".prop_map(|s| EnvChange::Set(OsString::from(s))),
                0..4
            ),
        ) {
            let cmd = Command::new(cmd, ReturnNothing)
                .with_env_updates(&map1);

            prop_assert_eq!(cmd.env_updates(), &map1);

            let cmd = cmd.with_env_update(&variable, &value);

            let mut n_map = map1.clone();
            n_map.insert(variable, EnvChange::Set(value));
            prop_assert_eq!(cmd.env_updates(), &n_map);

            let cmd = cmd.with_env_updates(&map2);

            for (key, value) in &map2 {
                n_map.insert(key.into(), value.into());
            }
            prop_assert_eq!(cmd.env_updates(), &n_map);
        }


        //FIXME on CI this test can leak secrets if it fails
        #[test]
        fn env_variables_can_be_set_to_be_removed_from_inherited_env(
            cmd in ".*",
            rem_key in proptest::sample::select(env::vars_os().map(|(k,_v)| k).collect::<Vec<_>>())
        ) {
            let cmd = Command::new(cmd, ReturnNothing).with_env_update(rem_key.clone(), EnvChange::Remove);
            prop_assert_eq!(cmd.env_updates().get(&rem_key), Some(&EnvChange::Remove));

            let produced_env = cmd.create_expected_env_iter()
                .map(|(k,v)| (k.into_owned(), v.into_owned()))
                .collect::<HashMap<OsString, OsString>>();

            prop_assert_eq!(produced_env.get(&rem_key), None);
        }

        //FIXME on CI this test can leak secrets if it fails
        #[test]
        fn env_variables_can_be_set_to_be_replaced_from_inherited_env(
            cmd in ".*",
            rem_key in proptest::sample::select(env::vars_os().map(|(k,_v)| k).collect::<Vec<_>>()),
            replacement in ".*".prop_map(OsString::from)
        ) {
            let cmd = Command::new(cmd, ReturnNothing).with_env_update(rem_key.clone(), EnvChange::Set(replacement.clone()));
            let expect = EnvChange::Set(replacement.clone());
            prop_assert_eq!(cmd.env_updates().get(&rem_key), Some(&expect));
            let produced_env = cmd.create_expected_env_iter()
                .map(|(k,v)| (k.into_owned(), v.into_owned()))
                .collect::<HashMap<OsString, OsString>>();

            prop_assert_eq!(produced_env.get(&rem_key), Some(&replacement));
        }



        #[test]
        fn the_working_directory_can_be_changed(
            cmd in ".*",
            wd_override in prop_oneof!(".*".prop_map(|v| Some(PathBuf::from(v))), Just(None)),
            wd_override2 in prop_oneof!(".*".prop_map(|v| Some(PathBuf::from(v))), Just(None))
        ) {
            let cmd = Command::new(cmd, ReturnNothing)
                .with_working_directory_override(wd_override.as_ref());

            assert_eq!(cmd.working_directory_override(), wd_override.as_ref().map(|i|&**i));

            let cmd = cmd.with_working_directory_override(wd_override2.as_ref());
            assert_eq!(cmd.working_directory_override(), wd_override2.as_ref().map(|i|&**i));
        }

        #[test]
        fn program_execution_can_be_replaced_with_an_callback(
            cmd in ".*".prop_map(OsString::from)
        ) {
            let cmd_ = cmd.clone();
            let was_run = Rc::new(RefCell::new(false));
            let was_run_  = was_run.clone();
            let cmd = Command::new(cmd, ReturnStdoutAndErr)
                .with_exec_replacement_callback(move |for_cmd,_| {
                    *(*was_run_).borrow_mut() = true;
                    assert_eq!(&*for_cmd.program(), cmd_);
                    Ok(ExecResult {
                        exit_code: 0.into(),
                        stdout: Some("result=12".to_owned().into()),
                        stderr: Some(Vec::new())
                    })
                });

            let res = cmd.run().unwrap();
            assert_eq!(*was_run.borrow_mut(), true);
            assert_eq!(&*res.stdout, "result=12".as_bytes());
            assert_eq!(&*res.stderr, "".as_bytes());
        }

        #[test]
        fn return_an_error_if_the_command_has_non_zero_exit_status(
            exit_code in prop_oneof!(..0, 1..).prop_map(ExitCode::from)
        ) {
            let res = Command::new("foo", ReturnNothing)
                .with_exec_replacement_callback(move |_,_| {
                    Ok(ExecResult {
                        exit_code,
                        ..Default::default()
                    })
                })
                .run();

            res.unwrap_err();
        }

        #[test]
        fn replacing_the_expected_exit_code_causes_error_on_different_exit_codes(
            exit_code in -5..6,
            offset in prop_oneof!(-100..0, 1..101)
        ) {
            let res = Command::new("foo", ReturnNothing)
                .with_expected_exit_code(exit_code)
                .with_exec_replacement_callback(move |cmd,_| {
                    assert_eq!(cmd.expected_exit_code(), exit_code);
                    Ok(ExecResult {
                        exit_code: ExitCode::from(exit_code + offset),
                        ..Default::default()
                    })
                })
                .run();

            match res {
                Err(CommandExecutionError::UnexpectedExitCode {got, expected}) => {
                    assert_eq!(expected, exit_code);
                    assert_eq!(got, exit_code+offset);
                },
                _ => panic!("Unexpected Result: {:?}", res)
            }
        }

        #[test]
        fn exit_code_checking_can_be_disabled_and_enabled(
            change1 in proptest::bool::ANY,
            change2 in proptest::bool::ANY,
        ) {
            let cmd = Command::new("foo", ReturnNothing)
                .with_check_exit_code(change1);

            assert_eq!(cmd.check_exit_code(), change1);

            let cmd = cmd.with_check_exit_code(change2);
            assert_eq!(cmd.check_exit_code(), change2);
        }

        #[test]
        fn only_pass_stdout_stderr_to_map_output_if_return_settings_indicate_they_capture_it(
            capture_stdout in proptest::bool::ANY,
            capture_stderr in proptest::bool::ANY
        ) {
            let res = Command::new("foo", TestReturnSetting { capture_stdout, capture_stderr })
                .with_exec_replacement_callback(move |_,_| {
                    Ok(ExecResult {
                        exit_code: 0.into(),
                        stdout: if capture_stdout { Some(Vec::new()) } else { None },
                        stderr: if capture_stderr { Some(Vec::new()) } else { None }
                    })
                })
                .run()
                .map_err(|e| e.unwrap_prop())?;

            assert!(res);
        }

        #[test]
        fn command_provides_a_getter_to_check_if_stdout_and_err_will_be_captured(
            capture_stdout in proptest::bool::ANY,
            capture_stderr in proptest::bool::ANY
        ) {
            let cmd = Command::new("foo", TestReturnSetting { capture_stdout, capture_stderr });
            prop_assert_eq!(cmd.will_capture_stdout(), capture_stdout);
            prop_assert_eq!(cmd.will_capture_stderr(), capture_stderr);
        }

        #[test]
        fn capture_hints_are_available_in_the_callback(
            capture_stdout in proptest::bool::ANY,
            capture_stderr in proptest::bool::ANY
        ) {
            Command::new("foo", TestReturnSetting { capture_stdout, capture_stderr })
                .with_exec_replacement_callback(move |_cmd, return_settings| {
                    assert_eq!(return_settings.capture_stdout(), capture_stdout);
                    assert_eq!(return_settings.capture_stderr(), capture_stderr);
                    Ok(ExecResult {
                        exit_code: 0.into(),
                        stdout: if capture_stdout { Some(Vec::new()) } else { None },
                        stderr: if capture_stderr { Some(Vec::new()) } else { None }
                    })
                })
                .run()
                .unwrap();
        }
    }
}
