use std::{
    borrow::Cow,
    collections::HashMap,
    ffi::{OsStr, OsString},
    path::PathBuf,
};

use crate::{EnvChange, ProcessPipeSetting};

/// The options used to spawn the sub-process.
///
/// Many getters and `&mut` based setters are provided through
/// dereferencing the [`ExecImplOptions`] instance contained in
/// a [`Command`].
///
#[derive(Debug)]
pub struct ExecImplOptions {
    /// The program to spawn
    pub program: OsString,

    /// The arguments to pass to the subprocess
    pub arguments: Vec<OsString>,

    /// The way the environment is updated for the subprocess.
    ///
    /// For every key in `env_updates` depending on `EnvChange`
    /// this will update the new processes environment to either
    /// remove the key (if it exists), set the key or make sure
    /// the key is inherited even if inheritance is disabled.
    ///
    /// See [`EnvChange`], [`ExecImplOptions::inherit_env`].
    ///
    /// # Warning
    ///
    /// The keys of env variables will *not* be evaluated for syntactic validity,
    /// until actually spawning the subprocess.
    /// Setting a key invalid on given platform *might* cause the process spawning to
    /// fail (e.g. using a key lik `"="` or `""`). It also *might* also do other thinks
    /// like the env variable being passed in but being unaccessible or similar. It's completely
    /// dependent on the OS and the impl. of `std::process::Command` or whatever is used to
    /// execute the command.
    pub env_updates: HashMap<OsString, EnvChange>,

    /// Determines if the child inherits the parents environment.
    ///
    /// After inheriting (or creating a empty) environment for
    /// the new process it will be updated based on [`ExecImplOptions.env_updates`].
    ///
    /// If inheritance is disabled specific env variables can still
    /// be inherited explicitly using [`EnvChange::Inherit`].
    pub inherit_env: bool,

    /// If `Some` the given path will be used as cwd for the new process.
    pub working_directory_override: Option<PathBuf>,

    /// Allows setting how the stdout pipe will be setup.
    ///
    /// If `Some` given `Stdio` setting will be used.
    ///
    /// If `None` and [`OutputMapping::needs_captured_stdout()`] is `true` then
    /// it will be set to `Some(Stdio::pipe())` before executing.
    ///
    /// If `None` and [`OutputMapping::needs_captured_stdout()`] is `false` then
    /// it stays `None` which implies the rust std default should be used.
    ///
    /// If set to `Some(Stdio::pipe())` then by default output should be captured
    /// even if [`OutputMapping::needs_captured_stdout()`] is `false`. Non-standard
    /// `ExecImpl` should do so too, but might not. For example using mock `ExecImpl`
    /// might panic on unexpected settings.
    ///
    /// # Panics
    ///
    /// Be aware that if [`OutputMapping::needs_captured_stdout()`] is `true` but
    /// this is set to a `Stdio` which is not `Stdio::pipe()` this will lead to
    /// an panic.
    pub override_stdout: Option<ProcessPipeSetting>,

    /// Same as [`ExecImplOptions::use_stdout_setup`] but for stderr.
    pub override_stderr: Option<ProcessPipeSetting>,

    /// Allows setting how the stdin pipe will be setup.
    ///
    /// If `Some` given `Stdio` setting will be used.
    ///
    /// If `None` it implies the rust std default should be used.
    ///
    pub override_stdin: Option<ProcessPipeSetting>,
}

impl ExecImplOptions {
    /// Create a new `ExecImplOptions` instance.
    pub fn new(program: OsString) -> Self {
        Self {
            program,
            arguments: Vec::new(),
            env_updates: HashMap::new(),
            inherit_env: true,
            working_directory_override: None,
            override_stdout: None,
            override_stderr: None,
            override_stdin: None,
        }
    }

    /// Returns a map with all env variables the sub-process spawned by this command would have
    /// if the current processes env is not changed.
    ///
    /// # Site note about `env::set_var()` problems
    ///
    /// Note that if you use `std::env::set_var()` in a multi-threaded setup depending on
    /// the operating system you run this on this can lead to all kind of problem, including
    /// unexpected race conditions in some situations (especially if `inherit_env(true)` is
    /// combined with `EnvChange::Inherit` and multiple variables are changed in another thread
    /// racing with this function and some but not all are covered by `EnvChange::Inherit`).
    ///
    /// Given that [`std::env::set_var()`] should strictly be avoided in a multi-threaded context
    /// this is seen as an acceptable drawback.
    ///
    /// Note that this function + `std::env::set_var()` is not unsafe it might just have a
    /// very unexpected result. Except if `env::set_var()` + reading env races are inherently
    /// unsafe on your system, in which case this has nothing to do with this function.
    pub fn create_expected_env_iter(&self) -> impl Iterator<Item = (Cow<OsStr>, Cow<OsStr>)> {
        crate::env::create_expected_env_iter(self.inherit_env, &self.env_updates)
    }
}
