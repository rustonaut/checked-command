//! Types related to how spawning a process is implemented.
//!
use std::{ffi::OsString, fmt::Debug, path::PathBuf};

use crate::{
    env::EnvBuilder,
    pipe::{InputPipeSetup, OutputPipeSetup},
};

//TODO (non-?)exhaustive but constructable, clone-able etc.
/// The options used to spawn the sub-process.
///
/// Many getters and `&mut` based setters are provided through
/// dereferencing the [`SpawnOptions`] instance contained in
/// a [`Command`].
///
#[derive(Debug)]
pub struct SpawnOptions {
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
    /// See [`EnvChange`], [`SpawnOptions::inherit_env`].
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
    pub env_builder: EnvBuilder,

    /// If `Some` the given path will be used as cwd for the new process.
    pub working_directory_override: Option<PathBuf>,

    /// Allows setting how the stdout pipe will be setup IFF it's not specified by the OutputMapping.
    ///
    /// **Warning: If [`OutputMapping::needs_captured_stdout()`] is true this field will be ignored**
    /// (in the default [`Spawner`]).
    ///
    /// If a custom `Piped` setting is setup (and it's not ignored, see above) and the pipe is not
    /// taken out between a spawn and the following wait then it's [`Spawner`] specific what will
    /// happen when wait is called. The default implementation will drop the pipe closing it in
    /// effect.
    pub custom_stdout_setup: Option<OutputPipeSetup>,

    /// Same as [`SpawnOptions::use_stdout_setup`] but for stderr.
    pub custom_stderr_setup: Option<OutputPipeSetup>,

    /// Allows setting how the stdin pipe will be setup.
    ///
    /// If `Some` given `Stdio` setting will be used.
    ///
    /// If `None` it implies the rust std default should be used.
    ///
    pub custom_stdin_setup: Option<InputPipeSetup>,
}

impl SpawnOptions {
    /// Create a new `SpawnOptions` instance.
    pub fn new(program: OsString) -> Self {
        Self {
            arguments: Vec::new(),
            env_builder: Default::default(),
            working_directory_override: None,
            custom_stdin_setup: None,
            program,
            custom_stdout_setup: None,
            custom_stderr_setup: None,
        }
    }
}
