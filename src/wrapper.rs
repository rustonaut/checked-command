use std::ffi::OsStr;
use std::path::Path;
use std::process::Stdio;
use std::process;
use std::io::{Error as IoError};
use std::process::{Command, Child};

use ext::{CommandExt, ChildExt, Error, Output};


/// A wrapper around `std::process::Child`
/// which hides the original `wait`/`wait_with_output` methods
/// and replaces it with the versions from `checked_command::ChildExt`
#[derive(Debug)]
pub struct CheckedChild {
    child: Child
}


impl From<Child> for CheckedChild {
    fn from(child: Child) -> CheckedChild {
        CheckedChild { child }
    }
}


impl CheckedChild {

    /// returns a mutable reference to the wrapped child
    pub fn as_std_command(&mut self) -> &mut Child {
        &mut self.child
    }

    /// converts the checked child into a `std::process::Child`
    /// note that a `Into<Child>` implementation is not provided.
    pub fn into_std_command(self) -> Child {
        self.child
    }

    /// return a optional &mut to the childs Stding
    pub fn stdin(&mut self) -> &mut Option<process::ChildStdin> {
        &mut self.child.stdin
    }

    /// return a optional &mut to the childs Stding
    pub fn stdout(&mut self) -> &mut Option<process::ChildStdout> {
        &mut self.child.stdout
    }

    /// return a optional &mut to the childs Stding
    pub fn stderr(&mut self) -> &mut Option<process::ChildStderr> {
        &mut self.child.stderr
    }

    /// calls `std::process::Child::kill`
    pub fn kill(&mut self) -> Result<(), IoError> {
        self.child.kill()
    }

    /// calls `std::process::Child::id`
    pub fn id(&self) -> u32 {
        self.child.id()
    }

    /// calls `ChildExt::checked_wait` on the wrapped `Child`
    pub fn wait(&mut self) -> Result<(), Error> {
        self.child.checked_wait()
    }

    /// calls `ChildExt::checked_try_wait` on the wrapped `Child`
    #[cfg(feature="process_try_wait")]
    pub fn try_wait(&mut self) -> Result<bool, Error> {
        self.child.checked_try_wait()
    }

    /// calls `ChildExt::checked_wait_with_output` on the wrapped `Child`
    pub fn wait_with_output(self) -> Result<Output, Error> {
        self.child.checked_wait_with_output()
    }

}


/// A wrapper around `std::process::Command`
/// which hides the original `status`/`output` methods
/// and replaces them with the versions from `checked_command::CommandExt`
#[derive(Debug)]
pub struct CheckedCommand {
    command: Command
}


impl From<Command> for CheckedCommand {
    fn from(command: Command) -> CheckedCommand {
        CheckedCommand { command }
    }
}


impl CheckedCommand {

    /// return a mutable reference to the wrapped `std::process::Command`
    /// this can be useful if the cammand has to be passed to a function
    /// or to access a extension trait for `std::process::Command`
    pub fn as_std_command(&mut self) -> &mut Command {
        &mut self.command
    }

    /// converts this Command into a `std::process::Command`
    /// (basically unwrapping it).
    ///
    /// Note that this function was intentionally provided instead
    /// of `Into<Command>` as the main reason for this wrapper is
    /// to prevent forgetting to check the exit status. As such it
    /// will forward all calls except `spawn`,`status` and `output`
    /// which normally tend to be the last function ever called on
    /// a command instances. Therefore converting a `CheckedCommand`
    /// to a `Command` is quite likely a bug. (Through there are some
    /// cases where it makes sense).
    pub fn into_std_command(self) -> Command {
        self.command
    }

    /// creates a new `CheckedCommand`
    /// see `std::process:Command::new` for more details
    pub fn new<S: AsRef<OsStr>>(program: S) -> CheckedCommand {
        CheckedCommand {
            command: Command::new(program)
        }
    }

    /// calls `std::process::Command::arg`
    pub fn arg<S: AsRef<OsStr>>(&mut self, arg: S) -> &mut CheckedCommand {
        self.command.arg(arg);
        self
    }

    /// calls `std::process::Command::args`
    pub fn args<I, S>(&mut self, args: I) -> &mut CheckedCommand
        where I: IntoIterator<Item=S>, S: AsRef<OsStr>
    {
        self.command.args(args);
        self
    }

    /// calls `std::process::Command::env`
    pub fn env<K, V>(&mut self, key: K, val: V) -> &mut CheckedCommand
        where K: AsRef<OsStr>, V: AsRef<OsStr>
    {
        self.command.env(key, val);
        self
    }

    /// calls `std::process::Command::envs`
    #[cfg(feature="command_envs")]
    pub fn envs<I, K, V>(&mut self, vars: I) -> &mut CheckedCommand
        where I: IntoIterator<Item=(K, V)>, K: AsRef<OsStr>, V: AsRef<OsStr>
    {
        self.command.envs(vars);
        self
    }

    /// calls `std::process::Command::env_remove`
    pub fn env_remove<K: AsRef<OsStr>>(&mut self, key: K) -> &mut CheckedCommand {
        self.command.env_remove(key);
        self
    }

    /// calls `std::process::Command::env_clear`
    pub fn env_clear(&mut self) -> &mut CheckedCommand {
        self.command.env_clear();
        self
    }

    /// calls `std::process::Command::current_dir`
    pub fn current_dir<P: AsRef<Path>>(&mut self, dir: P) -> &mut CheckedCommand {
        self.command.current_dir(dir);
        self
    }

    /// calls `std::process::Command::stdin`
    pub fn stdin(&mut self, cfg: Stdio) -> &mut CheckedCommand {
        self.command.stdin(cfg);
        self
    }

    /// calls `std::process::Command::stdout`
    pub fn stdout(&mut self, cfg: Stdio) -> &mut CheckedCommand {
        self.command.stdout(cfg);
        self
    }

    /// calls `std::process::Command::stderr`
    pub fn stderr(&mut self, cfg: Stdio) -> &mut CheckedCommand {
        self.command.stderr(cfg);
        self
    }

    /// calls `std::process::Command::spawn` and converts the
    /// `Child` into a `CheckedChild`
    pub fn spawn(&mut self) -> Result<CheckedChild, IoError> {
        self.command.spawn().map(Into::into)
    }

    /// calls `CommandExt::checked_output` on the wrapped `Command`
    pub fn output(&mut self) -> Result<Output, Error> {
        self.command.checked_output()
    }

    /// calls `CommandExt::checked_status` on the wrapped `Command`
    pub fn status(&mut self) -> Result<(), Error> {
        self.command.checked_status()
    }


}