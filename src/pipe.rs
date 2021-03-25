use std::process::{ChildStderr, ChildStdin, ChildStdout, Stdio};

/// Specifies how a process pipe (stdout/err/in) will be setup.
///
/// This is similar to `Stdio` but less opaque which does make
/// some parts around testing, mocking, tracing and similar easier.
///
/// Normally stdout/err is implicitly setup base don the [`OutputMapping`]
/// and as such you most times don't need to set this up.
///
/// The main usage is to set pipes to null when output is not captured
/// or in rare cases to redirect to another child.
#[derive(Debug)]
pub enum ProcessPipeSetting {
    /// Create a pipe to stdout/err/in.
    ///
    /// In case of stdout/err this will lead to output being captured.
    ///
    /// In case of stdin this means that the parent process can write
    /// to the stdin of the child process.
    ///
    /// This corresponds to `Stdio::pipe()`
    Piped,

    /// The child inherits the pipe from the parent.
    ///
    /// In the rust standard library this is the default
    /// setting when spawning a new process.
    ///
    /// This corresponds to `Stdio::inherit()`
    Inherit,

    /// Connect the child stdout/err/in to null (i.e. /dev/null on unix).
    ///
    /// This corresponds to `Stdio::null()`
    Null,

    /// Connects the child's stdout/err/in to another pipe.
    ///
    /// For example connects the stdin of a newly spawned process
    /// to the stdout of a child process spawned just before spawning
    /// this one.
    ///
    Redirect(Redirect),
}

impl Default for ProcessPipeSetting {
    fn default() -> Self {
        ProcessPipeSetting::Inherit
    }
}

impl From<Redirect> for ProcessPipeSetting {
    fn from(redirect: Redirect) -> Self {
        ProcessPipeSetting::Redirect(redirect)
    }
}

impl From<ChildStdout> for ProcessPipeSetting {
    fn from(out: ChildStdout) -> Self {
        Self::from(Redirect::from(out))
    }
}

impl From<ChildStderr> for ProcessPipeSetting {
    fn from(err: ChildStderr) -> Self {
        Self::from(Redirect::from(err))
    }
}

impl From<ChildStdin> for ProcessPipeSetting {
    fn from(inp: ChildStdin) -> Self {
        Self::from(Redirect::from(inp))
    }
}

impl From<ProcessPipeSetting> for Stdio {
    fn from(pps: ProcessPipeSetting) -> Self {
        use self::ProcessPipeSetting::*;
        match pps {
            Piped => Stdio::piped(),
            Inherit => Stdio::inherit(),
            Null => Stdio::null(),
            Redirect(opaque) => opaque.inner,
        }
    }
}

/// Opaque type representing the `ProcessPipeSetting::Redirect` variant.
#[derive(Debug)]
pub struct Redirect {
    inner: Stdio,
}

impl Redirect {
    /// Creates a instance from a `Stdio` instance without any checks.
    ///
    /// **Warning: If used with `Stdio` instances which do not do redirects,
    /// especially `Stdio::piped()` this can cause bugs and inconsistent
    /// behavior.**
    ///
    /// The reason for this is that we can't know what kind of variant
    /// `Stdio` internally is. Because of this the used `ExecImpl` instance
    /// might not be able to set things up properly. If the instance uses
    /// the normal child spawning functionality it will likely work, but
    /// no guarantees given.
    ///
    /// It still guarantees that this is safe.
    ///
    /// Still in some cases where you get a `Stdio` from child pipes
    /// or raw fds/handles this cna be a useful method.
    pub fn from_stdio_unchecked(inner: Stdio) -> Self {
        Redirect { inner }
    }
}

impl From<ChildStdout> for Redirect {
    fn from(out: ChildStdout) -> Self {
        Self {
            inner: Stdio::from(out),
        }
    }
}

impl From<ChildStderr> for Redirect {
    fn from(err: ChildStderr) -> Self {
        Self {
            inner: Stdio::from(err),
        }
    }
}

impl From<ChildStdin> for Redirect {
    fn from(inp: ChildStdin) -> Self {
        Self {
            inner: Stdio::from(inp),
        }
    }
}

#[cfg(unix)]
impl ::std::os::unix::io::FromRawFd for Redirect {
    unsafe fn from_raw_fd(fd: ::std::os::unix::io::RawFd) -> Self {
        Self {
            inner: Stdio::from_raw_fd(fd),
        }
    }
}
