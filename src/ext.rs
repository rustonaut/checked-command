


use std::fmt::Debug;
use std::process::{Command, ExitStatus, Child};
use std::process::{Output as StdOutput};
use std::io::Error as IoError;
use std::io::Result as IoResult;

#[cfg(use_std_output)]
pub type Output = StdOutput;

#[cfg(not(use_std_output))]
#[derive(Debug, Clone, Eq, PartialEq)]
/// custom output type which, diferently to `std::process::Output` is not
/// closely coupled with `ExitStatus` (it does not have a `status` field)
pub struct Output {
    /// the collected output of stdout as bytes
    pub stdout: Vec<u8>,
    /// the collected output of stderr as bytes
    pub stderr: Vec<u8>
}

#[cfg(not(use_std_output))]
impl From<StdOutput> for Output {
    fn from(out: StdOutput) -> Output {
        Output {
            stdout: out.stdout,
            stderr: out.stderr
        }
    }
}

quick_error! {

    /// error type representing either a `io::Error` or a
    /// failure caused by a non-successful exit status i.e.
    /// without exit code or a exit code not equal zero.
    #[derive(Debug)]
    pub enum Error {
        /// a `io::Error` occurred when handling the action
        Io(err: IoError) {
            from()
            description(err.description())
            cause(err)
        }
        /// Process exited with a non-zero exit code
        Failure(ex: ExitStatus, output: Option<Output>) {
            description("command failed with nonzero exit code")
            display("command failed with exit code {}", ex.code()
            .map(|code|code.to_string())
            .unwrap_or_else(||"<None> possible terminated by signal".into()))
        }
    }
}


/// Extension to `std::process::Command` adding versions of the output/status
/// functions which also fail/error with a non-success exit status
pub trait CommandExt {
    /// Behaves like `std::process::Command::output` but also checks the
    /// exit status for success. The `Output` produced in case of a
    /// command returning a failing exit status is included into the
    /// error.
    ///
    /// # Error
    ///
    /// if the exit status is not successful or a `io::Error` was returned
    /// from `Command::output`
    ///
    fn checked_output(&mut self) -> Result<Output, Error>;

    /// Behaves like `std::process::Command::status` but also checks the
    /// exit status for success, returning a error if it did not succeed
    ///
    /// # Error
    ///
    /// if the exit status is not successful or a `io::Error` was returned
    /// from `Command::output`
    ///
    fn checked_status(&mut self) -> Result<(), Error>;
}

/// Extension to `std::process::Child` adding versions of the wait_with_output/wait
/// functions which also fail/error with a non-success exit status
pub trait ChildExt {

    /// Behaves like `std::process::Child::wait_with_output` but also
    /// checks the exit status for success. The `Output` produced in
    /// case of a command returning a failing exit status is included
    /// into the error.
    ///
    /// # Error
    ///
    /// if the exit status is not successful or a `io::Error` was returned
    /// from `Child::wait_with_output`
    ///
    fn checked_wait_with_output(self) -> Result<Output, Error>;

    /// Behaves like `std::process::Child::wait` but also checks the
    /// exit status for success.
    ///
    /// # Error
    ///
    /// if the exit status is not successful or a `io::Error` was returned
    /// from `Child::checked_wait`
    ///
    fn checked_wait(&mut self) -> Result<(), Error>;

    /// Behaves like `std::process::child::try_wait` but also checks the
    /// exit status for success.
    ///
    /// This means `Ok(true)` is returned if the process exited and
    /// did with a successful exit status. If it did not exit jet `Ok(false)` is
    /// returned. If it exited but had a non successful exit status `Err(StatusError)`
    /// with the `StatusError::Failure` variant is returned.
    ///
    /// # Error
    ///
    /// if the exit status can be retrived but is not successful or
    /// a `io::Error` was returned from `Child::checked_try_wait`
    ///
    /// # Example
    ///
    /// ```no_run
    /// use std::process::Command;
    /// use checked_command::{ChildExt, Error};
    ///
    ///
    /// let mut child = Command::new("ls").spawn().unwrap();
    ///
    /// match child.checked_try_wait() {
    ///     Ok(true) => println!("exited with successful status (== 0)"),
    ///     Ok(false) => {
    ///         println!("command still running, now waiting");
    ///         let res = child.checked_wait();
    ///         println!("command finished");
    ///         println!("result: {:?}", res);
    ///     }
    ///     Err(Error::Io(e)) => println!("I/O error when attempting to wait for `ls` {}", e),
    ///     Err(Error::Failure(exit_status, _)) => {
    ///         println!("ls failed with exit code {:?}", exit_status.code())
    ///     }
    /// }
    /// ```
    #[cfg(feature="process_try_wait")]
    fn checked_try_wait(&mut self) -> Result<bool, Error>;
}


impl CommandExt for Command {
    fn checked_output(&mut self) -> Result<Output, Error> {
        convert_result(self.output())
    }
    fn checked_status(&mut self) -> Result<(), Error> {
        convert_result(self.status())
    }
}

impl ChildExt for Child {
    fn checked_wait_with_output(self) -> Result<Output, Error> {
        convert_result(self.wait_with_output())
    }
    fn checked_wait(&mut self) -> Result<(), Error> {
        convert_result(self.wait())
    }

    #[cfg(feature="process_try_wait")]
    fn checked_try_wait(&mut self) -> Result<bool, Error> {
        convert_result(self.try_wait())
    }
}



/// internal trait to zero-cost abstract
/// over handling `IoResult<Output>`` or
/// `IoResult<ExitStatus>``. It's a bit
/// over engineered but through zero-cost
/// abstraction (Type Extensions+Inlining,
/// ExitStatus is Copy) it should not
/// introduce any overhead at runtime
trait OutputOrExitStatus: Sized {
    type Out;
    fn use_ok_result(&self) -> bool;
    fn create_error(self) -> Error;
    fn convert(self) -> Self::Out;
}

#[cfg(feature="process_try_wait")]
impl OutputOrExitStatus for Option<ExitStatus> {
    type Out = bool;

    #[inline]
    fn use_ok_result(&self) -> bool { self.is_none() || self.unwrap().success() }

    #[inline]
    fn create_error(self) -> Error {
        // we can call unwrap as a None option won't lead to this branch
        // as it is only called if there is a exit status (but possible no
        // exit code nevertheless)
        Error::Failure(self.unwrap(), None)
    }

    #[inline]
    fn convert(self) -> bool {
        self.is_some()
    }
}

impl OutputOrExitStatus for ExitStatus {
    type Out = ();

    #[inline]
    fn use_ok_result(&self) -> bool {
        self.success()
    }

    #[inline]
    fn create_error(self) -> Error {
        Error::Failure(self, None)
    }

    #[inline]
    fn convert(self) -> () {
        ()
    }
}

impl OutputOrExitStatus for StdOutput {
    type Out = Output;

    #[inline]
    fn use_ok_result(&self) -> bool { self.status.success() }

    #[inline]
    fn create_error(self) -> Error {
        // because of the abstraction we got a Option but we can relay on
        // it to always be `Some(Output)` as long as this function is
        // not exported
        Error::Failure(self.status, Some(self.into()))
    }

    #[inline]
    fn convert(self) -> Output {
        self.into()
    }
}


/// works with both Output and `ExitStatus`
/// **without** introducing any clones or similar
/// which would not have been needed for
/// specialized methods
fn convert_result<T>(result: IoResult<T>) -> Result<T::Out, Error>
    where T: OutputOrExitStatus + Debug
{
    match result {
        Ok(think) => {
            if think.use_ok_result() {
                Ok(think.convert())
            } else {
                Err(think.create_error())
            }
        },
        Err(io_error) => Err(io_error.into())
    }
}

#[cfg(test)]
mod tests {



    // this crate on itself is doesn't care about unix/windows,
    // through the `from_raw` method is only aviable in the
    // unix specific `ExitStatusExt`, therefore tests are
    // only available on unix for now
    #[cfg(unix)]
    mod using_unix_exit_code_ext {

        use std::io;
        use std::fmt;
        use std::fmt::Write;
        use std::error::Error as StdError;
        use std::process::ExitStatus;
        use std::process::{Output as StdOutput};
        use std::os::unix::process::ExitStatusExt;
        use super::super::{convert_result, Error, Output};

        /// I will use this as a way to compare ExitStatus instances which do not
        /// implement PartialEq. Note that this is quite brittle, through ok for
        /// this contexts
        pub fn assert_debugstr_eq<Type: fmt::Debug>(a: Type, b: Type) {
            let mut buffer_a = String::new();
            write!(&mut buffer_a, "{:?}", a).expect("debug fmt (a) failed");
            let mut buffer_b = String::new();
            write!(&mut buffer_b, "{:?}", b).expect("debug fmt (b) failed");

            assert_eq!(buffer_a, buffer_b);
        }


        fn ok_exit_status() -> ExitStatus {
            ExitStatus::from_raw(0)
        }


        #[cfg(not(target_os="haiku"))]
        fn fail_exit_status() -> ExitStatus {
            ExitStatus::from_raw(2<<8)
        }

        #[cfg(target_os="haiku")]
        fn fail_exit_status() -> ExitStatus {
            ExitStatus::from_raw(2)
        }

        #[cfg(not(target_os="haiku"))]
        fn fail_exit_status_none() -> ExitStatus {
            ExitStatus::from_raw(2)
        }

        #[cfg(target_os="haiku")]
        fn fail_exit_status_none() -> ExitStatus {
            ExitStatus::from_raw(2<<8)
        }

        fn create_output(ex: ExitStatus) -> StdOutput {
            StdOutput {
                status: ex,
                stderr: vec![1,2,3],
                stdout: vec![1,2,3]
            }
        }

        #[test]
        fn conv_result_status_ok() {
            let res = convert_result(Ok(ok_exit_status()));
            assert_debugstr_eq(Ok(()), res);
        }

        #[test]
        fn conv_result_status_fail() {
            let fail_status = fail_exit_status();
            let res = convert_result(Ok(fail_status));
            assert_debugstr_eq(Err(Error::Failure(fail_status, None)), res);
        }

        #[test]
        fn conv_result_status_io_error() {
            let ioerr = io::Error::new(io::ErrorKind::Other, "bla");
            let ioerr2 = io::Error::new(io::ErrorKind::Other, "bla");
            let res: Result<(), Error> = convert_result::<ExitStatus>(Err(ioerr));
            assert_debugstr_eq(
                Err(Error::Io(ioerr2)),
                res
            )
        }

        #[test]
        fn conv_result_output_ok() {
            let out = create_output(ok_exit_status());
            let out2 = out.clone();
            assert_debugstr_eq(Ok(out2.into()), convert_result(Ok(out)));
        }

        #[test]
        fn conv_result_output_fail() {
            let fail_status = fail_exit_status();
            let out = create_output(fail_status);
            let out2 = out.clone();
            assert_debugstr_eq(
                Err(Error::Failure(fail_status, Some(out2.into()))),
                convert_result(Ok(out))
            )
        }

        #[test]
        fn conv_result_output_io_error() {
            let ioerr = io::Error::new(io::ErrorKind::Other, "bla");
            let ioerr2 = io::Error::new(io::ErrorKind::Other, "bla");
            let res: Result<Output, Error> = convert_result::<StdOutput>(Err(ioerr));
            assert_debugstr_eq(
                Err(Error::Io(ioerr2)),
                res
            )
        }

        #[cfg(feature="process_try_wait")]
        #[test]
        fn conv_result_not_ready() {
            match convert_result(Ok(None)) {
                Ok(false) => {},
                e => panic!("expected `Ok(false)` got `{:?}`", e)
            }
        }

        #[cfg(feature="process_try_wait")]
        #[test]
        fn conv_result_ready_ok() {
            match convert_result(Ok(Some(ok_exit_status()))) {
                Ok(true) => {},
                e => panic!("expected `Ok(true)` got `{:?}`", e)
            }
        }

        #[cfg(feature="process_try_wait")]
        #[test]
        fn conv_result_ready_failure() {
            let fail_status = fail_exit_status();
            let res = convert_result(Ok(Some(fail_status)));
            assert_debugstr_eq(Err(Error::Failure(fail_status, None)), res);
        }

        #[test]
        fn error_io_cause() {
            let ioerr = ||io::Error::new(io::ErrorKind::Other, "bla");
            let err = Error::Io(ioerr());
            assert_debugstr_eq(&ioerr() as &StdError, err.cause().unwrap());
        }

        #[test]
        fn error_io_description() {
            let ioerr = io::Error::new(io::ErrorKind::Other, "bla");
            let desc: String = ioerr.description().into();
            let got: String = Error::Io(ioerr).description().into();
            assert_eq!(desc, got);
        }

        #[test]
        fn error_failure_display() {
            let err = Error::Failure(fail_exit_status(), None);
            assert_eq!(format!("{}", err), "command failed with exit code 2");
        }

        #[test]
        fn error_failure_no_code_display() {
            let err = Error::Failure(fail_exit_status_none(), None);
            assert_eq!(format!("{}", err), "command failed with exit code <None> possible terminated by signal");
        }

        #[test]
        fn from_raw_ok() {
            let ex1 = ok_exit_status();
            assert_eq!(true, ex1.success());
            assert_eq!(Some(0), ex1.code());
        }

        #[test]
        fn from_raw_fail() {
            let ex1 = fail_exit_status();
            assert_eq!(false, ex1.success());
            assert_eq!(Some(2), ex1.code());
        }

        #[test]
        fn from_raw_fail_none() {
            let ex1 = fail_exit_status_none();
            assert_eq!(false, ex1.success());
            assert_eq!(None, ex1.code());
        }
    }

}