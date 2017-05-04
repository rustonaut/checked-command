//TODO add CheckedCommand/CheckedChild to doc
//! In std the methods used to retrieve the `ExitStatus`/`Output`
//! from executing a `Command` do not consider the exit status
//! when deciding weather or not the function returns a error.
//! This crate has two extension traits providing checked
//! versions of `Command::status`, `Command::output`, `Child::wait` and
//! `Child::wait_with_output`. All of them will return a error if the
//! `ExitStatus` is not successful or a `io::Error` occurred.
//!
//! As the `Output` of `output`/`wait_with_output` failing through a non sucessfull
//! exit status might still be needed (and is always available) it is contained into
//! the returned Error type, as there is never a output for `status`/`wait` the checked
//! variations of them return a different Error without a Output field. While this seemd
//! to be the best approach for cases where the error is directly handled (as there is no
//! "always ok unwrap" or "always ignored enum field") it can be bothersome for situations
//! where the error is "just" propagated up and the (err-)output is less relevant. Therefore
//! a third error with an optional Output is provided, including `From` implementations so
//! that `.into()` or the implicit conversion with `try!{}`/`?` can be used to convert to it.
//! _Note: in the future, before a v1.0 release the decision will be revisited and all functions
//! might be changed to directly return the `Error` containing a optional `Output`_
//!
//!
//! # Example (CommandExt)
//!
//! With direct handling of the output:
//!
//! ```
//! use checked_command::{StatusErrorWithOutput, CheckedCommand};
//! let result = CheckedCommand::new("ls")
//!             .arg("--badbadbad").arg("--")
//!             .output();
//!
//! match result {
//!     Ok(_) => panic!("ls should have failed"),
//!     Err(StatusErrorWithOutput::Io(io_err)) => panic!("a I/O Error occurred {}", io_err),
//!     Err(StatusErrorWithOutput::Failure(ex, output)) => {
//!         println!("exit code should be Some(2) and is: {:?}", ex.code());
//!         println!("stdout of ls:\n{}", String::from_utf8(output.stdout).unwrap());
//!         println!("stderr of ls:\n{}", String::from_utf8(output.stderr).unwrap());
//!     }
//! }
//! ```
//!
//! Without any output handling:
//!
//! ```
//! use checked_command::{ StatusError, CheckedCommand };
//!
//! let result = CheckedCommand::new("ls")
//!                 .arg("--badbadbad").arg("--")
//!                 .status();
//!
//! match result {
//!     Ok(_) => panic!("ls should have failed"),
//!     Err(StatusError::Io(io_err)) => panic!(io_err),
//!     Err(StatusError::Failure(ex)) => {
//!         println!("should be 2 is {:?}", ex.code());
//!     }
//! }
//! ```
//!
//! For easier propagation:
//!
//! ```
//! use checked_command::{ Error, CheckedCommand };
//!
//! fn run_bad_ls() -> Result<(), Error> {
//!     CheckedCommand::new("ls").arg("--badbadbad").arg("--").status()?;
//!     Ok(())
//! }
//!
//! match run_bad_ls() {
//!     Ok(_) => panic!("ls should have failed"),
//!     Err(Error::Io(io_err)) => panic!(io_err),
//!     Err(Error::Failure(_ex, Some(_output))) => println!("we still can handle output"),
//!     Err(Error::Failure(_ex, None)) => println!("no output aviable")
//! }
//! ```
//!
//!
//! # Features
//!
//! ## process_try_wait
//!
//! Requires nightly rust as the rust feature `process_try_wait` is required.
//!
//! Adds a `checked_try_wait` method to `ChildExt` as well as a `try_wait`
//! method to `CheckedChild` (which uses `checked_try_wait` internally).
//! Both methods call the unstable `std::process::Child::try_wait` method
//! internally.
//!
//!
//! ## command_envs
//!
//! Requires nightly rust as the rust feature `command_envs` is required.
//!
//! Adds a `envs` method to `CheckedCommand` which calls the unstable
//! `std::process::Command::env` method.
//!
//!
//! ## use_std_output
//!
//! Works with stable.
//!
//! This crate uses normally it's own `Output` struct, with this option the
//! `std::process::Output` is used instead, which differs in that it has an
//! additional `status: ExitStatus` field.
//!
#![cfg_attr(feature="process_try_wait", feature(process_try_wait))]
#![cfg_attr(feature="command_envs", feature(command_envs))]
#![warn(missing_docs)]


#[macro_use]
extern crate quick_error;

#[cfg(test)]
#[macro_use]
extern crate lazy_static;

#[cfg(test)]
extern crate tutils;

/// internal module containing the CommandExt/ChildExt traints and implementation
#[doc(hidden)]
mod ext;

/// internal module containing the CheckedCommand and CheckedChild wrappers
mod wrapper;

pub use wrapper::CheckedChild;
pub use wrapper::CheckedCommand;

pub use ext::CommandExt;
pub use ext::ChildExt;
pub use ext::Output;
pub use ext::Error;
pub use ext::StatusError;
pub use ext::StatusErrorWithOutput;
