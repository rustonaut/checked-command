//! In std the methods used to retrieve the `ExitStatus`/`Output`
//! from executing a `Command` do not consider the exit status
//! when deciding weather or not the function returns a error.
//!
//! This creates provides:
//!
//! 1. A `CheckedCommand` and `CheckedChild` struct, which wrap `std::process::Command` and
//!    `std::process::Child` replacing `status()`,`output()`,`wait()` and `wait_with_output()`
//!    with a version which will check the `ExitStatus` and if it didn't succeed
//!    they will return a `Err(...)` instead of a `Ok(...)`.
//!
//! 2. A `CommandExt` and `ChildExt` extension trait which provide versions of `status()`,
//!    `output()` etc. which check the `ExitStatus` returning a `Err(...)` if the exit status
//!    is non successful (i.e. there is no exit code or  it is not equal zero). The
//!    checked methods are `checked_status()`, `checked_output()`, `checked_wait()`,
//!    `checked_wait_with_output()`. The `CheckedCommand` and `CheckedChild` wrapper use
//!    this methods as their `status()`/`output()` etc. methods.
//!
//! In case of functions originally returning a `Output` it might be necessary to process
//! the `Output` even if the command returned a non-successful exit status. For this reason
//! the `Output` is included into the error `Error::Failure` variant (as option as not all
//! functions provide a output)
//!
//! Note that the provided functions do return their own `Output` struct instead of
//! `std::process::Output` which differs in that it does not contain a `status` field
//! (which is also not anymore needed for the new methods). There is `use_std_output`
//! feature which will make the crate use the std's output implementation instead.
//!
//! # Example (CheckedCommand)
//!
//! With direct handling of the output:
//!
//! ```
//! use checked_command::{ Error, CheckedCommand };
//!
//! let result = CheckedCommand::new("ls")
//!                 .arg("--badbadbad").arg("--")
//!                 .output();
//!
//! match result {
//!     Ok(_) => panic!("ls should have failed"),
//!     Err(Error::Io(io_err)) => panic!("unexpected I/O Error: {:?}", io_err),
//!     Err(Error::Failure(ex, output)) => {
//!         println!("failed with exit code: {:?}", ex.code());
//!         if let Some(output) = output {
//!             println!("error output was:\n{}", String::from_utf8_lossy(&*output.stderr));
//!         }
//!     }
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


/// internal module containing the CommandExt/ChildExt traints and implementation
#[doc(hidden)]
mod ext;

/// internal module containing the CheckedCommand and CheckedChild wrappers
#[doc(hidden)]
mod wrapper;

pub use wrapper::CheckedChild;
pub use wrapper::CheckedCommand;

pub use ext::CommandExt;
pub use ext::ChildExt;
pub use ext::Output;
pub use ext::Error;