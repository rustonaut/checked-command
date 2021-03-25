use std::fmt::{self, Display};

/// A ExitStatus type similar to `std::process::ExitStatus` but which can be created (e.g. for testing).
///
/// # Display
///
/// If there is an exit code it will always be displayed as hexadecimal.
/// This is done because of two reasons:
///
/// - Some platforms allow rather large exit codes which are just very unreadable (and unrecognizable) in decimal formatting.
/// - The hex format is always bit based which removes confusions around differences between targets having signed and unsigned
///   exit codes. The drawback is that on platforms which do allow negative exit codes you need to convert the number not just
///   from hex to decimal but also consider signing wrt. the max supported unsigned size on that platform.
///
/// An target specific exit status is displayed in a target specific way.
/// The non os specific fallback defaults to displaying `NO_exit_status`.
/// A signal termination exit status on unix will be displayed as e.g.
/// `signal(9)`.
///
///
/// # Os Support
///
/// For now part of this type are only supported for targets of the os families
/// windows and unix(-like).
///
/// If you need support for _any_ other OS feel free to open an issue, I will
/// add the necessary code path for the methods which are not OS independent
/// then.
///
/// Currently this only affects the [`ExitStatus::successful()`] method.
///
/// # Why not `std::process::ExitStatus`?
///
/// The standard library and this library have different design goals, most
/// importantly this library can introduce braking changes while the standard
/// library ones can't really do so.
///
/// Major differences include:
///
/// - Just one enum instead of an `.exit_status() -> Option<i32>` accessor.
/// - Implements `PartialEq<RHS>` for various numbers making testing easier.
/// - Has a platform independent constructor, `std::process::ExitStatus` has
///   various platform specific constructors.
/// - Uses `i64` as exit code to more correctly represents exits codes (see below).
///
/// ## Incompatibilities and limitations.
///
/// Due to the current structures a various exit codes can be constructed which
/// are not possible to appear on the target you are currently compiling against.
/// **This is already true for the std implementation, but with slightly less
/// constraints in our case.** For example if you target linux you can still
/// create a exit code > 0xFF but in linux no exit code > 0xFF can be returned (
/// furthermore returning any exit code > 127 is a cause of unexpected problems
/// and must be avoided).
///
/// Furthermore `std::process::Command` returning a i32 code is a major problem
/// as it's incompatible with various platforms:
///
/// - Windows has a `u32`! exit code, rust std's Command does reinterpret
///   it as `i32` when returning it which in some cases lead to negative
///   exit codes even through there *are not negative exit codes on windows*.
///   Furthermore Fushisa does have a i64 exit status which they currently
///   can't handle at all. *Be aware that this library still uses
///   `std::process::Command` internally and a such can't handle this either*.
///   But we do "fix" the exit code so that an exit code of `u32::MAX` is still
///   `u32::MAX` and not `-1`!.
///
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum ExitStatus {
    /// The process exited with an exit code.
    ///
    /// As this allows any i64 this allows you to create an exit status which can not
    /// appear on the current target. (This is also possible with the standard libraries
    /// `ExitStatus`). This makes testing easier and allows you to test cases of exit
    /// codes which can't appear on your but other platforms.
    ///
    /// # Differences to `std::process::ExitStatus`
    ///
    /// This uses a `i64` as this allows a more correct representation of exit codes.
    ///
    /// *On windows a exit code > `i32::MAX` will be correctly be represented as such
    /// instead of wrongly being displayed as negative number.*
    Code(i64),

    /// An exit status which isn't a simple exit code was returned.
    ///
    /// On unix if a process was directly terminated via an signal no exit code was
    /// set but the signal causing the exit is returned (encoded with other values
    /// in the raw exit status).
    ///
    /// Rust represents this separately as depending on the exact unix-like operating
    /// system it might be encoded in different ways, in some cases instead of a integer
    /// encoding the status a struct with multiple fields is returned, as such there
    /// is no correct or reliable way to encode exit an status just as an number.
    ///
    /// Be aware that for testability [`OpaqueOsExitStatus`] can be created on all platforms
    /// even through e.g. on windows there are only exit codes! Note that the exact inner
    /// implementation of [`OpaqueOsExitStatus`] is platform dependent, but it implements
    /// [`arbitrary_default()`](OpaqueOsExitStatus::target_specific_default())
    OsSpecific(OpaqueOsExitStatus),
}

impl ExitStatus {
    /// Returns true if the command did succeed.
    ///
    /// As not all operating systems use 0 == success we need to have platform
    /// specific code for all of them. Which is infeasible and as such this is
    /// only enabled on the unix and window target family. (Note that windows
    /// and unix are currently the only target families as e.g. linux, all BSD's,
    /// OsX, iOs are unix-like enough to count as part of the unix family).
    #[cfg(any(window, unix))]
    pub fn successful(&self) -> bool {
        match self {
            Self::Code(code) if *code == 0 => true,
            _ => false,
        }
    }
}

impl Display for ExitStatus {
    fn fmt(&self, fter: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Code(code) => write!(fter, "0x{:X}", code),
            Self::OsSpecific(alt) => Display::fmt(alt, fter),
        }
    }
}

impl Default for ExitStatus {
    fn default() -> Self {
        Self::Code(0)
    }
}

impl From<OpaqueOsExitStatus> for ExitStatus {
    fn from(ooes: OpaqueOsExitStatus) -> Self {
        ExitStatus::OsSpecific(ooes)
    }
}

macro_rules! impl_from_and_partial_eq_for_fitting_int {
    ($($int:ty),*) => ($(
        impl From<$int> for ExitStatus {
            fn from(code: $int) -> Self {
                Self::Code(code as _)
            }
        }

        impl PartialEq<$int> for ExitStatus {
            fn eq(&self, other: &$int) -> bool {
                match self {
                    Self::Code(code) => *code == *other as i64,
                    Self::OsSpecific(_) => false,
                }
            }
        }
    )*);
}

impl_from_and_partial_eq_for_fitting_int!(u8, i8, u16, i16, u32, i32, i64);
/// A platform specific opaque exit status.
///
/// An exit status which is not an exit code, e.g.
/// on unix the signal which terminated an process
/// preventing it from exiting with an exit status.
///
/// **Warning: Besides [`OpaqueOsExitStatus::target_specific_default()`]
/// all other methods only exist on _some_ targets but not all.** As such
/// using them can lead to code which only compiles on some targets.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct OpaqueOsExitStatus {
    #[cfg(not(unix))]
    _priv: (),
    #[cfg(unix)]
    signal: i32,
}

impl OpaqueOsExitStatus {
    /// Creates a instance of this type.
    ///
    /// This is meant for allowing non-platform specific tests which
    /// handle the case of a non exit code process exit status.
    ///
    /// Platform specific tests likely still are needed as what
    /// this type means is platform specific.
    ///
    /// This will always create the same default value but it's
    /// a target_specific_value *and* it's picked arbitrary so
    /// it's not really appropriately to implement [`Default`].
    /// (To make clear why it isn't consider `u32` would default
    /// to `246` or similar arbitrary value.)
    pub fn target_specific_default() -> Self {
        Self {
            #[cfg(not(unix))]
            _priv: (),
            #[cfg(unix)]
            signal: 9,
        }
    }

    /// Return the signal number which did lead to the process termination.
    #[cfg(unix)]
    pub fn signal_number(&self) -> i32 {
        self.signal
    }

    /// Create a unix [`OpaqueOsExitStatus`] instance based on the signal code
    /// causing the non exit code termination.
    ///
    /// Like some other aspects you can define (and test) unrealistic signal numbers.
    /// IMHO this is better (more simple, flexible etc.) then to have a result which
    /// is potentially target dependent or a implicit target dependent bit masking.
    ///
    // E.g. on linux and most (all) unix it's limited to 7 bit (&0x7f) but on at least
    // OpenBSD the value 0x7F is reserved and doesn't count as signal (the macro for
    // testing if it exited with an signal excludes it). Also in any case 0 is not a
    // valid signal either.
    //
    // POSIX defines signals as `int` and with this more or less as i32, but this seems to
    // be because of practical reasons i.e. bitmasking a i32 produces a i32. I do not think
    // there are any negative signals at all, nor do there seem to be any platforms with more
    // than a handful of valid signals.
    #[cfg(unix)]
    pub fn from_signal_number(signal: i32) -> Self {
        Self { signal }
    }
}

impl Display for OpaqueOsExitStatus {
    fn fmt(&self, fter: &mut fmt::Formatter) -> fmt::Result {
        #[cfg(not(unix))]
        {
            fter.write_str("NO_EXIT_CODE")
        }
        #[cfg(unix)]
        {
            write!(fter, "signal({})", self.signal)
        }
    }
}

#[cfg(test)]
mod tests {

    mod ExitStatus {
        #![allow(non_snake_case)]

        mod display_fmt {
            use crate::{ExitStatus, OpaqueOsExitStatus};

            #[test]
            fn format_exit_status_as_hex() {
                let exit_status = ExitStatus::from(0x7Fi32);
                assert_eq!(&format!("{}", exit_status), "0x7F");
            }

            #[test]
            fn format_negative_exit_status_as_hex() {
                let exit_status = ExitStatus::Code(-1i32 as u32 as _);
                assert_eq!(&format!("{}", exit_status), "0xFFFFFFFF");
            }

            #[test]
            #[cfg(unix)]
            fn display_for_non_exit_code_on_unix() {
                let signal = OpaqueOsExitStatus::from_signal_number(9);
                assert_eq!(&format!("{}", signal), "signal(9)");
            }
        }

        mod new {
            use crate::ExitStatus;

            #[test]
            fn can_be_create_from_many_numbers() {
                let status = ExitStatus::from(12u8);
                assert_eq!(status, ExitStatus::Code(12));
                let status = ExitStatus::from(-12i8);
                assert_eq!(status, ExitStatus::Code(-12));
                let status = ExitStatus::from(12u16);
                assert_eq!(status, ExitStatus::Code(12));
                let status = ExitStatus::from(-12i16);
                assert_eq!(status, ExitStatus::Code(-12));
                let status = ExitStatus::from(u32::MAX);
                assert_eq!(status, ExitStatus::Code(u32::MAX as i64));
                let status = ExitStatus::from(-1i32);
                assert_eq!(status, ExitStatus::Code(-1));
                let status = ExitStatus::from(-13i64);
                assert_eq!(status, ExitStatus::Code(-13));
            }

            #[test]
            fn can_compare_to_many_numbers() {
                let status = ExitStatus::from(12u8);
                assert_eq!(status, 12u8);
                let status = ExitStatus::from(-12i8);
                assert_eq!(status, -12i8);
                let status = ExitStatus::from(12u16);
                assert_eq!(status, 12u16);
                let status = ExitStatus::from(-12i16);
                assert_eq!(status, -12i16);
                let status = ExitStatus::from(u32::MAX);
                assert_eq!(status, u32::MAX);
                let status = ExitStatus::from(-1i32);
                assert_eq!(status, -1i32);
                let status = ExitStatus::from(-13i64);
                assert_eq!(status, -13i64);
            }
        }
    }

    #[cfg(unix)]
    mod signal_number {
        use proptest::prelude::*;

        use crate::OpaqueOsExitStatus;

        proptest! {
            #[test]
            fn from_to_signal_number(
                nr in any::<i32>()
            ) {
                let exit_status = OpaqueOsExitStatus::from_signal_number(nr);
                assert_eq!(exit_status.signal_number(), nr);
            }

        }
    }
}
