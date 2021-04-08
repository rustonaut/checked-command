use std::{
    fmt::Debug,
    ops::{Deref, DerefMut},
};

#[cfg(test)]
use ::{
    proptest::{arbitrary::any, prelude::*, strategy::Strategy},
    std::{ffi::OsString, path::PathBuf},
};

#[cfg(test)]
pub fn opt_arbitrary_path_buf() -> impl Strategy<Value = Option<PathBuf>> {
    prop_oneof![
        any::<OsString>().prop_map(|v| Some(PathBuf::from(v))),
        Just(None)
    ]
}

/// Use instead of field reference to omit content of field in Debug implementation.
#[repr(transparent)]
pub struct NoDebug<T>(pub T);

impl<T> Debug for NoDebug<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("/*omitted, no Debug::fmt*/")
    }
}

impl<T> Deref for NoDebug<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for NoDebug<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T> From<T> for NoDebug<T> {
    fn from(v: T) -> Self {
        NoDebug(v)
    }
}

#[cfg(test)]
mod tests {
    #[allow(non_snake_case)]

    mod NoDebugField {
        use super::super::*;

        #[test]
        fn implements_deref_deref_mut_and_from() {
            let mut field = NoDebug::from("hy".to_owned());
            let _: &String = &field;
            let _: &mut String = &mut field;
        }

        #[test]
        fn allow_skipping_field_content_in_debug_fmt() {
            let value = A {
                field: NoDebug("hy"),
            };
            assert_eq!(
                format!("{:?}", value),
                "A { field: /*omitted, no Debug::fmt*/ }"
            );

            #[derive(Debug)]
            struct A {
                field: NoDebug<&'static str>,
            }
        }
    }
}
