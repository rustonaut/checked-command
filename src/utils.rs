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
