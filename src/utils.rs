#[cfg(test)]
use ::{
    proptest::{arbitrary::any, prelude::*, strategy::Strategy},
    std::{ffi::OsString, path::PathBuf},
};

#[macro_export]
macro_rules! fused_opt_iter_next {
    ($source:expr, |$name:pat| $code:block) => {{
        let mut drop_iterator = false;
        if let Some(iter) = $source.as_mut() {
            if let Some($name) = iter.next() {
                $code
            } else {
                drop_iterator = true;
            }
        }
        if drop_iterator {
            *$source = None;
        }
    }};
}

#[cfg(test)]
pub fn opt_arbitrary_path_buf() -> impl Strategy<Value = Option<PathBuf>> {
    prop_oneof![
        any::<OsString>().prop_map(|v| Some(PathBuf::from(v))),
        Just(None)
    ]
}

#[cfg(test)]
mod tests {

    mod fused_opt_iter_next {

        #[test]
        fn sets_iterator_to_null_if_empty() {
            let mut iter = Some(Vec::<String>::new().into_iter());

            fused_opt_iter_next!(&mut iter, |_val| {
                panic!("unexpected call");
            });

            assert!(iter.is_none());
        }

        #[test]
        fn calls_the_closure_until_the_iterator_is_empty() {
            let mut iter = Some(vec![1, 2, 3].into_iter());
            let mut sum = 0;
            loop {
                fused_opt_iter_next!(&mut iter, |val| {
                    sum += val;
                    continue;
                });
                break;
            }
            assert_eq!(sum, 6);
        }

        #[test]
        fn can_use_patterns_in_the_face_closure() {
            let mut iter = Some(vec![(1, 3), (2, 8), (3, 7)].into_iter());
            let mut sum = 0;
            loop {
                fused_opt_iter_next!(&mut iter, |(a, b)| {
                    sum += a * b;
                    continue;
                });
                break;
            }
            assert_eq!(sum, 40);
        }
    }
}
