use std::{
    borrow::Cow,
    collections::HashMap,
    env::VarsOs,
    ffi::{OsStr, OsString},
};

/// Used to determine how a env variable of the subprocess should be updated on spawn.
#[derive(Debug, Clone, PartialEq)]
pub enum EnvChange {
    /// Remove the env value if it normally would have been set.
    ///
    /// If no environment is inherited this does nothing.
    ///
    /// (e.g. because of inherited environment)
    Remove,

    /// Make sure the env variable will have given value in the sub-process.
    ///
    /// If environment is inherited this might override a existing value.
    Set(OsString),

    /// Make sure the env variable is inherited from the process spawning the sub-process.
    ///
    /// If environment inheritance is disabled (e.g. using [`Command::with_inherit_env()`]) this
    /// will cause given values to still be inherited.
    ///
    /// If environment inheritance is enabled this won't have any effect.
    ///
    /// This is very useful to have a subprocess with a clean environment while still inheriting
    /// some specific keys.
    Inherit,
}

impl From<&Self> for EnvChange {
    fn from(borrow: &Self) -> Self {
        borrow.clone()
    }
}
impl From<&OsString> for EnvChange {
    fn from(val: &OsString) -> Self {
        EnvChange::Set(val.clone())
    }
}

impl From<OsString> for EnvChange {
    fn from(val: OsString) -> Self {
        EnvChange::Set(val)
    }
}

impl From<&OsStr> for EnvChange {
    fn from(val: &OsStr) -> Self {
        EnvChange::Set(val.into())
    }
}

impl From<String> for EnvChange {
    fn from(val: String) -> Self {
        EnvChange::Set(val.into())
    }
}

impl From<&str> for EnvChange {
    fn from(val: &str) -> Self {
        EnvChange::Set(val.into())
    }
}

type EnvMap = HashMap<OsString, EnvChange>;

pub(crate) fn create_expected_env_iter(
    inherit_env: bool,
    env_updates: &EnvMap,
) -> impl Iterator<Item = (Cow<OsStr>, Cow<OsStr>)> {
    let inherit = if inherit_env {
        Some(::std::env::vars_os())
    } else {
        None
    };

    return ExpectedEnvIter {
        env_updates,
        inherit,
        update: Some(env_updates.iter()),
    };

    //FIXME[rust/generators] use yield base iterator
    struct ExpectedEnvIter<'a> {
        env_updates: &'a EnvMap,
        inherit: Option<VarsOs>,
        update: Option<std::collections::hash_map::Iter<'a, OsString, EnvChange>>,
    }

    impl<'a> Iterator for ExpectedEnvIter<'a> {
        type Item = (Cow<'a, OsStr>, Cow<'a, OsStr>);

        fn next(&mut self) -> Option<Self::Item> {
            loop {
                fused_opt_iter_next!(&mut self.inherit, |(key, val)| {
                    match self.env_updates.get(&key) {
                        Some(_) => continue,
                        None => return Some((Cow::Owned(key), Cow::Owned(val))),
                    }
                });
                fused_opt_iter_next!(&mut self.update, |(key, change)| {
                    match change {
                        EnvChange::Set(val) => {
                            return Some((Cow::Borrowed(&key), Cow::Borrowed(&val)));
                        }
                        EnvChange::Inherit => {
                            // Mostly used if inherit_var is valse in which case we *should* not
                            // have done aboves loop-part on vars_os. We could "optimize" this to
                            // handle Inherit in aboves loop if we run that loop, but why add that
                            // complexity?
                            if let Some(val) = ::std::env::var_os(&key) {
                                return Some((Cow::Borrowed(&key), Cow::Owned(val)));
                            } else {
                                continue;
                            }
                        }
                        EnvChange::Remove => {
                            continue;
                        }
                    }
                });
                return None;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    proptest! {
        #[test]
        fn env_variables_can_be_set_to_be_removed_from_inherited_env(
            rem_key in proptest::sample::select(std::env::vars_os().map(|(k,_v)| k).collect::<Vec<_>>())
        ) {
            let mut env_updates = EnvMap::default();
            env_updates.insert(rem_key.clone(), EnvChange::Remove);

            let produced_env = create_expected_env_iter(true, &&env_updates)
                .map(|(k,v)| (k.into_owned(), v.into_owned()))
                .collect::<HashMap<OsString, OsString>>();

            //FIXME on CI this test can leak secrets if it fails
            prop_assert_eq!(produced_env.get(&rem_key), None);
        }


        #[test]
        fn env_variables_can_be_set_to_be_replaced_from_inherited_env(
            env_key in proptest::sample::select(std::env::vars_os().map(|(k,_v)| k).collect::<Vec<_>>()),
            replacement in any::<OsString>()
        ) {
            let mut env_updates = EnvMap::default();
            env_updates.insert(env_key.clone(), EnvChange::Set(replacement.clone()));

            let produced_env = create_expected_env_iter(true, &&env_updates)
                .map(|(k,v)| (k.into_owned(), v.into_owned()))
                .collect::<HashMap<OsString, OsString>>();

            //FIXME on CI this test can leak secrets if it fails
            prop_assert_eq!(produced_env.get(&env_key), Some(&replacement));
        }

        #[test]
        fn env_variables_can_be_set_to_inherit_even_if_inheritance_is_disabled(
            inherit in proptest::sample::select(std::env::vars_os().map(|(k,_v)| k).collect::<Vec<_>>()),
        )  {
            let expected_val = std::env::var_os(&inherit);
            let mut env_updates = EnvMap::default();
            env_updates.insert(inherit.clone(), EnvChange::Inherit);

            let new_env = create_expected_env_iter(false, &env_updates).collect::<Vec<_>>();
            prop_assert_eq!(new_env.len(), 1);

            let got_value = new_env.iter()
                .find(|(k,_v)| &*k==&*inherit)
                .map(|(_k,v)| &**v);

            //FIXME on CI this test can leak secrets if it fails
            prop_assert_eq!(got_value, expected_val.as_ref().map(|v|&**v));
        }

        #[test]
        fn setting_inherit_does_not_affect_anything_if_we_anyway_inherit_all(
            pointless_inherit in proptest::sample::select(std::env::vars_os().map(|(k,_v)| k).collect::<Vec<_>>()),
        ) {
            const NON_EXISTING_VAR_KEY: &'static str = "____MAPPED_COMMAND__THIS_SHOULD_NOT_EXIST_AS_ENV_VARIABLE____";
            prop_assert_eq!(std::env::var_os(NON_EXISTING_VAR_KEY), None);

            let expected_values = std::env::vars_os()
                .map(|(k,v)| (Cow::Owned(k), Cow::Owned(v)))
                .collect::<HashMap<_,_>>();

            let mut update_env = EnvMap::default();
            update_env.insert(pointless_inherit.clone(), EnvChange::Inherit);
            update_env.insert(NON_EXISTING_VAR_KEY.into(), EnvChange::Inherit);

            let values = create_expected_env_iter(true, &update_env).collect::<HashMap<_,_>>();

            assert!(!values.contains_key(OsStr::new(NON_EXISTING_VAR_KEY)));
            assert_eq!(expected_values.len(), values.len());

            //FIXME on CI this test can leak secrets if it fails
            assert_eq!(
                expected_values.get(&pointless_inherit),
                values.get(&*pointless_inherit)
            );
        }
    }
}
