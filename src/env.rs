use std::{
    borrow::Cow,
    collections::HashMap,
    env::VarsOs,
    ffi::{OsStr, OsString},
};

/// Used to determine how a env variable of the subprocess should be updated on spawn.
#[derive(Debug, Clone, PartialEq)]
pub enum EnvUpdate {
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

impl From<&Self> for EnvUpdate {
    fn from(borrow: &Self) -> Self {
        borrow.clone()
    }
}
impl From<&OsString> for EnvUpdate {
    fn from(val: &OsString) -> Self {
        EnvUpdate::Set(val.clone())
    }
}

impl From<OsString> for EnvUpdate {
    fn from(val: OsString) -> Self {
        EnvUpdate::Set(val)
    }
}

impl From<&OsStr> for EnvUpdate {
    fn from(val: &OsStr) -> Self {
        EnvUpdate::Set(val.into())
    }
}

impl From<String> for EnvUpdate {
    fn from(val: String) -> Self {
        EnvUpdate::Set(val.into())
    }
}

impl From<&str> for EnvUpdate {
    fn from(val: &str) -> Self {
        EnvUpdate::Set(val.into())
    }
}

/// This type is used to determine how to create the environment for the child process
#[derive(Debug, PartialEq)]
pub struct EnvBuilder {
    inherit_env: bool,
    updates: HashMap<OsString, EnvUpdate>,
}

impl EnvBuilder {
    /// Creates a new env builder.
    ///
    /// This will create a new env builder which by default
    /// will inherit the parent processes env and doesn't
    /// do any updates to it. Through this behavior can be
    /// changed.
    pub fn new() -> Self {
        Self {
            inherit_env: true,
            updates: Default::default(),
        }
    }

    /// Inserts a given `EnvUpdate` for given variable into the builder.
    ///
    /// **Warning:** Validity of the name is not checked! Invalid names can lead to problems
    /// like spawning the child process failing because of it.
    ///
    /// This update is applied wen creating the child process environment.
    pub fn insert_update(
        &mut self,
        name: impl Into<OsString>,
        var: impl Into<EnvUpdate>,
    ) -> &mut Self {
        self.updates.insert(name.into(), var.into());
        self
    }

    /// The given variable will be set to the given value in the child process environment.
    ///
    /// **Warning:** Validity of the name is not checked! Invalid names can lead to problems
    /// like spawning the child process failing because of it.
    ///
    /// This is equivalent to `self.insert_update(name, EnvUpdate::Set(var))`.
    #[inline(always)]
    pub fn set_var(&mut self, name: impl Into<OsString>, var: impl Into<OsString>) -> &mut Self {
        self.insert_update(name.into(), EnvUpdate::Set(var.into()))
    }

    /// The given variable will always be inherited from the parent.
    ///
    /// It even inherits it if normally no variables will be inherited!
    ///
    /// **Warning:** Validity of the name is not checked! Invalid names can lead to problems
    /// like spawning the child process failing because of it.
    ///
    /// This is equivalent to `self.insert_update(name.into(), EnvUpdate::Inherit);`
    #[inline(always)]
    pub fn always_inherit_var(&mut self, name: impl Into<OsString>) -> &mut Self {
        self.insert_update(name.into(), EnvUpdate::Inherit)
    }

    /// The given variable will be removed even if it normally would be inherited.
    ///
    /// **Warning:** Validity of the name is not checked! Invalid names can lead to problems
    /// like spawning the child process failing because of it.
    ///
    /// This is equivalent to `self.insert_update(name.into(), EnvUpdate::Remove);`
    #[inline(always)]
    pub fn remove_var(&mut self, name: impl Into<OsString>) -> &mut Self {
        self.insert_update(name.into(), EnvUpdate::Remove)
    }

    /// If the parent env will be inherited this var will be inherit, if no it won't.
    ///
    /// This is the default behavior for all variables.
    ///
    /// This basically just removes any entry to the inner map of updates, i.e. this
    /// undoes any direct or indirect call to [`EnvBuilder::insert_update()`] previously
    /// done.
    #[inline(always)]
    pub fn use_default_behavior_on_var(&mut self, name: impl AsRef<OsStr>) -> &mut Self {
        self.updates.remove(name.as_ref());
        self
    }

    /// If set to true the parent process environment will be inherited.
    ///
    /// If set to false the child processes environment will be build from a empty
    /// environment instead (updates like specified through calls to
    /// [`EnvBuilder::insert_update()`] will still be applied on it.)
    pub fn set_inherit_env(&mut self, inherit_env: bool) -> &mut Self {
        self.inherit_env = inherit_env;
        self
    }

    /// Return a iterator over all updates.
    ///
    /// This is an helper for testing.
    pub fn env_updates_iter(
        &self,
    ) -> impl Iterator<Item = (&OsStr, &EnvUpdate)> + ExactSizeIterator {
        self.updates.iter().map(|(k, v)| (&**k, v))
    }

    /// Returns the state of the `inherit_env` flag.
    pub fn inherit_env(&self) -> bool {
        self.inherit_env
    }

    pub(crate) fn create_expected_env_iter(
        &self,
    ) -> impl Iterator<Item = (Cow<OsStr>, Cow<OsStr>)> {
        let inherit = if self.inherit_env {
            Some(::std::env::vars_os())
        } else {
            None
        };

        return ExpectedEnvIter {
            env_updates: &self.updates,
            inherit,
            update: Some(self.updates.iter()),
        };

        //FIXME[rust/generators] use yield base iterator
        struct ExpectedEnvIter<'a> {
            env_updates: &'a HashMap<OsString, EnvUpdate>,
            inherit: Option<VarsOs>,
            update: Option<std::collections::hash_map::Iter<'a, OsString, EnvUpdate>>,
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
                            EnvUpdate::Set(val) => {
                                return Some((Cow::Borrowed(&key), Cow::Borrowed(&val)));
                            }
                            EnvUpdate::Inherit => {
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
                            EnvUpdate::Remove => {
                                continue;
                            }
                        }
                    });
                    return None;
                }
            }
        }
    }
}

impl Default for EnvBuilder {
    fn default() -> Self {
        EnvBuilder::new()
    }
}

impl<K, V> Extend<(K, V)> for EnvBuilder
where
    K: Into<OsString>,
    V: Into<EnvUpdate>,
{
    fn extend<T: IntoIterator<Item = (K, V)>>(&mut self, iter: T) {
        self.updates
            .extend(iter.into_iter().map(|(k, v)| (k.into(), v.into())))
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
            let mut env_builder = EnvBuilder::default();
            env_builder.remove_var(&rem_key);


            let produced_env = env_builder.create_expected_env_iter()
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
            let mut env_builder = EnvBuilder::default();
            env_builder.set_var(env_key.clone(), replacement.clone());

            let produced_env = env_builder.create_expected_env_iter()
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
            let mut env_builder = EnvBuilder::default();
            env_builder
                .set_inherit_env(false)
                .always_inherit_var(inherit.clone());

            assert_eq!(env_builder.inherit_env(), false);

            let new_env = env_builder.create_expected_env_iter().collect::<Vec<_>>();
            prop_assert_eq!(new_env.len(), 1);

            let got_value = new_env.iter()
                .find(|(k,_v)| &*k==&*inherit)
                .map(|(_k,v)| &**v);

            //FIXME on CI this test can leak secrets if it fails
            prop_assert_eq!(got_value, expected_val.as_ref().map(|v|&**v));
        }

        #[test]
        fn setting_inherit_does_not_affect_anything_if_we_anyway_inherit_all(
            pointless_inherit in proptest::sample::select(std::env::vars_os().map(|(k,_v)| k).collect::<Vec<OsString>>()),
        ) {
            const NON_EXISTING_VAR_KEY: &'static str = "____MAPPED_COMMAND__THIS_SHOULD_NOT_EXIST_AS_ENV_VARIABLE____";
            prop_assert_eq!(std::env::var_os(NON_EXISTING_VAR_KEY), None);

            let expected_values = std::env::vars_os()
                .map(|(k,v)| (Cow::Owned(k), Cow::Owned(v)))
                .collect::<HashMap<_,_>>();

            let mut env_builder = EnvBuilder::default();
            env_builder
                .always_inherit_var(pointless_inherit.clone())
                .always_inherit_var(NON_EXISTING_VAR_KEY);

                assert_eq!(env_builder.inherit_env(), true);

            let values = env_builder.create_expected_env_iter().collect::<HashMap<_,_>>();

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
