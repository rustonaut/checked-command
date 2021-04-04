use std::{
    collections::HashMap,
    ffi::{OsStr, OsString},
};

/// Used to determine how a env variable of the subprocess should be updated on spawn.
#[derive(Debug, Clone, PartialEq)]
//FIXME strategy which limits the value (which for all tests is basically opaque)
#[cfg_attr(test, derive(proptest_derive::Arbitrary))]
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
    Set(
        #[cfg_attr(
            test,
            proptest(
                strategy = "{use proptest::prelude::*; \"[^\u{0}]{0,4}\".prop_map(OsString::from) }"
            )
        )]
        OsString,
    ),

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
#[derive(Debug, PartialEq, Clone)]
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
    pub fn insert_update(&mut self, name: impl Into<OsString>, var: impl Into<EnvUpdate>) {
        self.updates.insert(name.into(), var.into());
    }

    /// The given variable will be set to the given value in the child process environment.
    ///
    /// **Warning:** Validity of the name is not checked! Invalid names can lead to problems
    /// like spawning the child process failing because of it.
    ///
    /// This is equivalent to `self.insert_update(name, EnvUpdate::Set(var))`.
    #[inline(always)]
    pub fn set_var(&mut self, name: impl Into<OsString>, var: impl Into<OsString>) {
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
    pub fn always_inherit_var(&mut self, name: impl Into<OsString>) {
        self.insert_update(name.into(), EnvUpdate::Inherit)
    }

    /// The given variable will be removed even if it normally would be inherited.
    ///
    /// **Warning:** Validity of the name is not checked! Invalid names can lead to problems
    /// like spawning the child process failing because of it.
    ///
    /// This is equivalent to `self.insert_update(name.into(), EnvUpdate::Remove);`
    #[inline(always)]
    pub fn remove_var(&mut self, name: impl Into<OsString>) {
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
    pub fn use_default_behavior_on_var(&mut self, name: impl AsRef<OsStr>) {
        self.updates.remove(name.as_ref());
    }

    /// If set to true the parent process environment will be inherited.
    ///
    /// If set to false the child processes environment will be build from a empty
    /// environment instead (updates like specified through calls to
    /// [`EnvBuilder::insert_update()`] will still be applied on it.)
    pub fn set_inherit_env(&mut self, inherit_env: bool) {
        self.inherit_env = inherit_env;
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

    //TODO update doc
    // /// Returns a map with all env variables the sub-process spawned by this command would have
    // /// if the current processes env is not changed.
    // ///
    // /// # Site note about `env::set_var()` problems
    // ///
    // /// Note that if you use `std::env::set_var()` in a multi-threaded setup depending on
    // /// the operating system you run this on this can lead to all kind of problem, including
    // /// unexpected race conditions in some situations (especially if `inherit_env(true)` is
    // /// combined with `EnvChange::Inherit` and multiple variables are changed in another thread
    // /// racing with this function and some but not all are covered by `EnvChange::Inherit`).
    // ///
    // /// Given that [`std::env::set_var()`] should strictly be avoided in a multi-threaded context
    // /// this is seen as an acceptable drawback.
    // ///
    // /// Note that this function + `std::env::set_var()` is not unsafe it might just have a
    // /// very unexpected result. Except if `env::set_var()` + reading env races are inherently
    // /// unsafe on your system, in which case this has nothing to do with this function.

    /// Builds the environment for the given child.
    pub fn build_on(self, child_env: &mut impl ApplyChildEnv) {
        child_env.do_inherit_env(self.inherit_env);

        for (name, update) in self.updates {
            match update {
                EnvUpdate::Remove => {
                    child_env.remove_var(&name);
                }
                EnvUpdate::Set(value) => child_env.set_var(name, value),
                EnvUpdate::Inherit => {
                    if !self.inherit_env {
                        child_env.explicitly_inherit(name)
                    }
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

/// Interface abstracting ways to set a child process environment
pub trait ApplyChildEnv {
    fn do_inherit_env(&mut self, inherit_env: bool);
    fn set_var(&mut self, var: OsString, value: OsString);
    fn remove_var(&mut self, var: &OsStr);
    fn explicitly_inherit(&mut self, name: OsString);
}

impl ApplyChildEnv for HashMap<OsString, OsString> {
    fn do_inherit_env(&mut self, inherit_env: bool) {
        if inherit_env {
            self.extend(std::env::vars_os())
        }
    }

    fn set_var(&mut self, var: OsString, value: OsString) {
        self.insert(var, value);
    }

    fn remove_var(&mut self, var: &OsStr) {
        self.remove(var);
    }

    fn explicitly_inherit(&mut self, var: OsString) {
        if let Some(value) = std::env::var_os(&var) {
            self.set_var(var, value);
        }
    }
}

impl ApplyChildEnv for EnvBuilder {
    fn do_inherit_env(&mut self, inherit_env: bool) {
        self.set_inherit_env(inherit_env);
    }

    fn set_var(&mut self, var: OsString, value: OsString) {
        EnvBuilder::set_var(self, var, value);
    }

    fn remove_var(&mut self, var: &OsStr) {
        EnvBuilder::remove_var(self, var);
    }

    fn explicitly_inherit(&mut self, name: OsString) {
        EnvBuilder::always_inherit_var(self, name);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    //FIXME refactor tests

    #[test]
    fn impls_clone() {
        let mut a = EnvBuilder::new();
        a.set_var("hy", "ho");
        let b = a.clone();
        assert_eq!(a, b);
    }

    mod hash_map_implements_child_env {
        use super::*;

        #[test]
        fn inherit_env_filles_it_with_current_env() {
            let mut env = HashMap::<OsString, OsString>::new();

            env.do_inherit_env(true);

            let expected = std::env::vars_os().collect::<HashMap<OsString, OsString>>();
            assert_eq!(env, expected);
        }

        #[test]
        fn not_inheriting_env_does_nothing() {
            let mut env = HashMap::<OsString, OsString>::new();
            env.do_inherit_env(false);
            assert!(env.is_empty());
        }

        #[test]
        fn set_sets_a_value() {
            let mut env = HashMap::<OsString, OsString>::new();
            env.set_var("a".into(), "b".into());
            assert_eq!(env.get(OsStr::new("a")), Some(&"b".into()));
        }

        #[test]
        fn remove_removes_a_value() {
            let mut env = HashMap::<OsString, OsString>::new();
            env.set_var("a".into(), "b".into());
            assert_eq!(env.get(OsStr::new("a")), Some(&"b".into()));
            env.remove_var(OsStr::new("a"));
            assert_eq!(env.get(OsStr::new("a")), None);
        }

        #[derive(proptest_derive::Arbitrary, Debug)]
        enum SetRemoveOp {
            //Hint: If I don't limit this it will test to many cases, and
            //      The difference is the test taking 0.6s or 10s...
            Set(
                #[proptest(regex = "[^\u{0}]{0,4}")] String,
                #[proptest(regex = "[^\u{0}]{0,4}")] String,
            ),
            Remove(#[proptest(regex = "[^\u{0}]{0,4}")] String),
        }

        proptest! {
            #[test]
            fn set_remove_var_behave_like_insert_remove(
                ops in any::<Vec<SetRemoveOp>>()
            ) {
                use self::SetRemoveOp::*;

                let mut env = HashMap::new();
                let mut expected = HashMap::new();

                for op in ops.into_iter() {
                    match op {
                        Set(key, value) => {
                            env.set_var(OsString::from(&key), OsString::from(&value));
                            expected.insert(OsString::from(key), OsString::from(value));
                        },
                        Remove(key) => {
                            let key = OsStr::new(&key);
                            env.remove_var(key);
                            expected.remove(key);
                        }
                    }
                }

                assert_eq!(env, expected);
            }
        }
    }

    proptest! {

        #[test]
        fn the_child_environment_is_build_correctly(
            inherit in any::<bool>(),
            updates in prop::collection::hash_map(
                prop::string::string_regex("[^\u{0}]{0,4}").unwrap().prop_map(OsString::from),
                any::<EnvUpdate>(), 0..10),
        ) {
            //FIXME very unstable as EnvUpdate::Inherit involves the calling environment
            //FIXME might leak secret ENV variables , e.g. in CI
            let mut child_env = HashMap::<OsString, OsString>::default();
            let mut env_builder = EnvBuilder::default();
            env_builder.set_inherit_env(inherit);
            env_builder.extend(updates.iter());
            env_builder.build_on(&mut child_env);

            let mut expected = if inherit {
                std::env::vars_os().collect::<HashMap<_,_>>()
            } else {
                HashMap::new()
            };

            for (name, update) in updates {
                match update {
                    EnvUpdate::Remove => {
                        expected.remove(&name);
                    },
                    EnvUpdate::Inherit => {
                        if let Some(value) = std::env::var_os(&name) {
                            expected.insert(name, value);
                        }
                    }
                    EnvUpdate::Set(value) => {
                        expected.insert(name, value);
                    }
                }
            }

            prop_assert_eq!(child_env, expected);
        }

        #[test]
        fn env_variables_can_be_set_to_be_removed_from_inherited_env(
            rem_key in proptest::sample::select(std::env::vars_os().map(|(k,_v)| k).collect::<Vec<_>>())
        ) {
            let mut env_builder = EnvBuilder::default();
            env_builder.remove_var(&rem_key);

            let mut produced_env = HashMap::new();
            env_builder.build_on(&mut produced_env);

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

            let mut produced_env = HashMap::new();
            env_builder.build_on(&mut produced_env);

            //FIXME on CI this test can leak secrets if it fails
            prop_assert_eq!(produced_env.get(&env_key), Some(&replacement));
        }

        #[test]
        fn env_variables_can_be_set_to_inherit_even_if_inheritance_is_disabled(
            inherit in proptest::sample::select(std::env::vars_os().map(|(k,_v)| k).collect::<Vec<_>>()),
        )  {
            let expected_val = std::env::var_os(&inherit);
            let mut env_builder = EnvBuilder::default();
            env_builder.set_inherit_env(false);
            env_builder.always_inherit_var(inherit.clone());
            assert_eq!(env_builder.inherit_env(), false);

            let mut new_env = HashMap::new();
            env_builder.build_on(&mut new_env);
            prop_assert_eq!(new_env.len(), 1);

            let got_value = new_env.iter()
                .find(|(k,_v)| &**k == &*inherit)
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

            let expected_values = std::env::vars_os().collect::<HashMap<_,_>>();

            let mut env_builder = EnvBuilder::default();
            env_builder.always_inherit_var(pointless_inherit.clone());
            env_builder.always_inherit_var(NON_EXISTING_VAR_KEY);

            prop_assert_eq!(env_builder.inherit_env(), true);

            let mut values = HashMap::new();
            env_builder.build_on(&mut values);


            prop_assert!(!values.contains_key(OsStr::new(NON_EXISTING_VAR_KEY)));
            prop_assert_eq!(expected_values.len(), values.len());

            //FIXME on CI this test can leak secrets if it fails
            prop_assert_eq!(
                expected_values.get(&pointless_inherit),
                values.get(&*pointless_inherit)
            );
        }
    }
}
