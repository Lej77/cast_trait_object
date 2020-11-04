mod sub {
    //! We use a module to ensure it doesn't need to be expanded in the crate root.

    use cast_trait_object::*;

    #[cfg(not(feature = "proc-macros"))]
    create_dyn_cast_config!(pub SuperConfig = Super => Sub);

    #[cfg(not(feature = "proc-macros"))]
    create_dyn_cast_config!(pub SuperConfig2 = Super => Sub2);

    #[cfg(not(feature = "proc-macros"))]
    create_dyn_cast_config!(pub SuperConfig3 = Super => SubSub);

    // Should allow upcast to supertrait.
    #[cfg(not(feature = "proc-macros"))]
    create_dyn_cast_config!(pub SuperConfig4 = Super => Super);

    #[cfg(not(feature = "proc-macros"))]
    pub trait Super:
        DynCast<SuperConfig> + DynCast<SuperConfig2> + DynCast<SuperConfig3> + DynCast<SuperConfig4>
    {
    }
    #[cfg(feature = "proc-macros")]
    #[dyn_cast(Sub, Sub2, SubSub)]
    #[dyn_upcast]
    pub trait Super {}
    pub trait Sub: Super {}
    pub trait Sub2: Super {}

    pub trait SubSub: Sub {}

    #[cfg(not(feature = "proc-macros"))]
    impl Super for () {}
    #[cfg(not(feature = "proc-macros"))]
    impl_dyn_cast!(() => SuperConfig, SuperConfig2, SuperConfig3, SuperConfig4);

    // Orphan rules don't allow this:
    /*
    #[cfg(not(feature = "proc-macros"))]
    impl Super for fn() {}
    #[cfg(not(feature = "proc-macros"))]
    impl_dyn_cast!(fn() as Super => Super, Sub, Sub2, SubSub);
    */

    pub struct TestSuper;
    #[cfg_attr(feature = "proc-macros", dyn_cast(Sub, Sub2, SubSub))]
    #[cfg_attr(feature = "proc-macros", dyn_upcast)]
    impl Super for TestSuper {}
    #[cfg(not(feature = "proc-macros"))]
    impl_dyn_cast!(TestSuper => SuperConfig, SuperConfig2, SuperConfig3, SuperConfig4);

    pub struct TestSub;
    #[cfg_attr(feature = "proc-macros", dyn_cast(Sub, Sub2, SubSub, Super))]
    impl Super for TestSub {}
    impl Sub for TestSub {}
    #[cfg(not(feature = "proc-macros"))]
    impl_dyn_cast!(TestSub => SuperConfig, SuperConfig2, SuperConfig3, SuperConfig4);

    #[cfg_attr(feature = "proc-macros", dyn_cast(Super => Sub, Sub2, SubSub))]
    #[cfg_attr(feature = "proc-macros", dyn_upcast(Super))]
    pub struct TestSub2;
    impl Super for TestSub2 {}
    impl Sub for TestSub2 {}
    impl Sub2 for TestSub2 {}
    #[cfg(not(feature = "proc-macros"))]
    impl_dyn_cast!(TestSub2 as Super => Super, Sub, Sub2, SubSub);

    #[cfg_attr(feature = "proc-macros", dyn_cast(Super => Sub, Sub2, SubSub, Super))]
    pub struct TestSubSub;
    impl Super for TestSubSub {}
    impl Sub for TestSubSub {}
    impl SubSub for TestSubSub {}
    #[cfg(not(feature = "proc-macros"))]
    impl_dyn_cast!(TestSubSub as Super => Super, Sub, Sub2, SubSub);
}

#[cfg(feature = "proc-macros")]
#[allow(dead_code)]
pub mod with_macros {
    use cast_trait_object::*;

    #[dyn_cast(Sub)]
    #[dyn_upcast]
    trait Super {}
    trait Sub: Super {}

    #[dyn_cast(Super => Sub)]
    #[dyn_upcast(Super)]
    struct TestSuper;
    impl Super for TestSuper {}

    #[dyn_cast(Super => Sub)]
    #[dyn_upcast(Super)]
    struct TestSub;
    impl Super for TestSub {}
    impl Sub for TestSub {}

    // Orphan rules for `GetDynCastConfig` causes this to not compile:
    /*
    #[dyn_cast(Sub)]
    #[dyn_upcast]
    impl Super for () {}
    impl Sub for () {}
    */

    pub mod generic_traits {
        use cast_trait_object::*;

        #[dyn_upcast]
        #[dyn_cast(Sub<T>)]
        trait Super<T> {}
        trait Sub<T>: Super<T> {}

        // Currently orphan rules prevent this:
        /*
        #[dyn_cast(Sub<T>)]
        #[dyn_upcast]
        impl<T> Super<T> for (i32,) {}
        impl<T> Sub<T> for (i32,) {}
        */

        struct TestSuper;
        #[dyn_upcast]
        #[dyn_cast(Sub<T>)]
        impl<T: Clone> Super<T> for TestSuper where T: core::fmt::Display {}

        struct TestSub;
        #[dyn_upcast]
        #[dyn_cast(Sub<T>)]
        impl<T> Super<T> for TestSub {}
        impl<T> Sub<T> for TestSub {}
    }
}

#[cfg(test)]
mod tests {
    use super::sub::*;
    use cast_trait_object::*;

    #[cfg(not(feature = "proc-macros"))]
    #[test]
    fn it_works() {
        let s: &dyn Super = &TestSub;
        let _s: &dyn Sub = DynCast::<SuperConfig>::dyn_cast_ref(s)
            .ok()
            .expect("Failed to cast to dyn Sub");
    }
    #[test]
    fn super_to_sub() {
        let s: &dyn Super = &TestSub;
        let _s = s
            .dyn_cast::<dyn Sub>()
            .ok()
            .expect("Couldn't cast dyn Super to dyn Sub");

        let s: &mut dyn Super = &mut TestSub;
        let _s = s.dyn_cast::<dyn Sub>().ok().expect("&mut cast failed");

        let s: &mut dyn Super = &mut TestSub;
        let _s: &dyn Sub = s.dyn_cast::<dyn Sub>().ok().expect("&mut to & cast failed");
    }
    #[cfg(feature = "alloc")]
    #[test]
    fn super_to_sub_alloc() {
        let s: Box<dyn Super> = Box::new(TestSub);
        let _s = s
            .dyn_cast::<dyn Sub>()
            .ok()
            .expect("Couldn't cast dyn Super to dyn Sub");

        let s: std::rc::Rc<dyn Super> = std::rc::Rc::new(TestSub);
        let _s = s.dyn_cast::<dyn Sub>().ok().expect("Couldn't cast Rc");

        let s: std::sync::Arc<dyn Super> = std::sync::Arc::new(TestSub);
        let _s = s.dyn_cast::<dyn Sub>().ok().expect("Couldn't cast Arc");
    }

    #[cfg(not(feature = "proc-macros"))]
    #[test]
    fn super_to_sub_via_config() {
        let s: &dyn Super = &TestSub;
        let _s = s
            .dyn_cast_with_config::<SuperConfig>()
            .ok()
            .expect("Couldn't cast dyn Super to dyn Sub");
    }
    #[test]
    fn super_to_sub2() {
        let s: &dyn Super = &TestSub2;
        let _s = s
            .dyn_cast::<dyn Sub2>()
            .ok()
            .expect("Couldn't cast dyn Super to dyn Sub");
    }
    #[test]
    fn super_to_sub2_fail() {
        let s: &dyn Super = &TestSub;
        let s = s.dyn_cast::<dyn Sub2>();
        assert!(s.is_err(), "Succeeded in casting `TestSub` to Sub2.");
    }

    #[test]
    fn sub_to_super() {
        let s: &dyn Sub = &TestSub;
        let _s: &dyn Super = s.dyn_upcast::<dyn Super>();
    }

    /// Cast directly from a concrete type to a sub trait without first casting
    /// to a supertrait object.
    #[test]
    fn test_super_to_sub() {
        let s: &TestSuper = &TestSuper;
        let s = s.dyn_cast_adv::<dyn Super, dyn Sub>();
        assert!(s.is_err(), "Succeeded in casting `TestSuper` to Sub.");
    }

    /*
    #[test]
    fn test_sup_to_sub() {
        let s: &TestSub = &TestSub;
        let s: &dyn Sub = cast_trait_object::DynCast::cast_ref(s).ok().expect("Couldn't cast dyn Super to dyn Sub");
    }
    // */
}
