use crate::libraries::Library;
use crate::TypeRelation;
use crate::{ExternalFuncAdd, XcReflect};
use std::sync::atomic::*;

macro_rules! impl_atomic_reflect {
    ($type:ty, $to:ty) => {
        impl XcReflect for $type {
            fn alias_code() -> String { stringify!($type = Transparent({ $to, })).into() }
        }
    };
}

impl_atomic_reflect!(AtomicBool, bool);
impl_atomic_reflect!(AtomicI8, i8);
impl_atomic_reflect!(AtomicI16, i16);
impl_atomic_reflect!(AtomicI32, i32);
impl_atomic_reflect!(AtomicI64, i64);
impl_atomic_reflect!(AtomicIsize, i64);
impl_atomic_reflect!(AtomicU8, u8);
impl_atomic_reflect!(AtomicU16, u16);
impl_atomic_reflect!(AtomicU32, u32);
impl_atomic_reflect!(AtomicU64, u64);
impl_atomic_reflect!(AtomicUsize, u64);

macro_rules! add_atomic_to_unit {
    ($type:ty, $unit:ident, $ordering:ident:) => {{
        let (atomic_type, inner) = add_atomic_to_unit!($type, $unit, $ordering);

        $unit.add_rust_func_explicit(
            "fetch_add",
            <$type>::fetch_add as *const usize,
            ExternalFuncAdd {
                arg_types: vec![inner.clone(), $ordering.clone()],
                ret_type: inner.clone(),
                ..ExternalFuncAdd::empty()
            }
            .method_of(atomic_type.get_ref()),
        );
    }};

    ($type:ty, $unit:ident, $ordering:ident) => {{
        let atomic_type = $unit.add_reflect_type::<$type>().unwrap();
        let inner = $unit
            .comp_data
            .get_spec(&"T.0".into(), &vec![atomic_type.clone()])
            .unwrap();
        $unit.add_external_default::<$type>();

        $unit.add_rust_func_explicit(
            "load",
            <$type>::load as *const usize,
            ExternalFuncAdd {
                arg_types: vec![$ordering.clone()],
                ret_type: inner.clone(),
                ..ExternalFuncAdd::empty()
            }
            .method_of(atomic_type.get_ref()),
        );

        $unit.add_rust_func_explicit(
            "store",
            <$type>::store as *const usize,
            ExternalFuncAdd {
                arg_types: vec![inner.clone(), $ordering.clone()],
                ..ExternalFuncAdd::empty()
            }
            .method_of(atomic_type.get_ref()),
        );

        $unit.add_rust_func_explicit(
            "swap",
            <$type>::swap as *const usize,
            ExternalFuncAdd {
                arg_types: vec![inner.clone(), $ordering.clone()],
                ..ExternalFuncAdd::empty()
            }
            .method_of(atomic_type.clone()),
        );

        $unit.add_rust_func_explicit(
            "into_inner",
            <$type>::into_inner as *const usize,
            ExternalFuncAdd {
                ret_type: inner.clone(),
                ..ExternalFuncAdd::empty()
            }
            .method_of(atomic_type.clone()),
        );

        $unit.add_rust_func_explicit(
            "new",
            <$type>::new as *const usize,
            ExternalFuncAdd {
                arg_types: vec![inner.clone()],
                ret_type: atomic_type.clone(),
                relation: TypeRelation::Static(atomic_type.clone()),
                ..ExternalFuncAdd::empty()
            },
        );

        $unit.add_rust_func_explicit(
            "get_mut",
            <$type>::get_mut as *const usize,
            ExternalFuncAdd {
                ret_type: inner.get_ref(),
                ..ExternalFuncAdd::empty()
            }
            .method_of(atomic_type.get_ref()),
        );

        (atomic_type, inner)
    }};
}

impl XcReflect for Ordering {
    fn alias_code() -> String {
        //"Ordering = { 
        //    Relaxed: {} / 
        //    Release: {} / 
        //    Acquire: {} / 
        //    AcqRel: {} / 
        //    SeqCst: {} 
        //}"
        //.into()
        "Ordering = u8".into()
    }
}

pub(super) struct AtomicLib;

impl Library for AtomicLib {
    fn add_to_unit(&self, unit: &mut crate::Unit) {
        let ordering = unit.add_reflect_type::<Ordering>().unwrap();

        add_atomic_to_unit!(AtomicBool, unit, ordering);
        add_atomic_to_unit!(AtomicI8, unit, ordering:);
        add_atomic_to_unit!(AtomicI16, unit, ordering:);
        add_atomic_to_unit!(AtomicI32, unit, ordering:);
        add_atomic_to_unit!(AtomicI64, unit, ordering:);
        add_atomic_to_unit!(AtomicIsize, unit, ordering:);
        add_atomic_to_unit!(AtomicU8, unit, ordering:);
        add_atomic_to_unit!(AtomicU16, unit, ordering:);
        add_atomic_to_unit!(AtomicU32, unit, ordering:);
        add_atomic_to_unit!(AtomicU64, unit, ordering:);
        add_atomic_to_unit!(AtomicUsize, unit, ordering:);
    }
}
