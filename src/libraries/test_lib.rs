use std::fmt::{Debug, Display};

use crate::{parse::TypeRelation, Type};

use super::Library;

pub struct TestLib;

impl Library for TestLib {
    fn add_to_unit(&self, unit: &mut crate::Unit) {
        if let Some(string_type) = unit.comp_data.get_by_name(&"String".into()) {
            unit.add_rust_func_explicit(
                "print",
                print::<&String> as *const usize,
                Type::never().func_with_args(vec![string_type.clone().get_ref()]),
                TypeRelation::Unrelated,
                vec![string_type.get_ref()],
            );
        }

        unit.add_rust_func_explicit(
            "print",
            print::<i32> as *const usize,
            Type::never().func_with_args(vec![Type::i(32)]),
            TypeRelation::Unrelated,
            vec![Type::i(32)],
        )
        .add_rust_func_explicit(
            "print",
            print::<i64> as *const usize,
            Type::never().func_with_args(vec![Type::i(64)]),
            TypeRelation::Unrelated,
            vec![Type::i(64)],
        )
        .add_rust_func_explicit(
            "print",
            print::<f32> as *const usize,
            Type::never().func_with_args(vec![Type::f(32)]),
            TypeRelation::Unrelated,
            vec![Type::f(32)],
        )
        .add_rust_func_explicit(
            "assert_eq",
            assert::<i32> as *const usize,
            Type::never().func_with_args(vec![Type::i(32), Type::i(32)]),
            TypeRelation::Unrelated,
            vec![Type::i(32)],
        )
        .add_rust_func_explicit(
            "assert_eq",
            assert::<i64> as *const usize,
            Type::never().func_with_args(vec![Type::i(64), Type::i(64)]),
            TypeRelation::Unrelated,
            vec![Type::i(64)],
        )
        .add_rust_func_explicit(
            "assert_eq",
            assert::<f32> as *const usize,
            Type::never().func_with_args(vec![Type::f(32), Type::f(32)]),
            TypeRelation::Unrelated,
            vec![Type::f(32)],
        )
        .add_rust_func_explicit(
            "assert_eq",
            assert::<bool> as *const usize,
            Type::never().func_with_args(vec![Type::bool(), Type::bool()]),
            TypeRelation::Unrelated,
            vec![Type::bool()],
        )
        .add_rust_func("sqrt", [f32::sqrt])
        .add_rust_func_explicit(
            "panic",
            panic as *const usize,
            Type::never().func_with_args(vec![]),
            TypeRelation::Unrelated,
            vec![],
        );
    }
}

fn panic(_: ()) { panic!() }
fn print<T: Display>(val: T) { println!("{val}") }
fn assert<T: PartialEq + Debug>(lhs: T, rhs: T) { assert_eq!(lhs, rhs) }
