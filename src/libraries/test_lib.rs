use std::fmt::{Debug, Display};

use crate::Type;

use super::Library;

pub struct TestLib;

impl Library for TestLib {
    fn add_to_unit(&self, unit: &mut crate::Unit) {
        unit.add_rust_func_explicit(
            "print",
            print::<i32> as *const usize,
            Type::never().func_with_args(vec![]),
            None,
            vec![Type::i(32)],
        )
        .add_rust_func_explicit(
            "print",
            print::<i64> as *const usize,
            Type::never().func_with_args(vec![]),
            None,
            vec![Type::i(64)],
        )
        .add_rust_func_explicit(
            "print",
            print::<f32> as *const usize,
            Type::never().func_with_args(vec![]),
            None,
            vec![Type::f(32)],
        )
        .add_rust_func_explicit(
            "assert_eq",
            assert::<i32> as *const usize,
            Type::never().func_with_args(vec![Type::i(32), Type::i(32)]),
            None,
            vec![Type::i(32)],
        )
        .add_rust_func_explicit(
            "assert_eq",
            assert::<i64> as *const usize,
            Type::never().func_with_args(vec![Type::i(64), Type::i(64)]),
            None,
            vec![Type::i(64)],
        )
        .add_rust_func_explicit(
            "assert_eq",
            assert::<f32> as *const usize,
            Type::never().func_with_args(vec![Type::f(32), Type::f(32)]),
            None,
            vec![Type::f(32)],
        )
        .add_rust_func_explicit(
            "assert_eq",
            assert::<bool> as *const usize,
            Type::never().func_with_args(vec![Type::bool(), Type::bool()]),
            None,
            vec![Type::bool()],
        )
        .add_rust_func("sqrt", [f32::sqrt])
        .add_rust_func_explicit(
            "panic",
            panic as *const usize,
            Type::never().func_with_args(vec![]),
            None,
            vec![],
        )
        .add_rust_func("to_i64", [to_i64]);
    }
}

fn panic(_: ()) { panic!() }
fn print<T: Display>(val: T) { println!("{val}") }
fn assert<T: PartialEq + Debug>(lhs: T, rhs: T) { assert_eq!(lhs, rhs) }
fn to_i64(input: i32) -> i64 { input as i64 }
