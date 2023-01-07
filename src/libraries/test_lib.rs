use std::fmt::{Debug, Display};

use crate::{ExternalFuncAdd, Type};

use super::Library;

pub struct TestLib;

impl Library for TestLib {
    fn add_to_unit(&self, unit: &mut crate::Unit) {
        if let Some(string_type) = unit.comp_data.get_by_name(&"String".into()) {
            unit.add_rust_func_explicit(
                "print",
                print::<&String> as *const usize,
                ExternalFuncAdd {
                    arg_types: vec![string_type.clone().get_ref()],
                    generics: vec![string_type.get_ref()],
                    ..ExternalFuncAdd::empty()
                },
            );
        }

        unit.add_rust_func_explicit(
            "print",
            print::<i32> as *const usize,
            ExternalFuncAdd {
                arg_types: vec![Type::i(32)],
                generics: vec![Type::i(32)],
                ..ExternalFuncAdd::empty()
            },
        );
        unit.add_rust_func_explicit(
            "print",
            print_i64_as_bytes as *const usize,
            ExternalFuncAdd {
                arg_types: vec![Type::i(64)],
                generics: vec![Type::i(64)],
                ..ExternalFuncAdd::empty()
            },
        );
        unit.add_rust_func_explicit(
            "print",
            print::<f32> as *const usize,
            ExternalFuncAdd {
                arg_types: vec![Type::f(32)],
                generics: vec![Type::f(32)],
                ..ExternalFuncAdd::empty()
            },
        );
        unit.add_rust_func_explicit(
            "assert_eq",
            assert::<f32> as *const usize,
            ExternalFuncAdd {
                arg_types: vec![Type::f(32); 2],
                generics: vec![Type::f(32)],
                ..ExternalFuncAdd::empty()
            },
        );
        unit.add_rust_func_explicit(
            "assert_eq",
            assert::<i32> as *const usize,
            ExternalFuncAdd {
                arg_types: vec![Type::i(32); 2],
                generics: vec![Type::i(32)],
                ..ExternalFuncAdd::empty()
            },
        );
        unit.add_rust_func_explicit(
            "assert_eq",
            assert::<i64> as *const usize,
            ExternalFuncAdd {
                arg_types: vec![Type::i(64); 2],
                generics: vec![Type::i(64)],
                ..ExternalFuncAdd::empty()
            },
        );
        unit.add_rust_func_explicit(
            "assert_eq",
            assert::<bool> as *const usize,
            ExternalFuncAdd {
                arg_types: vec![Type::bool(); 2],
                generics: vec![Type::bool()],
                ..ExternalFuncAdd::empty()
            },
        );
        unit.add_rust_func("sqrt", [f32::sqrt]);
        unit.add_rust_func_explicit("panic", panic as *const usize, ExternalFuncAdd::empty());
    }
}

fn panic(_: ()) { panic!() }
fn print<T: Display>(val: T) { println!("{val}") }
fn print_i64_as_bytes(val: u64) {
    let slice: [u8; 8] = unsafe { std::mem::transmute(val) };
    dbg!(&slice);
}
fn assert<T: PartialEq + Debug>(lhs: T, rhs: T) { assert_eq!(lhs, rhs) }
