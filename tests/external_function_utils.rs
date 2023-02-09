#![allow(unused_must_use)]
mod test_utils;
use cxc::{
    library::StdLib, CompData, ExternalFuncAdd, StructType, Type, TypeEnum, Unit, VarName,
};

#[test]
fn default_util() {
    let mut unit = Unit::new();

    let string_type = unit.add_opaque_type::<String>();

    unit.add_external_default::<String>(string_type);
    unit.push_script("main(); String { ; String:default() }");

    let default_string = unsafe { unit.get_fn_by_name::<(), String>("main")(()) };
    assert_eq!(default_string, String::default());
}

#[test]
fn clone_util() {
    let mut unit = Unit::new();

    let string_type = unit.add_opaque_type::<String>();

    unit.add_external_clone::<String>(string_type.clone());

    unit.add_rust_func_explicit(
        "a_cool_string",
        a_cool_string as *const usize,
        ExternalFuncAdd {
            ret_type: string_type.clone(),
            ..ExternalFuncAdd::empty()
        },
    );

    unit.push_script(
        "
        main(); String { 
            cool_string: String = a_cool_string()
            ; cool_string.clone()
        }
    ",
    );

    let default_string = unsafe { unit.get_fn_by_name::<(), String>("main")(()) };
    assert_eq!(default_string, a_cool_string());
}

fn a_cool_string() -> String { String::from("coolman") }

fn add_an_i32(args: Vec<Type>, _: &CompData) -> Type {
    let TypeEnum::Struct(StructType { mut fields, .. }) = args[0].clone_type_enum() else { panic!() };
    fields.push((VarName::from("thei32"), Type::i(32)));
    Type::new_struct(fields)
}

#[test]
fn type_level_functions() {
    let mut unit = Unit::new();

    unit.add_lib(StdLib);

    unit.add_type_level_func("AddAnI32".into(), add_an_i32);

    unit.push_script(
        "
        Point = { x: f32, y: f32 }

        main() { 
            point_with_i32: AddAnI32(Point) = { -- }
            point_with_i32.x = 1.0
            point_with_i32.thei32 = 90

            assert_eq<i32>(point_with_i32.thei32, 90)

            defaulted_with_i32: AddAnI32(Point) = AddAnI32(Point) {
                x = 90.0,
                ++
            }

            print(42)
            print(defaulted_with_i32.thei32)
            assert_eq<i32>(defaulted_with_i32.thei32, 0)
        }
        ",
    );

    unsafe { unit.get_fn_by_name::<(), ()>("main")(()) };
}
