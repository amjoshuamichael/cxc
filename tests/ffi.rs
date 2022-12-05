mod test_utils;
use cxc::library::{StdLib, TestLib};
use cxc::{ExternalFuncAdd, FloatType, IntType, TypeData, TypeEnum, XcValue};
use cxc::{LLVMContext, Type, Unit};
use test_utils::{xc_test, Numbers5, Point2D, Point3D};

#[test]
fn return_arg() {
    xc_test!(
        input, i32; => i32;
        "; input";
        32 => 32
    )
}

#[test]
fn multiple_args() {
    xc_test!(
        a, i32; b, i32; => i32;
        "; a + b";
        4, 5 => 9
    )
}

#[test]
fn pointer() {
    let context = LLVMContext::new();
    let mut unit = Unit::new(&context);

    unit.push_script(
        "
            square(num: &i32): i32 {
                num = *num * *num
                ; 0
            }
            ",
    );

    let mut num = 4;
    unsafe { unit.get_fn_by_name::<&mut i32, i32>("square")(&mut num) };
    assert_eq!(num, 16);
}

#[test]
fn one_el_array_out() {
    xc_test!(
        "main(): i32[1] { ; [1] }";
        [1]
    )
}

#[test]
fn two_el_array_out() {
    xc_test!(
        "main(): i32[2] { ; [1, 4] }";
        [1, 4]
    )
}

#[test]
fn three_el_array_out() {
    xc_test!(
        "main(): i32[3] { ; [1, 4, 9] }";
        [1, 4, 9]
    )
}

#[test]
fn four_el_array_out() {
    xc_test!(
        "main(): i32[4] { ; [1, 4, 9, 90] }";
        [1, 4, 9, 90]
    )
}

#[test]
fn five_el_array_out() {
    xc_test!(
        "main(): i32[5] { ; [1, 4, 9, 90, 129] }";
        [1, 4, 9, 90, 129]
    )
}

#[test]
fn struct_pointer() {
    let context = LLVMContext::new();
    let mut unit = Unit::new(&context);

    unit.push_script(
        "
        Point2D = {
            x: i32,
            y: i32
        }

        sqr_magnitude_of(in_ptr: &Point2D): i32 {
            in: Point2D = *in_ptr

            ; in.x * in.x + in.y * in.y
        }
        ",
    );

    let point = Point2D { x: 2, y: 3 };

    let sqr_mag: i32 = unsafe { unit.get_fn_by_name("sqr_magnitude_of")(&point) };
    assert_eq!(sqr_mag, 13);
}

#[test]
fn small_struct() {
    xc_test!(
        "; Point2D { x = 32, y = 43 }" => Point2D;
        Point2D { x: 32, y: 43 }
    )
}

#[test]
fn medium_struct() {
    xc_test!(
        "; Point3D { x = 32, y = 43, z = 13 }" => Point3D;
        Point3D { x: 32, y: 43, z: 13 }
    )
}

#[test]
fn large_struct() {
    xc_test!(
        "; Numbers5 { a = 1, b = 2, c = 3, d = 4, e = 5 }" => Numbers5;
        Numbers5 { a: 1, b: 2, c: 3, d: 4, e: 5 }
    )
}

#[test]
fn external_function() {
    pub fn print_num(input: i64) {
        println!("{input}");
    }

    let context = LLVMContext::new();
    let mut unit = Unit::new(&context);

    unit.add_rust_func_explicit(
        "print_num",
        print_num as *const usize,
        ExternalFuncAdd {
            arg_types: vec![Type::i(64)],
            ..ExternalFuncAdd::empty()
        },
    )
    .push_script(
        "
        call(): i64 {
            x: i64 = 0
            @ x < 100 {
                print_num(x)
                x = x + 1
            }

            ; x
        }
        ",
    );

    unsafe { unit.get_fn_by_name::<(), i64>("call")(()) };
}

#[test]
fn strings() {
    xc_test!(
        use TestLib, StdLib;
        r#"
        main(): String {
            x: String = "that's cray"
            ; x
        }
        "#;
        String::from("that's cray")
    )
}

#[test]
fn reflected_type_masks() {
    pub fn assert_is_five(type_ptr: *const TypeData, input: *const u8) {
        let the_type = unsafe { Type::from_raw(type_ptr) };
        let val = unsafe { XcValue::new_from_ptr(the_type.clone(), input) };

        match the_type.as_type_enum() {
            TypeEnum::Int(IntType { size: 32 }) => {
                let five: i32 = unsafe { *val.get_data_as::<i32>() };

                assert_eq!(val.get_size(), std::mem::size_of::<i32>());
                assert_eq!(five, 5);
            },
            TypeEnum::Float(FloatType::F32) => {
                let five: f32 = unsafe { *val.get_data_as::<f32>() };

                assert_eq!(val.get_size(), std::mem::size_of::<f32>());
                assert_eq!(five, 5.0);
            },
            _ => panic!("wrong type!"),
        }
    }

    let context = LLVMContext::new();
    let mut unit = Unit::new(&context);

    unit.add_rust_func_explicit(
        "assert_is_five",
        assert_is_five as *const usize,
        ExternalFuncAdd {
            arg_types: vec![Type::void_ptr()],
            ..ExternalFuncAdd::empty()
        }
        .reflect_variable_types(),
    );
    unit.add_lib(cxc::library::TestLib);
    unit.push_script(
        "
        main(): i32 {
            assert_is_five(&5)
            assert_is_five(&5.0)

            ; 0
        }
        ",
    );

    unsafe { unit.get_fn_by_name::<(), i32>("main")(()) };
}

#[test]
fn value_from_code() {
    pub fn assert_is_five(val: &XcValue) {
        match val.get_type().as_type_enum() {
            TypeEnum::Int(IntType { size: 32 }) => {
                let five: i32 = unsafe { *val.get_data_as::<i32>() };

                assert_eq!(val.get_size(), std::mem::size_of::<i32>());
                assert_eq!(five, 5);
            },
            TypeEnum::Float(FloatType::F32) => {
                let five: f32 = unsafe { *val.get_data_as::<f32>() };

                assert_eq!(val.get_size(), std::mem::size_of::<f32>());
                assert_eq!(five, 5.0);
            },
            _ => panic!("wrong type!"),
        }
    }

    let context = LLVMContext::new();
    let mut unit = Unit::new(&context);

    unit.add_lib(StdLib);

    let value_type = unit.comp_data.get_by_name(&"XcValue".into()).unwrap();

    unit.add_rust_func_explicit(
        "assert_is_five",
        assert_is_five as *const usize,
        ExternalFuncAdd {
            arg_types: vec![value_type.clone()],
            ..ExternalFuncAdd::empty()
        },
    );
    unit.push_script(
        "
        main(): i32 {
            assert_is_five(&XcValue:from(&5))
            assert_is_five(&XcValue:from(&5.0))

            ; 0
        }
        ",
    );

    unsafe { unit.get_fn_by_name::<(), i32>("main")(()) };
}
