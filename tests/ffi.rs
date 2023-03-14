#![allow(unused_must_use)]
mod test_utils;
use cxc::library::StdLib;
use cxc::{ExternalFuncAdd, FloatType, IntType, TypeData, TypeEnum, TypeRelation, XcValue};
use cxc::{Type, Unit};
use test_utils::{xc_test, Numbers5, Point2D, Point3D};

#[test]
fn return_arg() {
    let mut unit = Unit::new();
    unit.push_script("main(a: i32); i32 { ; a }");

    let num = unit.get_fn("main").unwrap().downcast::<(i32,), i32>()(32);
    assert_eq!(num, 32);
}

#[test]
fn multiple_args() {
    let mut unit = Unit::new();

    unit.add_lib(cxc::library::StdLib);
    unit.push_script(
        "
        add(a: i32, b: i32); i32 {
            ; a + b
        }

        add2(a: i32, b: i32); i32 {
            ; a + b
        }
        ",
    );

    let num = unit.get_fn("add").unwrap().downcast::<(i32, i32), i32>()(4, 5);
    assert_eq!(num, 9);
}

#[test]
fn basic_pointer() {
    let mut unit = Unit::new();

    unit.add_lib(cxc::library::StdLib);
    unit.push_script(
        "
        square(num: &i32) {
            num.write<i32>(*num * *num)
        }
        ",
    );

    let mut num = 4;
    unit.get_fn("square")
        .unwrap()
        .downcast::<(&mut i32,), i32>()(&mut num);
    assert_eq!(num, 16);
}

#[test]
fn one_el_array_out() { xc_test!("; [1]i32 { ; [1] }"; [1]) }
#[test]
fn two_el_array_out() { xc_test!( "; [2]i32 { ; [1, 4] }"; [1, 4]) }
#[test]
fn three_el_array_out() { xc_test!( "; [3]i32 { ; [1, 4, 9] }"; [1, 4, 9]) }
#[test]
fn four_el_array_out() { xc_test!( "; [4]i32 { ; [1, 4, 9, 90] }"; [1, 4, 9, 90]) }
#[test]
fn five_el_array_out() { xc_test!( "; [5]i32 { ; [1, 4, 9, 90, 129] }"; [1, 4, 9, 90, 129]) }

#[test]
fn struct_pointer() {
    let mut unit = Unit::new();

    unit.push_script(
        "
        Point2D = {
            x: i32,
            y: i32
        }

        sqr_magnitude_of(in_ptr: &Point2D); i32 {
            in: Point2D = *in_ptr

            ; in.x * in.x + in.y * in.y
        }
        ",
    )
    .unwrap();

    let point = Point2D { x: 2, y: 3 };

    let sqr_mag: i32 = unit
        .get_fn("sqr_magnitude_of")
        .unwrap()
        .downcast::<(&Point2D,), i32>()(&point);
    assert_eq!(sqr_mag, 13);
}

#[test]
fn small_struct() {
    xc_test!(
        "; Point2D { ; Point2D { x = 32, y = 43 } }";
        Point2D { x: 32, y: 43 }
    )
}

#[test]
fn medium_struct() {
    xc_test!(
        "; Point3D { ; Point3D { x = 32, y = 43, z = 13 } }";
        Point3D { x: 32, y: 43, z: 13 }
    )
}

#[test]
fn large_struct() {
    xc_test!(
        "; Numbers5 { ; Numbers5 { a = 1, b = 2, c = 3, d = 4, e = 5 } }";
        Numbers5 { a: 1, b: 2, c: 3, d: 4, e: 5 }
    )
}

#[test]
fn i32_and_ref() {
    let mut unit = Unit::new();

    unit.add_lib(StdLib);

    unit.push_script(
        "
        main(pointer: &i32); {i32, &i32} {
            output: {i32, &i32} = {i32, &i32} { 10, pointer }
            ; output
        }
        ",
    )
    .unwrap();

    let mut thirty_two = 32;
    let func = unit
        .get_fn("main")
        .unwrap()
        .downcast::<(&i32,), (i32, &i32)>();
    let output: (i32, &i32) = func(&mut thirty_two);
    assert_eq!(output, (10, &thirty_two));
}

#[test]
fn method_on_struct_with_arg() {
    let mut unit = Unit::new();

    let point2d = unit.add_reflect_type::<Point2D>().unwrap();
    unit.add_rust_func_explicit(
        "magnify",
        Point2D::magnify as *const usize,
        ExternalFuncAdd {
            arg_types: vec![point2d.get_ref().clone(), Type::i(32)].clone(),
            ret_type: Type::i(32),
            relation: TypeRelation::MethodOf(point2d.get_ref()),
            ..ExternalFuncAdd::empty()
        },
    );

    unit.push_script(
        "
        main(); i32 {
            point: Point2D = Point2D { x = 42, y = 43 }
            point = point.magnify(40)
            ; point.x
        }
        ",
    )
    .unwrap();

    let output = unit.get_fn("main").unwrap().downcast::<(), i32>()();
    assert_eq!(output, 1680);
}

#[test]
fn external_function() {
    pub fn print_num(input: i64) {
        assert!(input < 100);
    }

    let mut unit = Unit::new();

    unit.add_rust_func_explicit(
        "assert_is_less_than_100",
        print_num as *const usize,
        ExternalFuncAdd {
            arg_types: vec![Type::i(64)],
            ..ExternalFuncAdd::empty()
        },
    );
    unit.push_script(
        "
        call(); i64 {
            x: i64 = 0
            @ x < 100 {
                assert_is_less_than_100(x)
                x = x + 1
            }

            ; x
        }
        ",
    )
    .unwrap();

    unit.get_fn("call").unwrap().downcast::<(), i64>();
}

#[test]
fn strings() {
    xc_test!(
        use StdLib;
        r#"
        main(); String {
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
            TypeEnum::Int(IntType { size: 32, .. }) => {
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

    let mut unit = Unit::new();

    unit.add_rust_func_explicit(
        "assert_is_five",
        assert_is_five as *const usize,
        ExternalFuncAdd {
            arg_types: vec![Type::void_ptr()],
            ..ExternalFuncAdd::empty()
        }
        .reflect_variable_types(),
    );
    unit.add_lib(cxc::library::StdLib);
    unit.push_script(
        "
        main() {
            assert_is_five(&5)
            assert_is_five(&5.0)
            ; 0 
        }
        ",
    );

    unit.get_fn("main").unwrap().downcast::<(), i32>();
}

#[test]
fn value_from_code() {
    pub fn assert_is_five(val: &XcValue) {
        match val.get_type().as_type_enum() {
            TypeEnum::Int(IntType { size: 32, .. }) => {
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

    let mut unit = Unit::new();

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
        main() {
            assert_is_five(&XcValue:from(&5))
            assert_is_five(&XcValue:from(&5.0))
        }
        ",
    );

    unit.get_fn("main").unwrap().downcast::<(), i32>();
}
