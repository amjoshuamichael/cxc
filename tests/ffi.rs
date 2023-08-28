#![allow(unused_must_use)]
mod test_utils;
use std::rc::Rc;

use cxc::library::StdLib;
use cxc::{ExternalFuncAdd, TypeRelation};
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
            *num = *num * *num
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

        sqr_magnitude_of(in: &Point2D); i32 {
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
fn struct_rc_pointer() {
    let mut unit = Unit::new();

    unit.add_lib(StdLib);
    unit.push_script(
        r#"
        Point2D = {
            x: i32,
            y: i32
        }

        sqr_magnitude_of(in: Rc<Point2D>); i32 {
            ; in.x * in.x + in.y * in.y
        }
        "#,
    )
    .unwrap();

    let point = Point2D { x: 2, y: 3 };

    let sqr_mag: i32 = unit
        .get_fn("sqr_magnitude_of")
        .unwrap()
        .downcast::<(Rc<Point2D>,), i32>()(Rc::new(point));
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
            ret_type: point2d.clone(),
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
    pub fn assert_is_less_than_100(input: i32) {
        assert!(input < 100);
    }

    let mut unit = Unit::new();

    unit.add_rust_func_explicit(
        "assert_is_less_than_100",
        assert_is_less_than_100 as *const usize,
        ExternalFuncAdd {
            arg_types: vec![Type::i(32)],
            ..ExternalFuncAdd::empty()
        },
    );
    unit.push_script(
        "
        call(); i32 {
            x: i32 = 0
            @ x < 100 {
                assert_is_less_than_100(x)
                x = x + 1
            }

            ; x
        }
        ",
    )
    .unwrap();

    unit.get_fn("call").unwrap().downcast::<(), i64>()();
}

#[test]
fn return_from_external_4byte() {
    pub fn ext() -> i32 {
        1
    }

    let mut unit = Unit::new();

    unit.add_rust_func_explicit(
        "ext",
        ext as *const usize,
        ExternalFuncAdd {
            ret_type: Type::i(32),
            ..ExternalFuncAdd::empty()
        }
    );

    unit.push_script("main(); i32 { ; ext() }");
    let main = unit.get_fn("main").unwrap().downcast::<(), i32>();
    assert_eq!(main(), ext());
}

#[test]
fn return_from_external_8byte() {
    pub fn ext() -> i64 {
        (i32::MAX as i64) + 20938
    }

    let mut unit = Unit::new();

    unit.add_rust_func_explicit(
        "ext",
        ext as *const usize,
        ExternalFuncAdd {
            ret_type: Type::i(64),
            ..ExternalFuncAdd::empty()
        }
    );

    unit.push_script("main(); i64 { ; ext() }");
    let main = unit.get_fn("main").unwrap().downcast::<(), i64>();
    assert_eq!(main(), ext());
}

#[test]
fn return_from_external_8byte_sep() {
    pub fn ext() -> (i32, i32) {
        (4325, 3948)
    }

    let mut unit = Unit::new();

    unit.add_rust_func_explicit(
        "ext",
        ext as *const usize,
        ExternalFuncAdd {
            ret_type: Type::new_tuple(vec![Type::i(32), Type::i(32)]),
            ..ExternalFuncAdd::empty()
        }
    );

    unit.push_script("main(); {i32, i32} { ; ext() }");
    let main = unit.get_fn("main").unwrap().downcast::<(), (i32, i32)>();
    assert_eq!(main(), ext());
}

#[test]
fn return_from_external_12byte_2sep() {
    pub fn ext() -> (i64, i32) {
        ((i32::MAX as i64) + 98543, 43829)
    }

    let mut unit = Unit::new();

    unit.add_rust_func_explicit(
        "ext",
        ext as *const usize,
        ExternalFuncAdd {
            ret_type: Type::new_tuple(vec![Type::i(64), Type::i(32)]),
            ..ExternalFuncAdd::empty()
        }
    );

    unit.push_script("main(); {i64, i32} { ; ext() }");
    let main = unit.get_fn("main").unwrap().downcast::<(), (i64, i32)>();
    assert_eq!(main(), ext());
}

#[test]
fn return_from_external_12byte_3sep() {
    pub fn ext() -> (i32, i32, i32) {
        (4325, 3948, 43829)
    }

    let mut unit = Unit::new();

    unit.add_rust_func_explicit(
        "ext",
        ext as *const usize,
        ExternalFuncAdd {
            ret_type: Type::new_tuple(vec![Type::i(32); 3]),
            ..ExternalFuncAdd::empty()
        }
    );

    unit.push_script("main(); {i32, i32, i32} { ; ext() }");
    let main = unit.get_fn("main").unwrap().downcast::<(), (i32, i32, i32)>();
    assert_eq!(main(), ext());
}

#[test]
fn return_from_external_16byte_4sep() {
    pub fn ext() -> (i32, i32, i32, i32) {
        (4325, 3948, 43829, 645089)
    }

    let mut unit = Unit::new();

    unit.add_rust_func_explicit(
        "ext",
        ext as *const usize,
        ExternalFuncAdd {
            ret_type: Type::new_tuple(vec![Type::i(32); 4]),
            ..ExternalFuncAdd::empty()
        }
    );

    unit.push_script("main(); {i32, i32, i32, i32} { ; ext() }");
    let main = unit.get_fn("main").unwrap().downcast::<(), (i32, i32, i32, i32)>();
    assert_eq!(main(), ext());
}

#[test]
fn return_from_external_16byte_2sep() {
    pub fn ext() -> (i64, i64) {
        ((i32::MAX as i64) + 98543, (i32::MAX as i64) + 57458)
    }

    let mut unit = Unit::new();

    unit.add_rust_func_explicit(
        "ext",
        ext as *const usize,
        ExternalFuncAdd {
            ret_type: Type::new_tuple(vec![Type::i(64); 2]),
            ..ExternalFuncAdd::empty()
        }
    );

    unit.push_script("main(); {i64, i64} { ; ext() }");
    let main = unit.get_fn("main").unwrap().downcast::<(), (i64, i64)>();
    assert_eq!(main(), ext());
}

#[test]
fn return_from_external_32byte() {
    pub fn ext() -> (i64, i64, i64, i64) {
        (
            (i32::MAX as i64) + 98543, 
            (i32::MAX as i64) + 57458, 
            (i32::MAX as i64) + 23903, 
            (i32::MAX as i64) + 10948,
        )
    }

    let mut unit = Unit::new();

    unit.add_rust_func_explicit(
        "ext",
        ext as *const usize,
        ExternalFuncAdd {
            ret_type: Type::new_tuple(vec![Type::i(64); 4]),
            ..ExternalFuncAdd::empty()
        }
    );

    unit.push_script("main(); {i64, i64, i64, i64} { ; ext() }");
    let main = unit.get_fn("main").unwrap().downcast::<(), (i64, i64, i64, i64)>();
    assert_eq!(main(), ext());
}

#[test]
fn pass_to_4byte() {
    let mut unit = Unit::new();

    unit.push_script("add_59(x: i32); i32 { ; x + 59 }").unwrap();
    let add_59 = unit.get_fn("add_59").unwrap().downcast::<(i32,), i32>();
    assert_eq!(add_59(41), 100);
}

#[test]
fn pass_to_16byte() {
    let mut unit = Unit::new();

    unit.push_script("sum_two_add_3(x: { i64, i64 }); i64 { ; x.0 + x.1 + i64 3 }").unwrap();
    let sum_two_add_3 = 
        unit.get_fn("sum_two_add_3").unwrap().downcast::<((i64, i64),), i64>();
    assert_eq!(sum_two_add_3((3i64, 90i64)), 96i64);
}

#[test]
fn pass_to_two_16byte() {
    let mut unit = Unit::new();

    unit.push_script(
        "
        sum_four_add_3(x: { i64, i64 }, y: { i64, i64 }); i64 {
             ; x.0 + x.1 + y.0 + y.1 + i64 3
        }
        ")
        .unwrap();
    let sum_four_add_3 = 
        unit.get_fn("sum_four_add_3").unwrap().downcast::<((i64, i64), (i64, i64)), i64>();
    assert_eq!(sum_four_add_3((3i64, 90i64), (8i64, 23i64)), 127i64);
}

#[test]
fn pass_to_20byte() {
    let mut unit = Unit::new();

    unit.push_script("take_5(nums: {i32, i32, i32, i32, i32}); i32 { ; nums.3 + 2 }").unwrap();
    let take_5 = 
        unit.get_fn("take_5").unwrap().downcast::<((i32, i32, i32, i32, i32),), i32>();
    assert_eq!(take_5((439, 435, 102, 4, 100)), 6);
}

#[test]
fn pass_20byte_return_20byte() {
    let mut unit = Unit::new();

    unit.push_script("take_5(nums: {i32, i32, i32, i32, i32}); { i32, i32, i32, i32, i32 } { ; nums }").unwrap();
    let take_5 = 
        unit.get_fn("take_5").unwrap().downcast::<((i32, i32, i32, i32, i32),), (i32, i32, i32, i32, i32)>();
    assert_eq!(take_5((439, 435, 102, 4, 100)), (439, 435, 102, 4, 100));
}

#[test]
fn pass_to_40byte() {
    let mut unit = Unit::new();

    unit.push_script("take_5(nums: {i64, i64, i64, i64, i64}); i64 { ; nums.3 + i64 2 }").unwrap();
    let take_5 = 
        unit.get_fn("take_5").unwrap().downcast::<((i64, i64, i64, i64, i64),), i64>();
    assert_eq!(take_5((439, 435, 102, 4, 100)), 6);
}

#[test]
fn pass_40byte_return_40byte() {
    let mut unit = Unit::new();

    unit.push_script("take_5(nums: {i64, i64, i64, i64, i64}); {i64, i64, i64, i64, i64} { ; nums }").unwrap();
    let take_5 = 
        unit.get_fn("take_5").unwrap().downcast::<((i64, i64, i64, i64, i64),), (i64, i64, i64, i64, i64)>();
    assert_eq!(take_5((439, 435, 102, 4, 100)), (439, 435, 102, 4, 100));
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

