#![allow(unused_must_use)]
#![allow(dead_code)]
mod test_utils;
use cxc::{library::StdLib, Unit};
use std::rc::Rc;
use test_utils::{xc_test, BoolMM, BoolMMM, BoolMore, Point2D, Point3D};

#[derive(Debug, PartialEq)]
pub enum IntOrFloat {
    Int(i32),
    Float(f32),
}

#[test]
fn sum_type_basic_set_1() {
    xc_test!(
        "
            IntOrFloat = { Int: i32 / Float: f32 }

            main(); IntOrFloat {
                x: IntOrFloat = IntOrFloat.Int { 934 }
                ; x
            }
        ";
        IntOrFloat::Int(934)
    );
}

#[test]
fn sum_type_basic_set_2() {
    xc_test!(
        "
            IntOrFloat = { Int: i32 / Float: f32 }

            main(); IntOrFloat {
                x: IntOrFloat = IntOrFloat.Float { 490.24 }
                ; x
            }
        ";
        IntOrFloat::Float(490.24)
    );
}

#[derive(Debug, PartialEq)]
pub enum PointOrInt {
    Point(Point2D),
    Int(i32),
}

#[test]
fn sum_type_struct() {
    xc_test!(
        "
            PointOrFloat = { Point: { x: i32, y: i32 } / Int: i32 }

            main(); PointOrFloat {
                x: PointOrFloat = PointOrFloat.Point { x = 43, y = 54 }
                ; x
            }
        ";
        PointOrInt::Point(Point2D { x: 43, y: 54 })
    );
}

#[test]
fn sum_type_struct_2() {
    xc_test!(
        "
            PointOrFloat = { Point: { x: i32, y: i32 } / Int: i32 }

            main(); PointOrFloat {
                x: PointOrFloat = PointOrFloat.Int { 245 }
                ; x
            }
        ";
        PointOrInt::Int(245)
    );
}

#[test]
fn ref_option_none() {
    xc_test!(
        use StdLib;
        "
            main(); Option<&i32> {
                output: Option<&i32> = Option<&i32>:default()

                ; output
            }
        ";
        Option::<&i32>::None
    )
}

#[test]
fn ref_option_and_more() {
    xc_test!(
        use StdLib;
        "
            main(); {Option<&i32>, i32} {
                output: {Option<&i32>, i32} =
                    { Option<&i32>, i32 } { Option<&i32>:default(), 32 }
                ; output
            }
        ";
        (Option::<&i32>::None, 32)
    )
}

#[test]
fn ref_option_some() {
    let mut unit = Unit::new();

    unit.add_lib(StdLib);

    unit.push_script(
        "
        main(pointer: &i32); Option<&i32> {
            output: Option<&i32> = Option<&i32>.Some { pointer }

            ; output
        }
        ",
    );

    let mut thirty_two = 32;
    let func = unit
        .get_fn("main")
        .unwrap()
        .downcast::<(&i32,), Option<&i32>>();
    let output: Option<&i32> = func(&mut thirty_two);
    assert_eq!(output, Option::Some(&thirty_two));
}

#[test]
fn ref_and_more_option() {
    let mut unit = Unit::new();

    unit.add_lib(StdLib);

    unit.push_script(
        "
        main(pointer: &i32); Option<{i32, &i32}> {
            output: Option<{i32, &i32}> = Option<{i32, &i32}>.Some { 10, pointer }
            ; output
        }
        ",
    );

    let mut thirty_two = 32;
    let func = unit
        .get_fn("main")
        .unwrap()
        .downcast::<(&mut i32,), Option<(i32, &i32)>>();
    let output: Option<(i32, &i32)> = func(&mut thirty_two);
    assert_eq!(output, Option::Some((10, &32)));
}

#[derive(Debug, PartialEq)]
pub enum RatherLargeEnum {
    V1 { a: i32, b: i32, c: (i32, i64) },
    V2 { a: bool, b: [f32; 3] },
    V3,
}

#[test]
fn rather_large_enum_v1() {
    xc_test!(
        "
            RatherLargeEnum = {
                V1: { a: i32, b: i32, c: {i32, i64} } / 
                V2: { a: bool, b: [3]f32 } /
                V3: {}
            }

            main(); RatherLargeEnum {
                x: RatherLargeEnum = RatherLargeEnum.V1 
                    { a = 10, b = 20, c = {i32, i64} {30, i64 40} }
                ; x
            }
        ";
        RatherLargeEnum::V1 {
            a: 10,
            b: 20,
            c: (30, 40)
        }
    );
}

#[test]
fn rather_large_enum_v2() {
    xc_test!(
        use StdLib;
        "
            RatherLargeEnum = {
                V1: { a: i32, b: i32, c: {i32, i64} } / 
                V2: { a: bool, b: [3]f32 } /
                V3: {}
            }

            main(); { i8, i32, i32, { i32, i64 } } {
                float_array: [3]f32 = [ 89.0, 73.0, 3.0 ]
                x: RatherLargeEnum = RatherLargeEnum.V2 
                    { a = true, b = float_array }
                ; x
            }
        ";
        RatherLargeEnum::V2 {
            a: true,
            b: [89.0, 73.0, 3.0]
        }
    );
}

#[test]
fn rather_large_enum_v3() {
    xc_test!(
        use StdLib;
        "
            RatherLargeEnum = {
                V1: { a: i32, b: i32, c: {i32, i64} } / 
                V2: { a: bool, b: [3]f32 } /
                V3: {}
            }

            main(); { i8, i32, i32, { i32, i64 } } {
                x: RatherLargeEnum = RatherLargeEnum.V3 {}
                ; x
            }
        ";
        RatherLargeEnum::V3
    );
}

#[test]
fn basic_option() {
    xc_test!(
        use StdLib;
        "
            main(); Option<i32> {
                output: Option<i32> = Option<i32>.Some { 10 }

                ; output
            }
        ";
        Some(10)
    )
}

#[test]
fn option_rc() {
    xc_test!(
        use StdLib;
        "
            main(); Option< Rc<i32> > {
                x: i32 = 90
                rc: Rc<i32> = Rc<i32>:new(x)
                output: Option< Rc<i32> > = Option< Rc<i32> >.Some { rc }

                ; output
            }
        ";
        Some(Rc::new(90))
    )
}

// TODO: add ability to return sum types directly
#[test]
fn option_rc_and_int64() {
    xc_test!(
        use StdLib;
        "
            main(); Option< { i64, Rc<i32> } > {
                output: Option< { i64, Rc<i32> } > = 
                    Option< { i64, Rc<i32> } >.Some { i64 4, Rc<i32>:new(90) }

                ; output
            }
        ";
        Some((4i64, Rc::new(90)))
    )
}

#[test]
fn option_rc_and_int32() {
    xc_test!(
        use StdLib;
        "
            main(); Option< { i32, Rc<i32> } > {
                output: Option< { i32, Rc<i32> } > = 
                    Option< { i32, Rc<i32> } >.Some { i32 4, Rc<i32>:new(90) }

                ; output
            }
        ";
        Some((4, Rc::new(90)))
    )
}

#[test]
fn option_vec() {
    xc_test!(
        use StdLib;
        "
            main(); Option< Vec<i32> > {
                x: i32 = 90
                vec: Vec<i32> = Vec<i32>:new()
                vec.push(x)
                output: Option< Vec<i32> > = Option< Vec<i32> >.Some { vec }

                ; output
            }
        ";
        Some(vec![90])
    )
}

#[test]
fn option_default() {
    xc_test!(
        use StdLib;
        "
            main(); Option<i32> {
                output: Option<i32> = Option<i32>:default()

                ; output
            }
        ";
        Option::<i32>::None
    )
}

#[test]
fn option_numbers3_once() {
    xc_test!(
        use StdLib;
        "
            main(); Option<Point3D> {
                output: Option<Point3D> = 
                    Option<Point3D>.Some { x = 10, y = 20, z = 30 }

                ; output
            }
        ";
        Option::<Point3D>::Some(Point3D { x: 10, y: 20, z: 30 })
    )
}

#[test]
fn option_numbers3_twice() {
    xc_test!(
        use StdLib;
        "
            main(); Option<{Point3D, Point3D}> {
                output: Option<{Point3D, Point3D}> = 
                    Option<{Point3D, Point3D}>.Some {
                        { x = 394, y = 744, z = 53 },
                        { x = 94, y = 49, z = 384 }
                    }

                ; output
            }
        ";
        Option::<(Point3D, Point3D)>::Some((
            Point3D { x: 394, y: 744, z: 53 },
            Point3D { x: 94, y: 49, z: 384 }
        ))
    )
}

#[test]
fn bigger_option() {
    xc_test!(
        use StdLib;
        "
            main(); Option<{i32, f32, i32}> {
                output: Option<{i32, f32, i32}> = 
                    Option<{i32, f32, i32}>.Some { 10, 20.0, 30 }

                ; output
            }
        ";
        Option::<(i32, f32, i32)>::Some((10, 20.0, 30))
    )
}

#[test]
fn bool_and_then_some_true() {
    xc_test!(
        use StdLib;
        "
            main(); BoolMore {
                output: BoolMore = BoolMore.Bool { true }

                ; output
            }
        ";
        BoolMore::Bool(true)
    )
}

#[test]
fn bool_and_then_some_false() {
    xc_test!(
        use StdLib;
        "
            main(); BoolMore {
                output: BoolMore = BoolMore.Bool { false }

                ; output
            }
        ";
        BoolMore::Bool(false)
    )
}

#[test]
fn bool_and_then_some_bool() {
    xc_test!(
        use StdLib;
        "
            main(); { u8, u8 } {
                output: { BoolMore, BoolMore } =
                    { BoolMore.Bool { false }, BoolMore.Bool { true } }

                ; output
            }
        ";
        (BoolMore::Bool(false), BoolMore::Bool(true))
    )
}

#[test]
fn bool_and_then_some_variants() {
    xc_test!(
        use StdLib;
        "
            main(); { BoolMore, BoolMore, BoolMore, BoolMore } {
                output: { BoolMore, BoolMore, BoolMore, BoolMore } =
                    { BoolMore.V5 { }, BoolMore.V3 { }, BoolMore.V4 { }, BoolMore.V2 { } } 

                ; output
            }
        ";
        (BoolMore::V5, BoolMore::V3, BoolMore::V4, BoolMore::V2)
    )
}

#[test]
fn bool_mm_t() {
    xc_test!(
        use StdLib;
        "
            main(); BoolMM {
                output: BoolMM = BoolMM.BoolM { BoolMore.Bool { true } }

                ; output
            }
        ";
        BoolMM::BoolM(BoolMore::Bool(true))
    )
}

#[test]
fn bool_mm_f() {
    xc_test!(
        use StdLib;
        "
            main(); BoolMM {
                output: BoolMM = BoolMM.BoolM { BoolMore.Bool { false } }

                ; output
            }
        ";
        BoolMM::BoolM(BoolMore::Bool(false))
    )
}

#[test]
fn bool_mm_v1() {
    xc_test!(
        use StdLib;
        "
            main(); BoolMM {
                output: BoolMM = BoolMM.BoolM { BoolMore.V2 { } }

                ; output
            }
        ";
        BoolMM::BoolM(BoolMore::V2)
    )
}

#[test]
fn bool_mm_v2() {
    xc_test!(
        use StdLib;
        "
            # BoolMore

            main(); BoolMM {
                output: BoolMM = BoolMM.V6 {}

                ; output
            }
        ";
        BoolMM::V6
    )
}

#[test]
fn bool_mmm_t() {
    xc_test!(
        use StdLib;
        "
            main(); BoolMMM {
                output: BoolMMM = BoolMMM.BoolMM { BoolMM.BoolM { BoolMore.Bool { true } } }

                ; output
            }
        ";
        BoolMMM::BoolMM(BoolMM::BoolM(BoolMore::Bool(true)))
    )
}

#[test]
fn bool_mmm_f() {
    xc_test!(
        use StdLib;
        "
            main(); BoolMMM {
                output: BoolMMM = BoolMMM.BoolMM { BoolMM.BoolM { BoolMore.Bool { false } } }

                ; output
            }
        ";
        BoolMMM::BoolMM(BoolMM::BoolM(BoolMore::Bool(false)))
    )
}

#[test]
fn bool_mmm_v1() {
    xc_test!(
        use StdLib;
        "
            main(); BoolMMM {
                output: BoolMMM = BoolMMM.BoolMM { BoolMM.BoolM { BoolMore.V2 { } } }

                ; output
            }
        ";
        BoolMMM::BoolMM(BoolMM::BoolM(BoolMore::V2))
    )
}

#[test]
fn bool_mmm_v2() {
    xc_test!(
        use StdLib;
        "
            # BoolMore

            main(); BoolMMM {
                output: BoolMMM = BoolMMM.BoolMM { BoolMM.V6 { } }

                ; output
            }
        ";
        BoolMMM::BoolMM(BoolMM::V6)
    )
}

#[test]
fn bool_mmm_v3() {
    xc_test!(
        use StdLib;
        "
            # BoolMore

            main(); BoolMMM {
                output: BoolMMM = BoolMMM.V11 { }

                ; output
            }
        ";
        BoolMMM::V11
    )
}

// TODO: this doesn't work
#[derive(Debug, PartialEq, Eq)]
enum BSBBBB {
    // BIG small BIG BIG BIG BIG
    Some(i32, i32, i32, i32, i32, bool),
    None,
}

//#[test]
//fn bsbbbbso() {
//    xc_test!(
//        use StdLib;
//        "
//            BSBBBB = {
//                Some: { i32, i32, i32, i32, i32, bool } /
//                None: {}
//            }
//
//            main(); BSBBBB {
//                output: BSBBBB = BSBBBB.Some { 17, 39, 0, 43, 0, true }
//                ; output
//            }
//        ";
//        BSBBBB::Some(17, 39, 0, 43, 0, true)
//    )
//}
