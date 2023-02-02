#![allow(unused_must_use)]
mod test_utils;
use cxc::{library::StdLib, Unit};
use std::rc::Rc;

use test_utils::{xc_test, Point2D};

#[derive(Debug, PartialEq)]
pub enum IntOrFloat {
    Int(i32),
    Float(f32),
}

#[test]
fn sum_type_basic_set() {
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
pub enum PointOrFloat {
    Point(Point2D),
    Float(f32),
}

#[test]
fn sum_type_struct() {
    xc_test!(
        "
            PointOrFloat = { Point: { x: i32, y: i32 } / Float: f32 }

            main(); PointOrFloat {
                x: PointOrFloat = PointOrFloat.Point { 
                    { x: i32, y: i32 } { x = 43, y = 54 }
                }
                ; x
            }
        ";
        PointOrFloat::Point(Point2D { x: 43, y: 54 })
    );
}

#[test]
fn sum_type_struct_2() {
    xc_test!(
        "
            PointOrFloat = { Point: { x: i32, y: i32 } / Float: f32 }

            main(); PointOrFloat {
                x: PointOrFloat = PointOrFloat.Float { 
                    245.93
                }
                ; x
            }
        ";
        PointOrFloat::Float(245.93)
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
fn option_rc() {
    xc_test!(
        use StdLib;
        "
            main(); Option< Rc<i32> > {
                output: Option< Rc<i32> > = 
                    Option< Rc<i32> >.Some { { Rc<i32> } { Rc<i32>:new(5) } }

                ; output
            }
        ";
        Option::<Rc<i32>>::Some(Rc::new(5))
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
            output: Option<&i32> = Option<&i32>.Some{ { &i32 } { (pointer) } }

            ; output
        }
        ",
    );

    let mut thirty_two = 32;
    let func = unit.get_fn_by_name::<&i32, Option<&i32>>("main");
    let output: Option<&i32> = unsafe { func(&mut thirty_two) };
    assert_eq!(output, Option::Some(&thirty_two));
}

#[test]
fn ref_and_more_option() {
    let mut unit = Unit::new();

    unit.add_lib(StdLib);

    unit.push_script(
        "
        main(pointer: &i32); Option<{i32, &i32}> {
            output: Option<{i32, &i32}> =
                Option<{i32, &i32}>.Some{ {i32, &i32} { 10, (pointer) }}
            ; output
        }
        ",
    );

    let mut thirty_two = 32;
    let func = unit.get_fn_by_name::<&i32, Option<(i32, &i32)>>("main");
    let output: Option<(i32, &i32)> = unsafe { func(&mut thirty_two) };
    assert_eq!(output, Option::Some((10, &32)));
}
