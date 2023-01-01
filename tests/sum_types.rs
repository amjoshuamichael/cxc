mod test_utils;

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

            main(): IntOrFloat {
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

            main(): IntOrFloat {
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

            main(): PointOrFloat {
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

            main(): PointOrFloat {
                x: PointOrFloat = PointOrFloat.Float { 
                    245.93
                }
                ; x
            }
        ";
        PointOrFloat::Float(245.93)
    );
}
