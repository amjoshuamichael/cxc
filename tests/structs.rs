mod test_utils;
use cxc::library::StdLib;
use cxc::ExternalFuncAdd;
use cxc::LLVMContext;
use cxc::Type;
use cxc::TypeRelation;
use cxc::Unit;
use test_utils::{xc_test, Numbers5, Point2D};

#[test]
fn basic_struct() {
    xc_test!(
        "
            x: Point2D = Point2D { x = 30, y = 90 }
            ; x.x + x.y
        "
        => i32;
        120
    )
}

#[test]
fn set_to_with_function() {
    xc_test!(
        "
            double(in: i32): i32 { ; in * 2 }

            main(): i32 {
                x: Point2D = Point2D { x = double(2), y = 9 }
                ; x.x + x.y
            }
        ";
        13
    )
}

#[test]
fn anonymous() {
    xc_test!(
        "
            x: { x: i32, y: i32 } = 0
            x.x = 30
            x.y = 90
            ; x.x + x.y
        "
        => i32;
        120
    )
}

#[test]
fn anonymous_extra() {
    xc_test!(
        "
            x: { x: i32, y: { x: i32, y: i32 } } = 0
            x.x = 30
            x.y.x = 40
            x.y.y = 50
            ; x.x + x.y.x + x.y.y
        "
        => i32;
        120
    )
}

#[test]
fn generics_i() {
    xc_test!(
        "
            x: TwoOf<i32> = TwoOf<i32> { one = 42, two = 32 }
            ; x.one + x.two
        "
        => i32;
        74
    )
}

#[test]
fn generics_f() {
    xc_test!(
        "
            x: TwoOf<f32> = TwoOf<f32> { one = 42.4, two = 32.3 }
            ; x.one + x.two
        "
        => f32;
        74.7
    )
}

#[test]
fn opaque_types() {
    let context = LLVMContext::new();
    let mut unit = Unit::new(&context);

    let point_2d_opaque = unit.add_opaque_type::<Point2D>();
    unit.add_rust_func_explicit(
        "sqr_magnitude",
        Point2D::sqr_magnitude as *const usize,
        ExternalFuncAdd {
            arg_types: vec![point_2d_opaque.clone().get_ref()],
            ret_type: Type::i(32),
            relation: TypeRelation::MethodOf(point_2d_opaque.get_ref()),
            ..ExternalFuncAdd::empty()
        },
    );

    unit.push_script(
        r#"
        hello_world(input: Point2D): i32 {
            output: i32 = input.sqr_magnitude()
            ; output
        }
        "#,
    );

    let point = Point2D { x: 2, y: 3 };
    let output = unsafe { unit.get_fn_by_name::<_, i32>("hello_world")(point) };

    assert_eq!(output, point.sqr_magnitude());
}

#[test]
fn methods() {
    xc_test!(
        "
        MyPoint = {
            x: f32,
            y: f32
        }

        &MyPoint.sqr_hypotenuse(): f32 {
            ; self.x * self.x + self.y * self.y
        }

        &MyPoint. {
            scaled(by: f32): MyPoint {
                ; MyPoint { x = self.x * by, y = self.y * by }
            }
        }
        
        main() {
            original: MyPoint = MyPoint { x = 4.0, y = 3.0 }

            out_hypotenuse: f32 = original.sqr_hypotenuse()
            assert_eq<f32>(out_hypotenuse, 25.0)

            scaled_by_2: MyPoint = original.scaled(1.5)
            assert_eq<f32>(scaled_by_2.x, 6.0)
            assert_eq<f32>(scaled_by_2.y, 4.5)
        }
        "
    )
}

#[test]
fn arrays() {
    xc_test!(
        "
        main() {
            original: i32[7] = [1, 4, 8, 15, 16, 23, 42]

            assert_eq<i32>(original[3], 15)
            assert_eq<i32>(original[0], 1)
            assert_eq<i32>(original[6], 42)

            index: i32 = 0
            @ index < 7 {
                original[index] = index * 2

                index = index + 1
            }

            assert_eq<i32>(original[0], 0)
            assert_eq<i32>(original[1], 2)
            assert_eq<i32>(original[3], 6)
            assert_eq<i32>(original[6], 12)
        }
        "
    )
}

#[test]
fn struct_arrays() {
    xc_test!(
        "
        Point2D = {
            x: i32,
            y: i32
        }

        main() {
            points: Point2D[3] = [
                Point2D { x = 43, y = 15 },
                Point2D { x = 327, y = 413 },
                Point2D { x = 1672, y = 2526 },
            ]

            assert_eq<i32>(points[0].x, 43)
            assert_eq<i32>(points[0].y, 15)

            points[0].x = 94

            assert_eq<i32>(points[0].x, 94)
            assert_eq<i32>(points[0].y, 15)

            points[1] = Point2D { x = 4, y = 6 }

            assert_eq<i32>(points[1].x, 4)
            assert_eq<i32>(points[1].y, 6)
        }
        "
    )
}

#[test]
fn active_initialize() {
    xc_test!(
        use StdLib;
        "
        main(): Numbers5 {
            numbers: Numbers5 = Numbers5 { a = 10, b = 20, e = 90, ++ }

            ; numbers
        }
        ";
        Numbers5 {
            a: 10,
            b: 20,
            e: 90,
            ..Default::default()
        }
    );
}

#[derive(Copy, Clone, Debug, PartialEq)]
struct ThisUnion {
    a: i32,
    b: i32,
    c: f32,
    d: i32,
    e: f32,
}

#[test]
fn unions() {
    xc_test!(
        "
        Num2 = { a: i32, b: i32 }
        Num3<T> = { c: T, d: i32, e: T }
        Union = Num2 + Num3<f32>

        main(): Union {
            ; Union { a = 10, b = 20, e = 90.9, d = 30, c = 32.7 }
        }
        ";
        ThisUnion {
            a: 10,
            b: 20,
            e: 90.9,
            d: 30,
            c: 32.7,
        }
    );
}
