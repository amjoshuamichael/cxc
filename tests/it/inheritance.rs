use std::rc::Rc;

use cxc::{Unit, library::StdLib};
use super::test_utils::xc_test;

#[test]
fn auto_deref_method_1() {
    xc_test!(
        use StdLib;
        r#"
        MyPoint = { x: f32, y: f32 }

        &MyPoint:.sqr_hypotenuse(); f32 {
            ; self.x * self.x + self.y * self.y
        }

        main(); f32 {
            rc_point: Rc<MyPoint> = Rc<MyPoint>:new(MyPoint { x = 4.0, y = 3.0 })
            sqr_hyp: f32 = rc_point.sqr_hypotenuse()
            ; sqr_hyp
        }
        "#;
        25.0f32
    )
}

#[derive(Debug, PartialEq)]
struct MyPoint {
    x: f32,
    y: f32,
}

#[test]
#[cfg(not(feature = "backend-interpreter"))]
fn auto_deref_method_2() {
    let mut unit = Unit::new();
    unit.add_lib(StdLib);

    unit.push_script(
        r#"
        MyPoint = { x: f32, y: f32 }

        &MyPoint:.sqr_hypotenuse(); f32 {
            ; self.x * self.x + self.y * self.y
        }

        main(rc_point: Rc<MyPoint>); f32 {
            value: &MyPoint = &rc_point.value
            result: f32 = value.sqr_hypotenuse()

            ; result
        }
        "#,
    )
    .unwrap();

    let func = unit
        .get_fn("main")
        .unwrap()
        .downcast::<(Rc<MyPoint>,), f32>();

    let rc = Rc::new(MyPoint { x: 4.0, y: 3.0 });
    assert_eq!(func(rc), 25.0);
}

#[test]
fn auto_deref_member() {
    xc_test!(
        use StdLib;
        "
        MyPoint = { x: f32, y: f32 }

        main(); f32 {
            x: {&&&MyPoint}.x = 4.0
            y: {&&Rc<MyPoint>}.x = 4.0
            z: {&&Rc<&MyPoint>}.x = 4.0

            ; x + y + z
        }
        ";
        12.0f32
    )
}

#[test]
fn big_rc_sum() {
    xc_test!(
        use StdLib;
        "
        &{i32, i32}:.sum(); i32 {
            ; self.0 + self.1
        }

        main(); i32 {
            rcx: Rc<{i32, i32}> = Rc<{i32, i32}>:new({ 80, 100 })
            output := rcx.sum()
            ; output
        }
        ";
        180
    )
}

#[test]
#[cfg(not(feature = "backend-interpreter"))]
fn big_rc_to_string() {
    xc_test!(
        use StdLib;
        "
        main(); String {
            rcx: Rc<{i32, i32}> = Rc<{i32, i32}>:new({ 90, 90 })
            output := rcx.to_string()

            ; output
        }
        ";
        String::from("{0 = 90, 1 = 90}")
    )
}

#[test]
fn union_inheritance() {
    xc_test!(
        "
        Point = {
            x: f32,
            y: f32,
        }

        Shape = {
            tag: u8,
            +union: {
                +line: {
                    start: Point,
                    end: Point,
                } |
                +square: {
                    sides: [4]i32
                }
            }
        }

        main(); { Shape.union.square, Shape.union.line, } {
            square: Shape = {--}
            square.tag = 0
            square.sides = [0, 1, 2, 3]

            line: Shape = {--}
            line.tag = 1
            line.start.x = 0.0
            line.start.y = 1.0
            line.end = { x = 2.0, y = 3.0 }

            ; { square.union.square, line.union.line }
        }
        ";
        ([0, 1, 2, 3], (0.0f32, 1.0f32, 2.0f32, 3.0f32))
    )
}
