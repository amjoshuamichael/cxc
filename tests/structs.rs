mod test_utils;

use std::rc::Rc;

use cxc::{library::StdLib, Unit};
use test_utils::{xc_test, Numbers5, Strings4};

#[test]
fn basic_struct() {
    xc_test!(
        "
        ; i32 {
            x: Point2D = { -- }
            x.x = 30
            x.y = 90
            ; x.x + x.y
        }
        ";
        120
    )
}

#[test]
fn struct_lit() {
    xc_test!(
        "
        ; i32 {
            x: Point2D = Point2D { x = 30, y = 90 }
            ; x.x + x.y
        }
        ";
        120
    )
}

#[test]
fn set_to_with_function() {
    xc_test!(
        "
        double(in: i32); i32 { ; in * 2 }

        main(); i32 {
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
        ; i32 {
            x: { x: i32, y: i32 } = {--}
            x.x = 30
            x.y = 90
            ; x.x + x.y
        }
        ";
        120
    )
}

#[test]
fn anonymous_extra() {
    xc_test!(
        "
        ; i32 {
            x: { x: i32, y: { x: i32, y: i32 } } = {--}
            x.x = 30
            x.y.x = 40
            x.y.y = 50
            ; x.x + x.y.x + x.y.y
        }
        "; 
        120
    )
}

#[test]
fn generics_i() {
    xc_test!(
        "
        ; i32 { 
            x: TwoOf<i32> = TwoOf<i32> { one = 42, two = 32 }
            ; x.one + x.two
        }
        ";
        74
    )
}

#[test]
fn generics_f() {
    xc_test!(
        "
        ; f32 {
            x: TwoOf<f32> = TwoOf<f32> { one = 42.4, two = 32.3 }
            ; x.one + x.two
        }
        ";
        74.7f32
    )
}

#[test]
fn methods() {
    xc_test!(
        "
        MyPoint = {
            x: i32,
            y: i32
        }

        &MyPoint:.sqr_hypotenuse(); i32 {
            ; self.x * self.x + self.y * self.y
        }

        &MyPoint:. {
            scaled(by: i32); MyPoint {
                ; MyPoint { x = self.x * by, y = self.y * by }
            }
        }
        
        main(); i32 {
            original: MyPoint = MyPoint { x = 4, y = 3 }

            out_hypotenuse: i32 = original.sqr_hypotenuse()

            scaled_by_2: MyPoint = original.scaled(2)

            ; scaled_by_2.x + scaled_by_2.y + out_hypotenuse
        }
        ";
        4 * 2 + 3 * 2 + 4 * 4 + 3 * 3
    )
}

#[test]
#[ignore]
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
#[ignore]
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
#[ignore]
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
            sqr_hypotenuse: f32 = rc_point.sqr_hypotenuse()

            ; sqr_hypotenuse
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
fn active_initialize_struct_ints() {
    xc_test!(
        use StdLib;
        "
        main(); Numbers5 {
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

#[test]
fn active_initialize_struct_strings() {
    xc_test!(
        use StdLib;
        r#"
        main(); Strings4 {
            numbers: Strings4 = Strings4 { a = "Aa", b = "Bb", ++ }

            ; numbers
        }
        "#;
        Strings4 {
            a: "Aa".into(),
            b: "Bb".into(),
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

        main(); Union {
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

#[test]
fn deref() {
    xc_test!(
        "
        Num = i32
        NumRef = &Num
        NumAgain = *NumRef

        main(); NumAgain {
            x: Num = 5
            y: NumRef = &x
            z: NumAgain = *y
            ; z
        }
        ";
        5
    )
}

#[test]
fn field_of_struct_pointer() {
    xc_test!(
        "
        Point2D = { x: i32, y: i32 }

        main() {
            point: Point2D = { x = 5, y = 10 }
            point_ref: &Point2D = &point

            ; point_ref.x + point_ref.y
        }
        ";
        15
    )
}
