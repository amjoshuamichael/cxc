mod test_utils;

use cxc::library::StdLib;
use test_utils::{xc_test, Numbers5, Strings4};

#[test]
fn basic_struct() {
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
            x: f32,
            y: f32
        }

        &MyPoint:.sqr_hypotenuse(); f32 {
            ; self.x * self.x + self.y * self.y
        }

        &MyPoint:. {
            scaled(by: f32); MyPoint {
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
            original: [7]i32 = [1, 4, 8, 15, 16, 23, 42]

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
            points: [3]Point2D = [
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

#[test]
fn active_initialize_array_ints() {
    xc_test!(
        use StdLib;
        "
        main(); [5]i32 {
            numbers: [5]i32 = [10, 20, 90, ++]

            ; numbers
        }
        ";
        [10, 20, 90, 0, 0]
    );
}

#[test]
fn string_array() {
    xc_test!(
        use StdLib;
        r#"
        main(); [2]String {
            numbers: [2]String = ["one", "two"]
            ; numbers
        }
        "#;
        [String::from("one"), String::from("two")]
    );
}

#[test]
fn active_initialize_array_strings() {
    xc_test!(
        use StdLib;
        r#"
        main(); [5]String {
            numbers: [5]String = ["one", "two", "three", ++ ]

            ; numbers
        }
        "#;
        [String::from("one"), String::from("two"), String::from("three"), String::default(), String::default()]
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

        main() {
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
