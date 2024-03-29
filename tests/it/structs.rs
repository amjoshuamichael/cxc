use cxc::library::StdLib;
use super::test_utils::{xc_test, Numbers5, Strings4};

#[test]
fn basic_struct() {
    xc_test!(
        "
        main(); i32 {
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
#[cfg(not(feature = "backend-interpreter"))]
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
fn active_initialize_struct_arrays_1() {
    xc_test!(
        use StdLib;
        r#"
        main(); { [5]u32, [5]f32 } {
            ; { ++ }
        }
        "#;
        ( [0; 5], [0.0f32; 5] )
    );
}

#[test]
fn active_initialize_struct_arrays_2() {
    xc_test!(
        use StdLib;
        r#"
        main(); { [50]u32, [50]f32 } {
            ; { ++ }
        }
        "#;
        ( [0u32; 50], [0.0f32; 50] )
    );
}

#[test]
fn active_initialize_struct_arrays_3() {
    xc_test!(
        use StdLib;
        r#"
        main(); { [500]u32, [500]f32 } {
            ; { ++ }
        }
        "#;
        ( [0u32; 500], [0.0f32; 500] )
    );
}

#[test]
fn active_initialize_struct_arrays_4() {
    xc_test!(
        use StdLib;
        r#"
        main(); { [500]f32, [500]u32 } {
            ; { ++ }
        }
        "#;
        ( [0.0f32; 500], [0u32; 500] )
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
            x: Num = cast<i32, Num>(5)
            y: NumRef = cast<&Num, NumRef>(&x)
            z: NumAgain = cast<NumRef, NumAgain>(*y)
            ; z
        }
        ";
        5
    )
}

#[test]
fn field_of_struct_pointer_1() {
    xc_test!(
        "
        Point2D = { x: i32, y: i32 }

        main(); i32 {
            point: Point2D = { x = 5, y = 10 }
            point_ref: &Point2D = &point

            ; point_ref.x + point_ref.y
        }
        ";
        15
    )
}

#[test]
fn field_of_struct_pointer_2() {
    xc_test!(
    "
    Point2D = { x: i32, y: i32 }

    main(); i32 {
        point: { Point2D, } = { { x = 5, y = 5 }, }
        point_ref: &Point2D = &point.0
        point.0.y = 10

        ; point_ref.x + point_ref.y
    }
    ";
    15
    )
}

#[test]
fn field_of_struct_pointer_3() {
    xc_test!(
    "
    Point2D = { x: i32, y: i32 }

    main(); i32 {
        point: { { Point2D, }, } = { { { x = 5, y = 5 }, }, }
        point_container_ref: &{ Point2D, } = &point.0
        point_ref: &Point2D = &point_container_ref.0
        point.0.0.y = 10

        ; point_ref.x + point_ref.y
    }
    ";
    15
    )
}

#[test]
fn field_of_struct_pointer_4() {
    xc_test!(
    "
    Point2D = { x: i32, y: i32 }

    Holder = {
        p1: i64,
        p2: i64,
        value: Point2D,
    } 

    HolderRef = { ptr: &Holder }

    get_ref_to_value(holder_ref: &HolderRef); &Point2D {
        ; &holder_ref.ptr.value
    }

    main(); i32 {
        holder: Holder = { p1=0, p2=0, value = { x=5, y=5 } }
        holder_ref: HolderRef = { ptr = &holder }
        value_ptr: &Point2D = get_ref_to_value(&holder_ref)
        holder.value.y = 10

        ; value_ptr.x + value_ptr.y
    }
    ";
    15
    )
}
