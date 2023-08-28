#![allow(arithmetic_overflow)]

mod test_utils;
use cxc::library::StdLib;
use test_utils::xc_test;

#[test]
fn infer_i() {
    xc_test!(
        "
        ; i32 {
            x = 300
            ; x
        }
        ";
        300
    )
}

#[test]
fn infer_f() {
    xc_test!(
        "
        ; f32 {
            x = 320.7
            ; x
        }
        ";
        320.7f32
    )
}

#[test]
fn infer_b() {
    xc_test!(
        "
        ; bool {
            x = true
            ; x
        }
        ";
        true
    )
}

#[test]
fn infer_struct() {
    xc_test!(
        "
        ; i32 {
            twonums = { x = 43, y = 94 }
            ; twonums.y
        }
        ";
        94
    )
}

#[test]
fn infer_tuple() {
    xc_test!(
        "
        ; i32 {
            twonums = { 43, 94 }
            ; twonums.1
        }
        ";
        94
    )
}

#[test]
fn factorial_while() {
    xc_test!(
        "
        ; i32 {
            current = 5
            output = 1

            @ current > 0 {
                output = output * current
                current = current - 1
            }

            ; output
        }
        ";
        120
    )
}

#[test]
#[ignore]
fn hello_world() {
    xc_test!(
        r#"
        main() {
            print("hello, world!")
        }
        "#
    )
}

#[test]
fn infer_vec_push() {
    xc_test!(
        use StdLib;
        r#"
        main() {
            x = Vec<i32>:new<i32>()
            x.push(432)
            x.get(0)
        }
        "#;
        432
    )
}

#[test]
fn infer_alloc() {
    xc_test!(
        r#"
        main() {
            ptr: &i32 = alloc(i64 1)
            *ptr = 4
        }
        "#
    )
}

#[test]
fn infer_default_struct() {
    xc_test!(
        r#"
        Defaultable = { x: i32, y: i32 }

        main() {
            x = Defaultable { ++ } 
            assert_eq(x.x, 0)
            assert_eq(x.y, 0)
        }
        "#
    )
}

#[test]
fn infer_default_array() {
    xc_test!(
        use StdLib;
        r#"
        main() {
            x: [4]i32 = [ 90, 43, ++ ]
            assert_eq(x[i64 0], 90)
            assert_eq(x[i64 1], 43)
            assert_eq(x[i64 2], 0)
            assert_eq(x[i64 3], 0)
        }
        "#;
        ()
    )
}

#[test]
fn infer_cast() {
    xc_test!(
        use StdLib;
        r#"
        main(); i64 {
            ; cast( { 90, 8943 } )
        }
        "#;
        unsafe { std::mem::transmute::<(i32, i32), i64>((90, 8943)) }
    )
}

#[test]
fn infer_int_size_u8() {
    xc_test!(
        r#"
        # notifies the compiler that whatever variable is passed into this function is a u8
        takes_a_u8(a: u8) { }

        main(); bool {
            x = 90

            takes_a_u8(x)

            # this 250 should become a u8, and so should y. y should overflow, 
            # causing it to be less than x
            y = 250 + x

            ; x > y
        }
        "#;
        true
    )
}

#[test]
fn infer_float_size_f64_1() {
    xc_test!(
        r#"
        takes_an_f64(a: f64) { }

        size_of_val<T>(val: &T); u64 {
            ; size_of<T>()
        }

        main(); bool {
            x = 90.0

            takes_an_f64(x)

            y = 250.0 + x

            ; size_of_val(&y) == 8
        }
        "#;
        true
    )
}

#[test]
fn infer_float_size_f64_2() {
    xc_test!(
        r#"
        takes_an_f64(a: f64) { }

        main(); f64 {
            x = 90.0

            takes_an_f64(x)

            y = 250.0 + x

            ? y > 250.0 + 80.0 {
                ; y
            }

            ; 0.0
        }
        "#;
        250.0f64 + 90.0f64
    )
}

#[test]
fn infer_struct_fields_with_arg() {
    xc_test!(
        use StdLib;
        r#"
        takes_3_nums(me: {x: i32, y: i32, z: i32}) {}

        main(); i32 {
            three = { x = 43, y = 65, ++ }

            takes_3_nums(three)

            x = three.x + three.y + three.z

            ; x
        }
        "#;
        43 + 65
    )
}

#[test]
fn infer_struct_fields_with_member() {
    xc_test!(
        use StdLib;
        r#"
        main(); i32 {
            three = { x = 43, y = 65, ++ }

            x = three.x + three.y + three.z

            ; x
        }
        "#;
        43 + 65
    )
}

#[test]
fn infer_set_struct_fields_basic() {
    xc_test!(
        r#"
        size_of_val<T>(val: &T); i32 { ; cast(size_of<T>()) }

        main(); i32 {
            three = { -- }
            three.x = 90
            three.y = 48
            three.z = 39

            ; three.x + three.y + three.z + size_of_val(&three)
        }
        "#;
        90 + 48 + 39 + 12
    )
}

#[test]
fn infer_set_struct_fields_and_infer_number_size() {
    xc_test!(
        r#"
        size_of_val<T>(val: &T); u64 { ; size_of<T>() }

        main(); u64 {
            three = { -- }
            three.x = 90
            three.y = 48
            three.z = 39

            ; three.x + three.y + three.z + size_of_val(&three)
        }
        "#;
        90u64 + 48u64 + 39u64 + 24u64
    )
}

#[test]
fn infer_number_u32_in_struct() {
    xc_test!(
        r#"
        size_of_val<T>(val: &T); u64 { ; size_of<T>() }

        main(); u64 {
            twonums = { 30, 40 }

            ; size_of_val(&twonums)
        }
        "#;
        8u64
    )
}

#[test]
fn infer_number_u8_in_struct() {
    xc_test!(
        r#"
        size_of_val<T>(val: &T); u64 { ; size_of<T>() }
        takes_a_u8(val: u8) {}

        main(); u64 {
            twonums = { 30, 40 }
            takes_a_u8(twonums.0)
            takes_a_u8(twonums.1)

            ; size_of_val(&twonums)
        }
        "#;
        2u64
    )
}

#[test]
fn infer_array_len() {
    xc_test!(
        use StdLib;
        r#"
        takes_a_big_array(val: &[200]u8) {}

        main(); u64 {
            supposed_to_be_big = [432, 656, ++]
            takes_a_big_array(&supposed_to_be_big)

            ; supposed_to_be_big.len()
        }
        "#;
        200u64
    )
}
