mod test_utils;
use cxc::library::StdLib;
use test_utils::xc_test;

#[test]
fn infer_i() {
    xc_test!(
        "
            x = 300
            ; x
        "
        => i32;
        300
    )
}

#[test]
fn infer_f() {
    xc_test!(
        "
            x = 320.7
            ; x
        "
        => f32;
        320.7
    )
}

#[test]
fn infer_b() {
    xc_test!(
        "
            x = true
            ; x
        "
        => bool;
        true
    )
}

#[test]
fn infer_struct() {
    xc_test!(
        "
            main(); i32 {
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
            main(); i32 {
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
            current = 5
            output = 1

            @ current > 0 {
                output = output * current
                current = current - 1
            }

            ; output
        "
        => i32;
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
        r#"
        main() {
            x = Vec<i32>:new()
            x.push(432)
            assert_eq(x.get(0), 432)
        }
        "#
    )
}

#[test]
fn infer_alloc() {
    xc_test!(
        r#"
        main() {
            ptr: &i32 = alloc(1)
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
        r#"
        main() {
            x: [4]i32 = [ 90, 43, ++ ]
            assert_eq(x[0], 90)
            assert_eq(x[1], 43)
            assert_eq(x[2], 0)
            assert_eq(x[3], 0)
        }
        "#
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
