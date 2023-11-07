use super::test_utils;
use test_utils::xc_test;

#[test]
fn assign_i() {
    xc_test!(
        "
        ; i32 {
            x: i32 = 300
            ; x
        }
        ";
        300
    )
}

#[test]
fn assign_f() {
    xc_test!(
        "
        ; f32 {
            x: f32 = 320.7
            ; x
        }
        ";
        320.7f32
    )
}

#[test]
fn assign_bool() {
    xc_test!(
        "
        ; bool { 
            x: bool = true
            ; x
        }
        ";
        true
    )
}

#[test]
fn factorial_while() {
    xc_test!(
        "
        ; i32 {
            current: i32 = 5
            output: i32 = 1

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
