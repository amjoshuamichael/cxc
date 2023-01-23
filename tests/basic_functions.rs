mod test_utils;
use test_utils::xc_test;

#[test]
fn assign_i() {
    xc_test!(
        "
            x: i32 = 300
            ; x
        "
        => i32;
        300
    )
}

#[test]
fn assign_f() {
    xc_test!(
        "
            x: f32 = 320.7
            ; x
        "
        => f32;
        320.7
    )
}

#[test]
fn assign_bool() {
    xc_test!(
        "
            x: bool = true
            ; x
        "
        => bool;
        true
    )
}

#[test]
fn factorial_while() {
    xc_test!(
        "
            current: i32 = 5
            output: i32 = 1

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
