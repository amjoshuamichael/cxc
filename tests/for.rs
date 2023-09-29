mod test_utils;
use cxc::library::StdLib;
use test_utils::xc_test;

#[test]
fn basic_for() {
    xc_test!(
        use StdLib;
        r#"
        ; i32 {
            numbers := [4, 90, 32, 9]
            sum := 0

            for numbers as number {
                sum = sum + *number
            }

            ; sum
        }
        "#;
        135
    );
}

#[test]
fn basic_it_for() {
    xc_test!(
        use StdLib;
        r#"
        ; i32 {
            numbers: [4]i32 = [4, 90, 32, 9]
            sum: i32 = 0

            for numbers {
                sum = sum + *it
            }

            ; sum
        }
        "#;
        135
    );
}

#[test]
fn vec_for() {
    xc_test!(
        use StdLib;
        r#"
        ; i32 {
            numbers := Vec<i32>:new()
            numbers.push(4)
            numbers.push(90)
            numbers.push(32)
            numbers.push(9)

            sum := 0

            for numbers {
                sum = sum + *it
            }

            ; sum
        }
        "#;
        135
    );
}

#[test]
fn break_inner_with_as() {
    xc_test!(
        use StdLib;
        r#"
        ; i32 {
            numbers: [5]i32 = [1, 2, 3, 40, 4]
            bools: [4]bool = [true, true, false, true]

            sum: i32 = 0

            for numbers as number {
                ? *number == 40 {
                    ;:break
                }

                for bools as a_bool {
                    ? *a_bool {
                        sum = sum + *number
                    } : {
                        ;:continue
                    }
                }
            }

            ; sum
        }
        "#;
        18
    );
}