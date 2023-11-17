use super::test_utils::xc_test;
use cxc::library::StdLib;

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
        main(); i32 {
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
fn vec_tally() {
    xc_test!(
        use StdLib;
        r#"
        main(); i32 {
            numbers := Vec<i32>:new()
            numbers.push(1)
            numbers.push(49830)
            numbers.push(2)
            numbers.push(23049)
            numbers.push(3)
            numbers.push(4)
            numbers.push(5)
            numbers.push(98450)

            sum := 0

            for tally(numbers.into_iter()) {
                ? it_index % 2 == 0 || it_index == 5 {
                    sum = sum + *it
                }
            }

            ; sum
        }
        "#;
        15
    );
}

#[test]
fn vec_map() {
    xc_test!(
        use StdLib;
        r#"
        main(); i32 {
            numbers := Vec<i32>:new()
            numbers.push(1)
            numbers.push(2)
            numbers.push(3)
            numbers.push(4)

            sum := 0

            for map(numbers.into_iter(), double) {
                sum = sum + it
            }

            ; sum
        }

        double(x: &i32); i32 { ; *x * 2 }
        "#;
        20
    );
}

#[test]
fn vec_both_tallymap() {
    xc_test!(
        use StdLib;
        r#"
        main(); i32 {
            numbers := Vec<i32>:new()
            numbers.push(1)
            numbers.push(49830)
            numbers.push(2)
            numbers.push(23049)
            numbers.push(3)
            numbers.push(4)
            numbers.push(5)
            numbers.push(98450)

            sum := 0

            for tally(map(numbers.into_iter(), double)) {
                ? it_index % 2 == 0 || it_index == 5 {
                    sum = sum + it
                }
            }

            ; sum
        }

        double(x: &i32); i32 { ; *x * 2 }
        "#;
        30
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
