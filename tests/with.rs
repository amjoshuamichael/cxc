mod test_utils;
use test_utils::xc_test;

#[test]
fn basic_as_with() {
    xc_test!(
        r#"
        main(); i32 {
            mything := { x = 439, y = 54 }
            with mything.x as x
            ; x
        }
        "#;
        439
    )
}

#[test]
fn with_value_as() {
    xc_test!(
        r#"
        main(); i32 {
            with 32 + 90 as x
            ; x
        }
        "#;
        122
    )
}

#[test]
fn scoped_as_with() {
    xc_test!(
        r#"
        main(); i32 {
            with 1 as x

            sum := 0
            sum = sum + x
            
            ? true {
                with 100 as x
                sum = sum + x
            }

            sum = sum + x

            ; sum
        }
        "#;
        102
    )
}

#[test]
fn double_scoped_as_with() {
    xc_test!(
        r#"
        main(); i32 {
            with 1 as x

            sum := 0
            sum = sum + x
            
            ? true {
                with 10 as x
                sum = sum + x
            }

            ? true {
                with 100 as x

                ? true {
                    ? true {
                        with 1000 as x

                        ? true { sum = sum + x }
                        sum = sum + x
                        ? true { sum = sum + x }
                    }

                    sum = sum + x
                }
                sum = sum + x
            }

            sum = sum + x

            ; sum
        }
        "#;
        3212
    )
}

#[test]
fn basic_with() {
    xc_test!(
        r#"
        main(); i32 {
            mything := { x = 439, y = 54 }

            with mything

            ; x + y
        }
        "#;
        439 + 54
    )
}

#[test]
fn scoped_with_1() {
    xc_test!(
        r#"
        main(); i32 {
            my_thing := { x = 1, y = 2 }
            my_other_thing := { x = 3, y = 4 }
            sum := 0

            with my_thing

            ? true {
                with my_other_thing

                ? true {
                    sum = sum + x + y
                }

                sum = sum + x + y
            }

            sum = sum + x + y
            ; sum
        }
        "#;
        1 + 2 + 3 + 4 + 3 + 4
    )
}

#[test]
fn scoped_with_2() {
    xc_test!(
        r#"
        main(); i32 {
            my_thing := { x = 1, y = 2 }
            my_other_thing := { x = 3, y = 4 }
            sum := 0

            with my_thing

            ? true {
                with my_other_thing

                ? true {
                    sum = sum + x + y
                }
            }

            sum = sum + x + y
            ; sum
        }
        "#;
        1 + 2 + 3 + 4
    )
}

#[test]
fn basic_nested_with() {
    xc_test!(
        r#"
        Nested = { +x: { z: i32 }, y: i32 }

        main(); i32 {
            mything: Nested = { x = { z = 439 }, y = 54 }

            with mything

            ; z + y
        }
        "#;
        439 + 54
    )
}
