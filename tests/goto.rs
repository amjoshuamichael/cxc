mod test_utils;
use test_utils::xc_test;
use cxc::library::StdLib;

#[test]
fn basic_goto() {
    xc_test!(
        r#"
        ; i32 {
            x := 1

            :repeat

            x = x + 1

            ? x != 40 { ;:repeat }

            ; x
        }
        "#;
        40
    );
}

#[test]
fn goto_at_start() {
    xc_test!(
        r#"
        ; i32 {
            :repeat

            x := 1

            ? false { ;:repeat }

            ; x
        }
        "#;
        1
    );
}

#[test]
fn scoped_goto() {
    xc_test!(
        use StdLib;
        r#"
        ; i32 {
            x := 1

            :repeat

            x = x + 2

            ? true {
                :repeat

                x = x - 1

                ? x < 0 {
                    ; x
                }

                ;:repeat
            }

            ; x
        }
        "#;
        -1
    );
}
