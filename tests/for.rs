mod test_utils;
use cxc::library::StdLib;
use test_utils::xc_test;

#[test]
#[ignore]
fn basic_for() {
    xc_test!(
        use StdLib;
        r#"
        ; i32 {
            numbers: [4]i32 = [4, 90, 32, 9]
            sum: i32 = 0

            for numbers as number {
                sum = sum + number
            }

            ; sum
        }
        "#;
        135
    );
}
