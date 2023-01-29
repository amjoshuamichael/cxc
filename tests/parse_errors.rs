mod test_utils;
use test_utils::xc_test;

#[test]
#[should_panic]
fn improper_expression() {
    xc_test!(
        "
        main() {
            x = 100 +
        }
        "
    )
}

#[test]
#[should_panic]
fn double_error() {
    xc_test!(
        "
        main() {
            x = 100 + +
            y = 100 + 20 - 
        }
        "
    )
}
