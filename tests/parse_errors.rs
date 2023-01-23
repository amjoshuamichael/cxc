mod test_utils;
use test_utils::xc_test;

// TODO: this doesn't throw an error.
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
