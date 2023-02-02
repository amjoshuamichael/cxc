#![feature(assert_matches)]

mod test_utils;
use cxc::error::CompError;
use cxc::Unit;
use std::assert_matches::assert_matches;
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
fn double_error() {
    let mut unit = Unit::new();
    let mut errors = unit
        .push_script(
            r#"
        main() {
            x = 100 + +
            y = 100 + 20 - 
        }
        "#,
        )
        .unwrap_err();
    let mut iter = errors.drain(..);
    assert_matches!(iter.next(), Some(CompError::Parse(_)));
    assert_matches!(iter.next(), Some(CompError::Parse(_)));
    assert_matches!(iter.next(), None);
}
