#![feature(assert_matches)]

mod test_utils;
use cxc::error::CErr;
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
    assert_matches!(iter.next(), Some(CErr::Parse(_)));
    assert_matches!(iter.next(), Some(CErr::Parse(_)));
    assert_matches!(iter.next(), None);
}

#[test]
fn in_struct_expression() {
    let mut unit = Unit::new();
    let mut errors = unit
        .push_script(
            r#"
            main() {
                x = { x: i32, y: i32 } { 
                    x = 1 + +,
                    y = 2 + 20 -,
                }
            }
            "#,
        )
        .unwrap_err();
    let mut iter = errors.drain(..);
    assert_matches!(iter.next(), Some(CErr::Parse(_)));
    assert_matches!(iter.next(), Some(CErr::Parse(_)));
    assert_matches!(iter.next(), None);
}

#[test]
#[should_panic]
fn unknown_type() {
    let mut unit = Unit::new();

    let mut errors = unit
        .push_script(
            r#"
            main(x: NotReal); i32 {
                ; 0
            }
            "#,
        )
        .unwrap_err();
    let mut iter = errors.drain(..);
    assert_matches!(iter.next(), Some(CErr::Parse(_)));
    assert_matches!(iter.next(), Some(CErr::Parse(_)));
    assert_matches!(iter.next(), None);
}
