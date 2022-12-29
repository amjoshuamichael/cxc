mod test_utils;

use cxc::library::StdLib;
use cxc::ExternalFuncAdd;
use cxc::LLVMContext;
use cxc::Type;
use cxc::TypeRelation;
use cxc::Unit;
use test_utils::{xc_test, Numbers5, Point2D};

#[test]
fn basic_tuple() {
    xc_test!(
        "
            x: { i32, i32 } = { i32, i32 } { 90, 43 }
            ; x.0 - x.1
        "
        => i32;
        47
    )
}

#[test]
fn nested_tuple() {
    xc_test!(
        "
            x: { { i32, i32 }, i32 } = 
                { { i32, i32 }, i32 } 
                    { {i32, i32} { 90, 60 }, 43 }
            ; x.0.0 - x.0.1 + x.1
        "
        => i32;
        73
    )
}

#[test]
fn tuple_method() {
    xc_test!(
        "
            &{ i32, i32 }.add(): i32 {
                ; self.0 + self.1
            }

            main() {
                x: { i32, i32 } = { i32, i32 } { 30, 90 }
                assert_eq<i32>(x.add(), 120)
            }
        "
    )
}

#[test]
fn nested_structural_type_method() {
    xc_test!(
        "
            &{ i32, i32 }.add(): i32 {
                ; self.0 + self.1
            }

            main() {
                x: { { i32, i32 }, i32 } = 
                    { { i32, i32 }, i32 } 
                        { { i32, i32 } { 90, 30 }, 90 }

                assert_eq<i32>(x.0.add(), 120)
            }
        "
    )
}
