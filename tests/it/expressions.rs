use super::test_utils;
use test_utils::xc_test;

#[test]
fn i() { xc_test!("; i32 { ; 1 }"; 1) }
#[test]
fn f() { xc_test!("; f32 { ; 1.0 }"; 1.0f32) }
#[test]
fn bool_basic() { xc_test!("; bool { ; true }"; true) }

#[test]
fn addi() { xc_test!("; i32 { ; 1 + 2 }"; 3) }
#[test]
fn subi() { xc_test!("; i32 { ; 100 - 57 }"; 43) }
#[test]
fn muli() { xc_test!("; i32 { ; 22 * 6 }"; 132) }
#[test]
fn divi() { xc_test!("; i32 { ; 290 / 5 }"; 58) }

#[test]
fn addf() { xc_test!("; f32 { ; 50.6 + 29.54 }"; 80.14f32) }
#[test]
fn subf() { xc_test!("; f32 { ; 100.39 - 61.39 }"; 39.0f32) }
#[test]
fn mulf() { xc_test!("; f32 { ; 1.2 * 1.2 }"; 1.44f32) }
#[test]
fn divf() { xc_test!("; f32 { ; 290.0 / 4.0 }"; 72.5f32) }

#[test]
fn order_of_op_1() { xc_test!("; i32 { ; 4 * 12 + 90 }"; 138) }
#[test]
fn order_of_op_2() { xc_test!("; i32 { ; 100 / 4 - 5 }"; 20) }
#[test]
fn order_of_op_3() { xc_test!("; i32 { ; 90 * 4 + 90 - 30 * 2 }"; 390) }

#[test]
fn parenthesis_1() { xc_test!("; i32 { ; (4 * 12) + 90 }"; 138) }
#[test]
fn parenthesis_2() { xc_test!("; i32 { ; 30 - (100 / 4) }"; 5) }
#[test]
fn parenthesis_3() { xc_test!("; i32 { ; 90 * (4 + 90) - 30 * 2 }"; 8400) }

#[test]
fn bool_complement() { xc_test!("; bool { ; !true }"; false) }
#[test]
fn double_bool_complement() { xc_test!("; bool { ; !!true }"; true) }

