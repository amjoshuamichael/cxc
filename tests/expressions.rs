mod test_utils;
use test_utils::xc_test;

#[test]
fn i() { xc_test!("; 1" => i32; 1) }
#[test]
fn f() { xc_test!("; 1.0" => f32; 1.0) }
#[test]
fn bool() { xc_test!("; true" => bool; true) }

#[test]
fn addi() { xc_test!("; 1 + 2" => i32; 3) }
#[test]
fn subi() { xc_test!("; 100 - 57" => i32; 43) }
#[test]
fn muli() { xc_test!("; 22 * 6" => i32; 132) }
#[test]
fn divi() { xc_test!("; 290 / 5" => i32; 58) }

#[test]
fn addf() { xc_test!("; 50.6 + 29.54" => f32; 80.14) }
#[test]
fn subf() { xc_test!("; 100.39 - 61.39" => f32; 39.0) }
#[test]
fn mulf() { xc_test!("; 1.2 * 1.2" => f32; 1.44) }
#[test]
fn divf() { xc_test!("; 290.0 / 4.0" => f32; 72.5) }

#[test]
fn order_of_op_1() { xc_test!("; 4 * 12 + 90" => i32; 138) }
#[test]
fn order_of_op_2() { xc_test!("; 100 / 4 - 5" => i32; 20) }
#[test]
fn order_of_op_3() { xc_test!("; 90 * 4 + 90 - 30 * 2" => i32; 390) }

#[test]
fn bool_complement() { xc_test!("; !true" => bool; false) }
#[test]
fn double_bool_complement() { xc_test!("; !!true" => bool; true) }

#[test]
fn ret_if() { xc_test!("? true { ; 100 } ; 20" => i32; 100) }
#[test]
fn ret_if_no() { xc_test!("? false { ; 100 } ; 20" => i32; 20) }

// TODO:
// these do not work.
//
// The first two don't work because LLVM needs to see a return at the END of a
// function in order to compile it.
//
// The second two don't work because the to_llvm module does not support return
// an if-else statement. Both of these can be fixed by adding extra HLR passes.
#[test]
#[ignore]
fn ret_if_else() { xc_test!("? true { ; 100 } : { ; 20 }" => i32; 100) }
#[test]
#[ignore]
fn ret_if_else_no() { xc_test!("? false { ; 1 } : { ; 2 }" => i32; 2) }
#[test]
#[ignore]
fn expr_if_else() { xc_test!("; ? true { 10 } : { 2 }" => i32; 10) }
#[test]
#[ignore]
fn expr_if_else_no() { xc_test!("? false { 10 } : { 2 }" => i32; 20) }
