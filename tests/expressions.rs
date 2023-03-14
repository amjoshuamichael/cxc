mod test_utils;
use test_utils::xc_test;

#[test]
fn i() { xc_test!("{ ; 1 }"; 1) }
#[test]
fn f() { xc_test!("{ ; 1.0 }"; 1.0f32) }
#[test]
fn bool() { xc_test!("{ ; true }"; true) }

#[test]
fn addi() { xc_test!("{ ; 1 + 2 }"; 3) }
#[test]
fn subi() { xc_test!("{ ; 100 - 57 }"; 43) }
#[test]
fn muli() { xc_test!("{ ; 22 * 6 }"; 132) }
#[test]
fn divi() { xc_test!("{ ; 290 / 5 }"; 58) }

#[test]
fn addf() { xc_test!("{ ; 50.6 + 29.54 }"; 80.14f32) }
#[test]
fn subf() { xc_test!("{ ; 100.39 - 61.39 }"; 39.0f32) }
#[test]
fn mulf() { xc_test!("{ ; 1.2 * 1.2 }"; 1.44f32) }
#[test]
fn divf() { xc_test!("{ ; 290.0 / 4.0 }"; 72.5f32) }

#[test]
fn order_of_op_1() { xc_test!("{ ; 4 * 12 + 90 }"; 138) }
#[test]
fn order_of_op_2() { xc_test!("{ ; 100 / 4 - 5 }"; 20) }
#[test]
fn order_of_op_3() { xc_test!("{ ; 90 * 4 + 90 - 30 * 2 }"; 390) }

#[test]
fn parenthesis_1() { xc_test!("{ ; (4 * 12) + 90 }"; 138) }
#[test]
fn parenthesis_2() { xc_test!("{ ; 30 - (100 / 4) }"; 5) }
#[test]
fn parenthesis_3() { xc_test!("{ ; 90 * (4 + 90) - 30 * 2 }"; 8400) }

#[test]
fn bool_complement() { xc_test!("{ ; !true }"; false) }
#[test]
fn double_bool_complement() { xc_test!("{ ; !!true }"; true) }

#[test]
fn ret_if() { xc_test!("{ ? true { ; 100 } ; 20 }"; 100) }
#[test]
fn ret_if_no() { xc_test!("{ ? false { ; 100 } ; 20 }"; 20) }

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
fn ret_if_else() { xc_test!("{ ? true { ; 100 } : { ; 20 } }"; 100) }
#[test]
#[ignore]
fn ret_if_else_no() { xc_test!("{ ? false { ; 1 } : { ; 2 } }"; 2) }
#[test]
#[ignore]
fn expr_if_else() { xc_test!("{ ; ? true { 10 } : { 2 } }"; 10) }
#[test]
#[ignore]
fn expr_if_else_no() { xc_test!("{ ? false { 10 } : { 2 } }"; 20) }
