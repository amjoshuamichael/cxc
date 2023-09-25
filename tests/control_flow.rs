mod test_utils;

use test_utils::xc_test;

#[test]
fn ret_if_yes() {
    xc_test!(
        "; i32 { 
            ? true { ; 100 } 
            ; 20 
        }"; 
        100
    )
}

#[test]
fn ret_if_no() {
    xc_test!(
        "; i32 { 
            ? false { ; 100 } 
            ; 20 
        }"; 
        20
    )
}

#[test]
fn ret_if_else() {
    xc_test!(
        "; i32 {
            ? true { ; 100 } : { ; 20 } 
        }"; 
        100
    ) 
}

#[test]
fn ret_if_else_no() {
    xc_test!(
        "; i32 { 
            ? false { ; 1 } : { ; 2 } 
        }"; 
        2
    ) 
}

#[test]
fn nested_if() {
    xc_test!(
        "; i32 {
            x := 90

            ? x > 40 {
                ? x > 100 {
                    ; 10
                } : {
                    ; 20
                }
            } : {
                ; 30
            }
        }";
        20
    );
}

#[test]
fn nested_while() {
    xc_test!(
        "; i32 {
            sum := 0
            x := 0
            y := 0

            @ x < 7 {
                x = x + 1

                y = 0
                @ y < 31 {
                    y = y + 1

                    sum = sum + 1
                }
            }

            ; sum
        }";
        217
    );
}

#[test]
fn scope() {
    xc_test!(
        "; i32 {
            x := 10

            ? true {
                x = 20
            }

            ? true {
                x := 40
                x = x + 90
            }

            ? true {
                x = x + 10
            }

            ; x
        }";
        30
    )
}

#[test]
fn while_continue() {
    xc_test!(
        "; i32 {
            x := 0
            sum := 0

            @ x < 30 {
                x = x + 1

                ? x % 2 == 0 {
                    ;:continue
                }

                sum = sum + x
            }

            ; x
        }";
        30
    );
}

#[test]
fn while_break() {
    xc_test!(
        "; i32 {
            x := 0

            @ x < 50 {
                ? x >= 30 {
                    ;:break
                }
                
                x = x + 1
            }

            ; x
        }";
        30
    );
}
