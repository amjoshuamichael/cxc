mod test_utils;

use test_utils::xc_test;

#[test]
fn basic_structural_type() {
    xc_test!(
        "
        ; i32 {
            x: { x: i32, y: i32 } = { x: i32, y: i32 } { x = 30, y = 90 }
            ; x.y - x.x
        }
        ";
        60
    )
}

#[test]
fn nested_structural_type() {
    // NOTE: for people reading this looking for ways to use features of the
    // language, this is a terrible example. please do not do this. this is
    // purely for testing purposes.
    xc_test!(
        "
        ; i32 {
            x: { x: { a: i32, b: i32 }, y: i32 } = 
                { x: { a: i32, b: i32 }, y: i32 } 
                    { x = { a: i32, b: i32 } { a = 90, b = 30 }, y = 90 }
            ; x.x.a - x.x.b + x.y
        }
        ";
        150
    )
}

#[test]
fn structural_type_method() {
    xc_test!(
        "
            &{ x: i32, y: i32 }:.add(); i32 {
                ; self.x + self.y
            }

            main() {
                x: { x: i32, y: i32 } = { x: i32, y: i32 } { x = 30, y = 90 }
                assert_eq<i32>(x.add(), 120)
            }
        "
    )
}

#[test]
fn nested_structural_type_method() {
    // NOTE: for people reading this looking for ways to use features of the
    // language, this is a terrible example. please do not do this. this is
    // purely for testing purposes.
    xc_test!(
        "
            &{ a: i32, b: i32 }:.add(); i32 {
                ; self.a + self.b
            }

            main() {
                x: { x: { a: i32, b: i32 }, y: i32 } = 
                    { x: { a: i32, b: i32 }, y: i32 } 
                        { 
                            x = { a: i32, b: i32 } { a = 90, b = 30 }, 
                            y = 90 
                        }

                assert_eq<i32>(x.x.add(), 120)
            }
        "
    )
}

#[test]
fn structural_type_static() {
    xc_test!(
        "
            { a: i32, b: i32 }::five(); i32 {
                ; 5
            }

            main() {
                x: i32 = { a: i32, b: i32 }:five()
                assert_eq<i32>(x, 5)
            }
        "
    )
}
