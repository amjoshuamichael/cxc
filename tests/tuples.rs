mod test_utils;

use test_utils::xc_test;

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

#[test]
fn named_basic_tuple() {
    xc_test!(
        "
            TwoNumsTuple = { i32, i32 }

            main() {
                x: TwoNumsTuple = TwoNumsTuple { 90, 43 }
                assert_eq<i32>(x.0 - x.1, 47)
            }
        "
    )
}

#[test]
fn named_nested_tuple() {
    xc_test!(
        "
            Inner = { i32, i32 }
            Nested = { Inner, i32 }

            main() {
                x: Nested = Nested { Inner { 90, 60 }, 43 }
                assert_eq<i32>( x.0.0 - x.0.1 + x.1, 73 )
            }
        "
    )
}

#[test]
fn named_tuple_method() {
    xc_test!(
        "
            Inner = { i32, i32 }

            &Inner.add(): i32 {
                ; self.0 + self.1
            }

            main() {
                x: Inner = Inner { 30, 90 }
                assert_eq<i32>(x.add(), 120)
            }
        "
    )
}

#[test]
fn named_nested_structural_type_method() {
    xc_test!(
        "
            Inner = { i32, i32 }

            &Inner.add(): i32 {
                ; self.0 + self.1
            }

            main() {
                x: { Inner, i32 } = 
                    { Inner, i32 } { Inner { 90, 30 }, 90 }

                assert_eq<i32>(x.0.add(), 120)
            }
        "
    )
}
