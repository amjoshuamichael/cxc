mod test_utils;

use cxc::library::StdLib;
use test_utils::xc_test;

#[test]
fn basic_drop() {
    xc_test!(
        use StdLib;
        "
        main(); i64 {
            original := Rc<i32>:new(30)
            create_clone(&original)
            strong_count := original.strong
            ; strong_count
        }

        create_clone(rc: &Rc<i32>) {
            cloned := rc.clone()
        }
        ";
        1i64
    )
}

#[test]
fn drop_no_var() {
    xc_test!(
        use StdLib;
        "
        main(); i64 {
            original := Rc<i32>:new(30)
            create_clone(&original)
            strong_count := original.strong
            ; strong_count
        }

        create_clone(rc: &Rc<i32>) {
            rc.clone()
        }
        ";
        1i64
    )
}

#[test]
fn pass_around() {
    xc_test!(
        use StdLib;
        "
        main(); i64 {
            new := create_one()
            strong_count := new.strong
            ; strong_count
        }

        create_one(); Rc<i32> {
            new := Rc<i32>:new(30)
            create_clone(&new)
            ; new
        }

        create_clone(rc: &Rc<i32>) {
            rc.clone()
        }
        ";
        1i64
    )
}

#[test]
fn return_with_member() {
    xc_test!(
        use StdLib;
        "
        main(); i64 {
            new := create_one()
            strong_count := new.strong
            ; strong_count
        }

        create_one(); Rc<i32> {
            new := { Rc<i32>:new(30), 30 }
            create_clone(&new.0)
            ; new.0
        }

        create_clone(rc: &Rc<i32>) {
            rc.clone()
        }
        ";
        1i64
    )
}

// Tests involving vecs, if broken, will create memory leaks on compiled backends.
// Technically, this will not make the tests fail on these backends. On the interpreter 
// backend, however, the memory leaks will be detected, and the tests will fail.
#[test]
#[cfg(feature = "backend-interpreter")]
fn vec_basic() {
    xc_test!(
        use StdLib;
        "
        main() {
            new_vec := Vec<i32>:new()
            new_vec.push(4880)
        }
        "
    )
}

#[test]
#[cfg(feature = "backend-interpreter")]
fn vec_replace() {
    xc_test!(
        use StdLib;
        "
        main() {
            new_vec := Vec<i32>:new()
            new_vec.push(4880)
            new_vec = Vec<i32>:new()
            new_vec.push(2439)
        }
        "
    )
}

#[test]
#[cfg(feature = "backend-interpreter")]
fn empty_vec() {
    xc_test!(
        use StdLib;
        "
        main() {
            new_vec := Vec<i32>:new()
        }
        "
    )
}

#[test]
#[cfg(feature = "backend-interpreter")]
fn if_then_return() {
    xc_test!(
        use StdLib;
        "
        main() {
            flow(true)
            flow(false)
        }

        flow(if: bool) {
            x := Vec<i32>:new()
            x.push(230)

            ? if {
                ;
            }

            ;
        }
        "
    )
}

#[test]
#[cfg(feature = "backend-interpreter")]
fn if_then_no_return() {
    xc_test!(
        use StdLib;
        "
        main() {
            flow(true)
            flow(false)
        }

        flow(if: bool) {
            x := Vec<i32>:new()
            x.push(230)

            ? if { }

            ;
        }
        "
    )
}

#[test]
#[cfg(feature = "backend-interpreter")]
fn if_then_throw() {
    xc_test!(
        use StdLib;
        "
        main() {
            flow(true)
            flow(false)
        }

        flow(if: bool) {
            x := Vec<i32>:new()

            ? if {
                throw_away(x)
            }
        }

        throw_away(vec: Vec<i32>) { }
        "
    )
}

#[test]
#[cfg(feature = "backend-interpreter")]
fn drop_while() {
    xc_test!(
        use StdLib;
        "
        main() {
            x := Vec<i32>:new()
            
            sum := 0

            @ sum < 4 {
                x.push(30)
                sum = sum + 1
            }
        }
        "
    )
}

#[test]
#[cfg(feature = "backend-interpreter")]
fn drop_nested_while() {
    xc_test!(
        use StdLib;
        "
        main(); i32 {
            x := Vec<i32>:new()
            x.push(0)
            x.push(0)
            x.push(0)
            y := Vec<i32>:new()
            
            sum := 0

            for x {
                for y { sum = sum + 1 }

                y.push(23)
            }

            ; sum
        }
        ";
        3
    )
}

#[test]
#[cfg(feature = "backend-interpreter")]
fn conditional_initialization_1way() {
    xc_test!(
        use StdLib;
        "
        main() {
            init(true)
            init(false)
        }

        init(if: bool) {
            ? if {
                x := Rc<i32>:new(23)
            }
        }
        "
    )
}

#[test]
#[cfg(feature = "backend-interpreter")]
fn conditional_initialization_2way() {
    xc_test!(
        use StdLib;
        "
        main() {
            init(true)
            init(false)
        }

        init(if: bool) {
            ? if {
                x := Rc<i32>:new(23)
            } : {
                x := Rc<i32>:new(90)
            }
        }
        "
    )
}

#[test]
#[cfg(feature = "backend-interpreter")]
fn conditional_set() {
    xc_test!(
        use StdLib;
        "
        main() {
            init(true)
            init(false)
        }

        init(if: bool) {
            x := { -- }

            ? if {
                x = Rc<i32>:new(23)
            } : {
                x = Rc<i32>:new(90)
            }
        }
        "
    )
}
