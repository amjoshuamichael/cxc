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

#[test]
fn scopes() {
    xc_test!(
        use StdLib;
        "
        AddOnDrop = { ptr: &i32 } ~ { *self.ptr = *self.ptr + 1 }
        AddOnDrop::make(ptr: &i32); Me {
            ; cast<{ptr: &i32}, Me>({ptr = ptr})
        }

        main(); [4]i32 {
            counter := 0

            counter_checks := [++]

            a := AddOnDrop:make(&counter)

            ? true {
                b := AddOnDrop:make(&counter)

                counter_checks[0] = counter

                ? true {
                    c := AddOnDrop:make(&counter)
                    d := AddOnDrop:make(&counter)

                    counter_checks[1] = counter
                }

                counter_checks[2] = counter
            }

            counter_checks[3] = counter

            ; counter_checks
        }
        ";
        [0, 0, 2, 3]
    )
}

// Tests involving vecs, if broken, will create memory leaks on compiled backends.
// Technically, this will not make the tests fail.
// On the interpreter backend, the memory leaks will be detected, so these tests wlil fail.
#[test]
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
