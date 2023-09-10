mod test_utils;
use std::{rc::Rc, sync::Arc};

use cxc::library::StdLib;
use test_utils::xc_test;

#[test]
fn default_vec_alone() {
    xc_test!(
        use StdLib;
        "
            main(); Vec<i32> {
                output: Vec<i32> = Vec<i32>:default()                

                ; output
            }
        ";
        Vec::<i32>::new()
    )
}

#[test]
fn default_vec_with_push() {
    xc_test!(
        use StdLib;
        "
            main(); Vec<i32> {
                output: Vec<i32> = Vec<i32>:default()                
                output.push(432)

                ; output
            }
        ";
        vec![432]
    )
}

#[test]
fn vec_push_and_check() {
    xc_test!(
        "     
        TwoNums = {
            num_one: i64,
            num_two: i64
        }

        two_nums_from_one(in: i64); TwoNums {
            two_nums: TwoNums = TwoNums { 
                num_one = in,
                num_two = in + i64 42,
            }

            ; two_nums
        }

        main() {
            number_of_checks: i64 = i64 100

            num_vec: Vec<i64> = Vec<i64>:new()
            two_num_vec: Vec<TwoNums> = Vec<TwoNums>:new()

            current_index: i64 = i64 0
            @ current_index < number_of_checks {
                num_vec.push(current_index)

                current_index = current_index + i64 1
            }

            current_index = i64 0
            @ current_index < number_of_checks {
                assert_eq<i64>(num_vec.get<i64>(current_index), current_index)

                current_index = current_index + i64 1
            }

            current_index = i64 0
            @ current_index < number_of_checks {
                two_num_vec.push(two_nums_from_one(current_index))

                current_index = current_index + i64 1
            }

            current_index = i64 0
            @ current_index < i64 1 {
                in_vec: TwoNums = two_num_vec.get<TwoNums>(current_index)
                corresponding: TwoNums = two_nums_from_one(current_index)

                assert_eq<i64>(in_vec.num_two, corresponding.num_two)

                current_index = current_index + i64 1
            }
        }"
    )
}

#[test]
fn rc_basic() {
    xc_test!(
        use StdLib;
        "
        main(); Rc<i32> {
            x: i32 = 90
            rcx: Rc<i32> = Rc<i32>:new(x)

            ; rcx
        }
        ";
        Rc::new(90i32)
    )
}

#[test]
fn big_rc() {
    xc_test!(
        use StdLib;
        "
        main(); Rc<{i32, i32}> {
            rcx: Rc<{i32, i32}> = Rc<{i32, i32}>:new({ 90, 90 })

            ; rcx
        }
        ";
        Rc::new((90i32, 90i32))
    )
}

#[test]
fn big_rc_to_string() {
    xc_test!(
        use StdLib;
        "
        main(); String {
            rcx: Rc<{i32, i32}> = Rc<{i32, i32}>:new({ 90, 90 })

            ; rcx.to_string()
        }
        ";
        String::from("{0 = 90, 1 = 90}")
    )
}

#[test]
fn bit_array_16() {
    xc_test!(
        "
        main() {
            arr8 = BitArray<i16>:new()

            arr8.toggle(u16 0)
            arr8.toggle(u16 4)
            arr8.toggle(u16 15)

            assert_eq(arr8.get(u16 0), true)
            assert_eq(arr8.get(u16 1), false)
            assert_eq(arr8.get(u16 3), false)
            assert_eq(arr8.get(u16 4), true)
            assert_eq(arr8.get(u16 5), false)
            assert_eq(arr8.get(u16 14), false)
            assert_eq(arr8.get(u16 15), true)
        }
        "
    )
}

#[test]
fn bit_array_32() {
    xc_test!(
        "
        main() {
            arr8 = BitArray<i32>:new()

            arr8.toggle(u32 0)
            arr8.toggle(u32 4)
            arr8.toggle(u32 31)

            assert_eq(arr8.get(u32 0), true)
            assert_eq(arr8.get(u32 1), false)
            assert_eq(arr8.get(u32 3), false)
            assert_eq(arr8.get(u32 4), true)
            assert_eq(arr8.get(u32 5), false)
            assert_eq(arr8.get(u32 30), false)
            assert_eq(arr8.get(u32 31), true)
        }
        "
    )
}

#[test]
fn bit_array_8() {
    xc_test!(
        "
        main() {
            arr8 = BitArray<i8>:new()

            arr8.toggle(u8 0)
            arr8.toggle(u8 4)
            arr8.toggle(u8 7)
            arr8.toggle(u8 7)
            arr8.toggle(u8 7)

            assert_eq(arr8.get(u8 0), true)
            assert_eq(arr8.get(u8 1), false)
            assert_eq(arr8.get(u8 3), false)
            assert_eq(arr8.get(u8 4), true)
            assert_eq(arr8.get(u8 5), false)
            assert_eq(arr8.get(u8 6), false)
            assert_eq(arr8.get(u8 7), true)
        }
        "
    )
}

#[test]
fn cast() {
    xc_test!(
        use StdLib;
        "
        main(); i64 {
            ; cast<{i32, i32}, i64>( { 32, 32 } )
        }
        ";
        unsafe { std::mem::transmute::<(i32, i32), i64>((32, 32)) }
    )
}

#[test]
fn arc_test() {
    xc_test!(
        use StdLib;
        "
        main(); Arc<i32> {
            x: i32 = 90
            arc: Arc<i32> = Arc<i32>:new(x)

            ; arc
        }
        ";
        Arc::new(90i32)
    )
}

// TODO: naming a variable with the name of a method causes bugs??
#[test]
#[ignore]
fn arc_clone() {
    xc_test!(
        use StdLib;
        "
        main(); Arc<i32> {
            x: i32 = 90
            arc: Arc<i32> = Arc<i32>:new(x)
            cloned: Arc<i32> = arc.clone()

            ; cloned
        }
        ";
        Arc::new(90i32)
    )
}

// TODO: backtraces don't work properly
#[test]
#[ignore]
#[should_panic(expected = "100 is not equal to 200!")]
fn int_failed_assert() {
    xc_test!(
        "
        main() {
            assert_eq(100, 200)
        }
        "
    )
}

#[test]
#[ignore]
#[should_panic(expected = "true is not equal to false!")]
fn bool_failed_assert() {
    xc_test!(
        "
        main() {
            assert_eq(true, false)
        }
        "
    )
}

#[test]
#[ignore]
#[should_panic(expected = "failure!")]
fn panic() {
    xc_test!(
        r#"
        main() {
            panic()
        }
        "#
    )
}
