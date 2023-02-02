mod test_utils;
use std::rc::Rc;

use cxc::library::StdLib;
use test_utils::xc_test;

// VEC
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
                output.push<i32>(432)

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
            two_nums: TwoNums = 0
            two_nums.num_one = in
            two_nums.num_two = in + 42

            ; two_nums
        }

        main() {
            number_of_checks: i32 = 128

            num_vec: Vec<i32> = Vec<i32>:new()
            two_num_vec: Vec<TwoNums> = Vec<TwoNums>:new()

            current_index: i32 = 0
            @ current_index < number_of_checks + 0 {
                num_vec.push<i32>(current_index)

                current_index = current_index + 1
            }

            current_index = 0
            @ current_index < number_of_checks + 0 {
                assert_eq<i32>(num_vec.get<i32>(to_i64(current_index)), current_index)

                current_index = current_index + 1
            }


            current_index: i64 = 0
            @ current_index < number_of_checks + 0 {
                two_num_vec.push<TwoNums>(two_nums_from_one(current_index))

                current_index = current_index + 1
            }

            current_index = 0
            @ current_index < number_of_checks + 0 {
                in_vec: TwoNums = two_num_vec.get<TwoNums>(current_index)
                corresponding: TwoNums = two_nums_from_one(current_index)

                assert_eq<i64>(in_vec.num_one, corresponding.num_one)
                assert_eq<i64>(in_vec.num_two, corresponding.num_two)

                current_index = current_index + 1
            }
        }"
    )
}

#[test]
fn rc() {
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
fn option() {
    xc_test!(
        use StdLib;
        "
            main(); Option<i32> {
                output: Option<i32> = Option<i32>.Some { 10 }

                ; output
            }
        ";
        Some(10)
    )
}

#[test]
fn option_default() {
    xc_test!(
        use StdLib;
        "
            main(); Option<i32> {
                output: Option<i32> = Option<i32>:default()

                ; output
            }
        ";
        Option::<i32>::None
    )
}

#[test]
fn bigger_option() {
    xc_test!(
        use StdLib;
        "
            main(); Option<{i32, f32, i32}> {
                output: Option<{i32, f32, i32}> = 
                    Option<{i32, f32, i32}>.Some{ {i32, f32, i32} { 10, 20.0, 30 }}

                ; output
            }
        ";
        Option::<(i32, f32, i32)>::Some((10, 20.0, 30))
    )
}

#[test]
fn bit_array() {
    xc_test!(
        "
        main() {
            arr8 = BitArray<[32]bool>:new()
            arr8.set(4, true)
            assert_eq(arr8.get(4), true)
            assert_eq(arr8.get(5), false)
        }
        "
    )
}
