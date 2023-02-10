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
            two_nums: TwoNums = TwoNums { 
                num_one = in,
                num_two = in + 42,
            }

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
fn option_rc() {
    xc_test!(
        use StdLib;
        "
            main(); Option< Rc<i32> > {
                x: i32 = 90
                rc: Rc<i32> = Rc<i32>:new(x)
                output: Option< Rc<u32> > = Option< Rc<i32> >.Some rc

                ; output
            }
        ";
        Some(Rc::new(90))
    )
}

#[test]
fn option_vec() {
    xc_test!(
        use StdLib;
        "
            main(); Option< Vec<i32> > {
                x: i32 = 90
                vec: Vec<i32> = Vec<i32>:new()
                vec.push(x)
                output: Option< Vec<u32> > = Option< Rc<i32> >.Some vec

                ; output
            }
        ";
        Some(vec![90])
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
fn bit_array_16() {
    xc_test!(
        "
        main() {
            arr8 = BitArray<[16]bool>:new()

            arr8.toggle(0)
            arr8.toggle(4)
            arr8.toggle(15)

            assert_eq(arr8.get(0), true)
            assert_eq(arr8.get(1), false)
            assert_eq(arr8.get(3), false)
            assert_eq(arr8.get(4), true)
            assert_eq(arr8.get(5), false)
            assert_eq(arr8.get(14), false)
            assert_eq(arr8.get(15), true)
        }
        "
    )
}

#[test]
fn bit_array_32() {
    xc_test!(
        "
        main() {
            arr8 = BitArray<[32]bool>:new()

            arr8.toggle(0)
            arr8.toggle(4)
            arr8.toggle(31)

            assert_eq(arr8.get(0), true)
            assert_eq(arr8.get(1), false)
            assert_eq(arr8.get(3), false)
            assert_eq(arr8.get(4), true)
            assert_eq(arr8.get(5), false)
            assert_eq(arr8.get(30), false)
            assert_eq(arr8.get(31), true)
        }
        "
    )
}

#[test]
fn bit_array_8() {
    xc_test!(
        "
        main() {
            arr8 = BitArray<[8]bool>:new()

            arr8.toggle(0)
            arr8.toggle(4)
            arr8.toggle(7)
            arr8.toggle(7)
            arr8.toggle(7)

            assert_eq(arr8.get(0), true)
            assert_eq(arr8.get(1), false)
            assert_eq(arr8.get(3), false)
            assert_eq(arr8.get(4), true)
            assert_eq(arr8.get(5), false)
            assert_eq(arr8.get(6), false)
            assert_eq(arr8.get(7), true)
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

//#[test]
// fn option_is_some_is_none() {
//    xc_test!(
//        "
//        main() {
//            some: Option<i32> = Option<i32>.Some { 10 }
//            assert_eq(some.is_some(), true)
//            assert_eq(some.is_none(), false)
//
//            none: Option<i32> = Option<i32>.None { }
//            assert_eq(none.is_none(), true)
//            assert_eq(none.is_some(), false)
//        }
//        "
//    )
//}
