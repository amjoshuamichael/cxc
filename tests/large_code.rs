mod test_utils;
use std::rc::Rc;

use cxc::library::{StdLib, TestLib, TypeInterfaceLib};
use cxc::{Kind, LLVMContext, Type, Unit};
use test_utils::{xc_test, TwoOf};

#[test]
fn backwards_struct_dependency() {
    xc_test!(
        "
        Julie = {
            letter: DreamFrom<TheEndOfTheWorld>
        }

        DreamFrom<T> = {
            location: T
        }

        TheEndOfTheWorld = {
            time: i32
        }

        main() {
            location: TheEndOfTheWorld =
                TheEndOfTheWorld { time = 2095 }

            dream: DreamFrom<TheEndOfTheWorld> =
                DreamFrom<TheEndOfTheWorld> {
                    location = location,
                }

            julie: Julie = Julie { letter = dream }

            assert_eq<i32>(julie.letter.location.time, 2095)
        }
        "
    )
}

#[test]
fn vec() {
    xc_test!(
        "     
        TwoNums = {
            num_one: i64,
            num_two: i64
        }

        two_nums_from_one(in: i64): TwoNums {
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
        use StdLib, TestLib;
        "
        main(): Rc<i32> {
            x: i32 = 90
            rcx: Rc<i32> = Rc<i32>:new(x)

            ; rcx
        }
        ";
        Rc::new(90i32)
    )
}

#[test]
fn generic_methods() {
    xc_test!(
        "
        Roll<T> = {
            val: T
        }

        <T> &Roll<T>:.come_on(): T {
            output: T = self.val

            counter: i32 = 1

            @ counter < 111 {
                output = output + self.val

                counter = counter + 1
            }

            ; output
        }

        # buy las vegas
        main() {
            after_this: Roll<f32> = Roll<f32> { val = 7.0 }
            assert_eq<f32>(after_this.come_on<f32>(), 7_7_7.0) # let's go!

            roll: Roll<i32> = Roll<i32> { val = 7 }
            assert_eq<f32>(roll.come_on<i32>(), 7_7_7) # i like it, i like it!
        }
        "
    )
}

#[test]
fn backwards_call() {
    xc_test!(
        "
        courthouse_1955(): i32 {
            gigawatt_count: f32 = courthouse_1985()
            assert_eq<f32>(gigawatt_count, 1.21) # great scott!

            ; 1
        }

        courthouse_1985(): f32 {
            ; 1.21
        }

        main(): f32 {
            ; courthouse_1985()
        }
        "
    )
}

#[test]
#[ignore]
fn hello_world() {
    xc_test!(
        r#"
        main() {
            print<&String>(&"hello, world!")
        }
        "#
    )
}

#[test]
fn derivations() {
    let context = LLVMContext::new();
    let mut unit = Unit::new(&context);
    unit.add_lib(TestLib).add_lib(TypeInterfaceLib).push_script(
        "
        Point2D = {
            x: i32,
            y: i32
        }

        Point3D = {
            x: i32,
            y: i32,
            z: i32
        }

        test_derivation() {
            point2D: Point2D = Point2D { x = 0, y = 0 }
            assert_eq<i32>(point2D.type_field_count(), 2)

            point3D: Point3D = Point3D { x = 0, y = 0, z = 0 }
            assert_eq<i32>(point3D.type_field_count(), 3)
        }
    ",
    );

    unsafe { unit.get_fn_by_name::<(), ()>("test_derivation")(()) };
}

#[test]
fn to_string() {
    xc_test!(
        use StdLib, TestLib;
        "
            OutputType = {
                ints: TwoOf<i32>,
                floats: TwoOf<f32>
            }

            main(): String {
                output: OutputType = OutputType { 
                    ints = TwoOf<i32> {
                        one = 20,
                        two = 303,
                    }, 
                    floats = TwoOf<f32> {
                        one = 3.2,
                        two = 40.54,
                    },
                }
                
                ; output.to_string()
            }
        ";
        String::from("OutputType {ints = TwoOf {one = 20, two = 303}, floats = TwoOf {one = 3.2, two = 40.54}}")
    )
}

#[derive(Default, Debug, PartialEq)]
struct OutputType {
    one: i32,
    two: f32,
}

#[test]
fn default() {
    xc_test!(
        use StdLib;
        "
            OutputType = {
                one: i32,
                two: f32
            }

            main(): OutputType {
                output: OutputType = OutputType:default()                

                ; output
            }
        ";
        OutputType::default()
    )
}

#[derive(Default, Debug, PartialEq)]
struct OutputType2 {
    one: TwoOf<i32>,
    two: TwoOf<f32>,
}

#[test]
fn default_generic() {
    xc_test!(
        use StdLib;
        "
            OutputType = {
                one: TwoOf<i32>,
                two: TwoOf<f32>
            }

            main(): OutputType {
                output: OutputType = OutputType:default()                

                ; output
            }
        ";
        OutputType2::default()
    )
}

#[test]
fn default_vec() {
    xc_test!(
        use StdLib;
        "
            main(): Vec<i32> {
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
            main(): Vec<i32> {
                output: Vec<i32> = Vec<i32>:default()                
                output.push<i32>(432)

                ; output
            }
        ";
        vec![432]
    )
}

#[test]
fn option() {
    xc_test!(
        use StdLib;
        "
            main(): Option<i32> {
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
            main(): Option<i32> {
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
            main(): Option<{i32, f32, i32}> {
                output: Option<{i32, f32, i32}> = 
                    Option<{i32, f32, i32}>.Some{ {i32, f32, i32} { 10, 20.0, 30 }}

                ; output
            }
        ";
        Option::<(i32, f32, i32)>::Some((10, 20.0, 30))
    )
}

// TODO: these rely on the way rust returns enums, which is not properly handled
// yet
//
//#[test]
// fn ref_option() {
//    xc_test!(
//        use StdLib;
//        "
//            main(): Option<&i32> {
//                output: Option<&i32> = Option<&i32>:default()
//
//                ; output
//            }
//        ";
//        Option::<&i32>::None
//    )
//}
//
//#[test]
// fn ref_option_and_more() {
//    xc_test!(
//        use StdLib;
//        "
//            main(): {Option<&i32>, i32} {
//                output: {Option<&i32>, i32} =
//                    { Option<&i32>, i32 } { Option<&i32>:default(), 32 }
//                ; output
//            }
//        ";
//        (Option::<&i32>::None, 32)
//    )
//}
//
//#[test]
// fn ref_and_more_option() {
//    let context: LLVMContext = cxc::LLVMContext::new();
//    let mut unit = Unit::new(&context);
//
//    unit.add_lib(StdLib);
//
//    unit.push_script(
//        "
//        main(pointer: &i32): Option<{&i32, i32}> {
//            output: Option<{&i32, i32}> =
//                Option<{&i32, i32}>.Some{ {&i32, i32} { (pointer), 10 }}
//            ; output
//        }
//        ",
//    );
//
//    let mut thirty_two = 32;
//    let func = unsafe { unit.get_fn_by_name::<&i32, Option<(&i32,
// i32)>>("main") };    let output: Option<(&i32, i32)> = unsafe { func(&mut
// thirty_two) };    assert_eq!(output, Option::Some((&32, 10)));
//    dbg!(&thirty_two);
//}

#[test]
fn push_string() {
    xc_test!(
        use StdLib, TestLib;
        r#"
        main(): String {
            x: String = "transformers was "
            x.push_string(&"directed by michael bay")
            ; x
        }
        "#;
        String::from("transformers was directed by michael bay")
    )
}
