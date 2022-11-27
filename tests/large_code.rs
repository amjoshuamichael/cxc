mod test_utils;
use cxc::library::{StdLib, TestLib, TypeInterfaceLib};
use cxc::{LLVMContext, Unit};
use test_utils::xc_test;

#[test]
fn backwards_struct_dependency() {
    xc_test!(
        "
        Julie {
            letter: DreamFrom<TheEndOfTheWorld>
        }

        DreamFrom<T> {
            location: T
        }

        TheEndOfTheWorld {
            time: i32
        }

        main(): i32 {
            location: TheEndOfTheWorld =
                TheEndOfTheWorld { time = 2095 }

            dream: DreamFrom<TheEndOfTheWorld> =
                DreamFrom<TheEndOfTheWorld> {
                    location = location,
                }

            julie: Julie = Julie { letter = dream }

            assert_eq<i32>(julie.letter.location.time, 2095)

            ; 0
        }
        "
    )
}

#[test]
fn vec() {
    xc_test!(
        "     
        TwoNums {
            num_one: i64
            num_two: i64
        }

        two_nums_from_one(in: i64): TwoNums {
            two_nums: TwoNums = 0
            two_nums.num_one = in
            two_nums.num_two = in + 42

            ; two_nums
        }

        main(): i32 {
            number_of_checks: i32 = 128

            num_vec: Vec<i32> = create_vec<i32>()
            two_num_vec: Vec<TwoNums> = create_vec<TwoNums>(two_nums_from_one(to_i64(0)))

            current_index: i32 = 0
            @ current_index < number_of_checks + 0 {
                x: i32 = num_vec.push<i32>(current_index)

                current_index = current_index + 1
            }

            current_index = 0
            @ current_index < number_of_checks + 0 {
                assert_eq<i32>(num_vec.get<i32>(to_i64(current_index)), current_index)

                current_index = current_index + 1
            }


            current_index: i64 = 0
            @ current_index < number_of_checks + 0 {
                x: i32 = two_num_vec.push<TwoNums>(two_nums_from_one(current_index))

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


            ; 0
        }"
    )
}

#[test]
fn generic_methods() {
    xc_test!(
        "
        Roll<T> {
            val: T

            .come_on(): T {
                output: T = self.val

                counter: i32 = 1

                @ counter < 111 {
                    output = output + self.val

                    counter = counter + 1
                }

                ; output
            }
        }

        # buy las vegas
        main(): i32 {
            after_this: Roll<f32> = Roll<f32> { val = 7.0 }
            assert_eq<f32>(after_this.come_on<f32>(), 7_7_7.0) # let's go!

            roll: Roll<i32> = Roll<i32> { val = 7 }
            assert_eq<f32>(roll.come_on<i32>(), 7_7_7) # i like it, i like it!

            ; 0
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
            hello_world(): i32 {
                print<&String>(&"hello, world!")
                ; 0
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
        Point2D {
            x: i32
            y: i32
        }

        Point3D {
            x: i32
            y: i32
            z: i32
        }

        test_derivation(): i32 {
            point2D: Point2D = Point2D { x = 0, y = 0 }
            assert_eq<i32>(point2D.type_field_count(), 2)

            point3D: Point3D = Point3D { x = 0, y = 0, z = 0 }
            assert_eq<i32>(point3D.type_field_count(), 3)

            ; 0
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
            OutputType {
                one: TwoOf<i32>
                two: TwoOf<f32>
            }

            main(): String {
                output: OutputType = OutputType { 
                    one = TwoOf<i32> {
                        one = 20,
                        two = 303,
                    }, 
                    two = TwoOf<f32> {
                        one = 3.2,
                        two = 40.54,
                    },
                }
                
                ; output.to_string()
            }
        ";
        String::from("OutputType {one = TwoOf {one = 20, two = 303}, two = TwoOf {one = 3.2, two = 40.54}}")
    )
}
