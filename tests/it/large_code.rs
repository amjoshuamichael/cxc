use cxc::{library::StdLib, Unit, ExternalFuncAdd, Type};
use super::test_utils::{xc_test, TwoOf};

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
fn generic_methods() {
    xc_test!(
        "
        Roll<T> = {
            val: T
        }

        <T> &Roll<T>:.come_on(); T {
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
            assert_eq(after_this.come_on<f32>() == 7_7_7.0, true) # let's go!

            roll: Roll<i32> = Roll<i32> { val = 7 }
            assert_eq<i32>(roll.come_on<i32>(), 7_7_7) # i like it, i like it!
        }
        "
    )
}

#[test]
fn backwards_call() {
    xc_test!(
        "
        courthouse_1955(); i32 {
            gigawatt_count: f32 = courthouse_1985()
            assert_eq<f32>(gigawatt_count, 1.21) # great scott!

            ; 1
        }

        courthouse_1985(); f32 {
            ; 1.21
        }

        main(); f32 {
            ; courthouse_1985()
        }
        "
    )
}

#[test]
#[cfg(not(feature = "backend-interpreter"))]
fn hello_world() {
    xc_test!(
        r#"
        main() {
            print<String>("hello, world!")
        }
        "#
    )
}

#[test]
#[cfg(not(feature = "backend-interpreter"))]
fn to_string() {
    xc_test!(
        use StdLib;
        "
            OutputType = {
                ints: TwoOf<i32>,
                floats: TwoOf<f32>
            }

            main(); String {
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

#[derive(Clone, Copy, Default, Debug, PartialEq)]
struct OutputType {
    one: i32,
    two: f32,
}

#[test]
fn basic_default() {
    xc_test!(
        use StdLib;
        "
            OutputType = {
                one: i32,
                two: f32,
            }

            main(); OutputType {
                output: OutputType = OutputType:default()                

                ; output
            }
        ";
        OutputType::default()
    )
}

#[derive(Clone, Copy, Default, Debug, PartialEq)]
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

            main(); OutputType {
                output: OutputType = OutputType:default()                

                ; output
            }
        ";
        OutputType2::default()
    )
}

#[test]
#[cfg(not(feature = "backend-interpreter"))]
fn push_string() {
    xc_test!(
        use StdLib;
        r#"
        main(); String {
            x: String = "transformers was "
            x.push_string(&"directed by michael bay")
            ; x
        }
        "#;
        String::from("transformers was directed by michael bay")
    )
}

#[test]
fn extern_and_local() {
    let mut unit = Unit::new();

    fn twenty_nine() -> i32 { 29 }

    unit.push_script("fourty_three(); i32 { ; 43 }").unwrap();
    unit.add_rust_func_explicit(
        "twenty_nine", 
        twenty_nine as *const usize,
        ExternalFuncAdd {
            ret_type: Type::i(32),
            ..ExternalFuncAdd::empty()
        },
    );
    unit.push_script("sum(); i32 { ; fourty_three() + twenty_nine() }").unwrap();
    unit.get_fn("fourty_three").unwrap().downcast::<(), i32>()();
}

#[test]
#[cfg(not(feature = "backend-interpreter"))]
fn combine_string_array() {
    xc_test!(
        use StdLib;
        r#"
            main(); String {
                message := ["H", "E", "L", "L", "O"]
                new_message := ""
                
                m := 0
                @ m < message.len {
                    new_message.push_string(&message[m])
                    m = m + 1
                }
                
                ; new_message
            }
        "#;
        String::from("HELLO")
    );
}

#[test]
#[cfg(not(feature = "backend-interpreter"))]
fn prime_sieve() {
    xc_test!(
        use StdLib;
        r#"
        main(); Vec<u64> {
            size: u64 = 40
            sieve := Vec<bool>:init_with(size, false)

            i := 2
            @ i < size {
                j := i + 1

                @ j < size {
                    ? j % i == 0 {
                        *sieve.get_ref(j) = true
                    }

                    j = j + 1
                }

                i = i + 1
            }

            final := Vec<u64>:new()

            l := 0
            @ l < size {
                is_prime := sieve.get(l)
                ? !is_prime {
                    final.push(l)
                }

                l = l + 1
            }

            ; final
        }
        "#;
        vec![0i64, 1, 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37]
    );
}
