#![allow(dead_code)]
#![feature(type_name_of_val)]
#![feature(let_chains)]
#![feature(type_alias_impl_trait)]
#![feature(box_syntax)]
#![feature(yeet_expr)]

pub static DEBUG: bool = false;

pub use typ::{Kind, Type, TypeEnum};
pub use unit::{Func, LLVMContext, UniqueFuncInfo, Unit, Value};

pub mod library {
    pub use crate::libraries::{Library, StdLib, TestLib};
}

mod hlr;
mod lex;
mod libraries;
mod parse;
mod to_llvm;
mod typ;
mod unit;

#[cfg(test)]
mod tests {
    use crate::{
        libraries::{StdLib, TestLib, TypeInterfaceLib},
        unit::LLVMContext,
    };

    use super::*;

    #[test]
    fn basic_test() {
        let context = LLVMContext::new();
        let mut unit = unit::Unit::new(&context);

        unit.push_script(
            "
            factorial(of_num: i32): i32  {
                current: i32 = of_num
                output: i32 = 1

                @ current > 0 {
                    output = output * current
                    current = current - 1
                }

                ; output
            }
        ",
        );

        let output = unsafe { unit.get_fn_by_name::<i32, i32>("factorial")(5) };
        assert_eq!(output, 120);
    }

    #[test]
    fn pointer_test() {
        let context = LLVMContext::new();
        let mut unit = unit::Unit::new(&context);

        unit.push_script(
            "
                square(num: &i32): i32 {
                    num = *num * *num
                    ; 0
                }
                ",
        );

        let mut num = 4;
        unsafe { unit.get_fn_by_name::<&mut i32, i32>("square")(&mut num) };
        assert_eq!(num, 16);
    }

    #[test]
    fn multiple_args() {
        let context = LLVMContext::new();
        let mut unit = unit::Unit::new(&context);

        unit.push_script(
            "
                sum(a: i32, b: i32): i32 {
                    output: i32 = a + b
                    ; output
                }
                ",
        );

        let output: i32 = unsafe { unit.get_fn_by_name("sum")(4, 5) };
        assert_eq!(output, 9);
    }

    #[test]
    fn float_test() {
        let context = LLVMContext::new();
        let mut unit = unit::Unit::new(&context);

        unit.push_script(
            "
                seventy(): f32 {
                    output: f32 = 60.0 + 10.0
                    ; output
                }
                ",
        );

        let output: f32 = unsafe { unit.get_fn_by_name("seventy")(()) };
        assert_eq!(output, 70.0);
    }

    #[test]
    fn booleans() {
        let context = LLVMContext::new();
        let mut unit = unit::Unit::new(&context);
        unit.add_lib(TestLib);

        unit.push_script(
            "
                yeah(): bool {
                    output: bool = true
                    ; output
                }

                no(): bool {
                    output: bool = false
                    ; output
                }

                if_statements(): i32 {
                    test_bool: bool = false
                    basic_if_works: bool = true
                    not_operator_works: bool = false
                    set_bool_works: bool = false

                    ? test_bool {
                        basic_if_works = false
                    }

                    ? !test_bool {
                        not_operator_works = true
                    }

                    test_bool = true

                    ? test_bool {
                        set_bool_works = true
                    }

                    assert_eq<bool>(basic_if_works, true)
                    assert_eq<bool>(not_operator_works, true)
                    assert_eq<bool>(set_bool_works, true)

                    ; 0
                }
            ",
        );

        let yeah: bool = unsafe { unit.get_fn_by_name("yeah")(()) };
        assert_eq!(yeah, true);
        let no: bool = unsafe { unit.get_fn_by_name("no")(()) };
        assert_eq!(no, false);
        unsafe { unit.get_fn_by_name::<_, ()>("if_statements")(()) };
    }

    #[test]
    fn call_test() {
        let context = LLVMContext::new();
        let mut unit = unit::Unit::new(&context);
        unit.add_lib(TestLib);

        unit.push_script(
            "
                divide_by_two(num: f32): f32 {
                    output: f32 = num / 2.0
                    ; output
                }
    
                mul_by_two(num: i32): i32 {
                    output: i32 = num * 2
                    ; output
                }
    
                everything_works(): i32 {
                    six_times_two: i32 = mul_by_two(6)
                    assert_eq<i32>(six_times_two, 12)
    
                    six_div_two: f32 = divide_by_two(6.0)
                    assert_eq<f32>(six_div_two, 3.0)
    
                    ; 0
                }
            ",
        );

        unsafe { unit.get_fn_by_name::<(), i32>("everything_works")(()) };
    }

    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    struct Square {
        position: Point2D,
        size: i32,
    }

    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    struct Point2D {
        x: i32,
        y: i32,
    }

    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    struct GenPoint2D<T> {
        x: T,
        y: T,
    }

    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    struct Point3D {
        x: i32,
        y: i32,
        z: i32,
    }

    #[repr(C)]
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    struct Point3D64 {
        x: i64,
        y: i64,
        z: i64,
    }

    #[test]
    fn return_struct() {
        let context = LLVMContext::new();
        let mut unit = unit::Unit::new(&context);

        unit.push_script(
            "
                Point3D {
                    x: i32
                    y: i32
                    z: i32
                }
    
                main(): &Point3D {
                    new_point: Point3D = Point3D {
                        x = 30 * 2,
                        y = 52,
                        z = 99999,
                    }
    
                    ; &new_point
                }
                ",
        );

        let new_point_y: Point3D =
            unsafe { *unit.get_fn_by_name::<(), &mut Point3D>("main")(()) };

        assert_eq!(
            new_point_y,
            Point3D {
                x: 60,
                y: 52,
                z: 99999
            }
        );
    }

    #[test]
    fn struct_pointer() {
        let context = LLVMContext::new();
        let mut unit = unit::Unit::new(&context);

        unit.push_script(
            "
                Point2D {
                    x: i32
                    y: i32
                }
    
                sqr_magnitude_of(in_ptr: &Point2D): i32 {
                    in: Point2D = *in_ptr
    
                    ; in.x * in.x + in.y * in.y
                }
                ",
        );

        let point = Point2D { x: 2, y: 3 };

        let sqr_mag: i32 =
            unsafe { unit.get_fn_by_name("sqr_magnitude_of")(&point) };
        assert_eq!(sqr_mag, 13);
    }

    #[test]
    fn type_aliasing() {
        let context = LLVMContext::new();
        let mut unit = unit::Unit::new(&context);

        unit.push_script(
            "
                Square {
                    position: {
                        x: i32
                        y: i32
                    }
                    size: i32
                }
    
                Point2D {
                    x: i32
                    y: i32
                }
    
                make_square(): &{
                    position: {
                        x: i32
                        y: i32
                    }
                    size: i32
                } {
                    new_square: Square = Square {
                        position = Point2D {
                            x = 43,
                            y = 92,
                        },
                        size = 4,
                    }
    
                    ; &new_square
                }
                ",
        );

        let square = Square {
            position: Point2D { x: 43, y: 92 },
            size: 4,
        };

        let new_square: Square =
            unsafe { *unit.get_fn_by_name::<(), &Square>("make_square")(()) };
        assert_eq!(new_square, square);
    }

    #[test]
    fn generics() {
        let context = LLVMContext::new();
        let mut unit = unit::Unit::new(&context);

        unit.push_script(
            "
                Point2D<T> {
                    x: T
                    y: T
                }
    
                int_point(): &Point2D<i32> {
                    new_point: Point2D<i32> = Point2D<i32> {
                        x = 42, y = 32
                    }
    
                    ; &new_point
                }
    
                float_point(): &Point2D<f32> {
                    new_point: Point2D<f32> = Point2D<f32> {
                        x = 42.8, y = 32.2
                    }
    
                    ; &new_point
                }
            ",
        );

        let int_point =
            unsafe { *unit.get_fn_by_name::<(), &GenPoint2D<i32>>("int_point")(()) };
        assert_eq!(int_point.x, 42);
        assert_eq!(int_point.y, 32);

        let float_point = unsafe {
            *unit.get_fn_by_name::<(), &GenPoint2D<f32>>("float_point")(())
        };

        assert_eq!(float_point.x, 42.8);
        assert_eq!(float_point.y, 32.2);
    }

    #[test]
    fn external_function() {
        use crate::Type;

        pub fn print_num(input: i64) {
            println!("{input}");
        }

        let context = LLVMContext::new();
        let mut unit = unit::Unit::new(&context);

        unit.add_rust_func_explicit(
            "print_num",
            print_num as *const usize,
            Type::never().func_with_args(vec![Type::i(64)]),
            None,
            Vec::new(),
        )
        .push_script(
            "
                call(): i64 {
                    x: i64 = 0
                    @ x < 100 {
                        print_num(x)
                        x = x + 1
                    }
    
                    ; x
                }
            ",
        );

        unsafe { unit.get_fn_by_name::<(), i64>("call")(()) };
    }

    #[test]
    fn methods() {
        let context = LLVMContext::new();
        let mut unit = unit::Unit::new(&context);

        unit.add_lib(TestLib).push_script(
            "
                Point2D {
                    x: f32
                    y: f32
    
                    .hypotenuse(): f32 {
                        ; sqrt(self.x * self.x + self.y * self.y)
                    }
    
                    .scaled(by: f32): Point2D {
                        ; Point2D { x = self.x * by, y = self.y * by }
                    }
                }
    
                main(): i32 {
                    original: Point2D = Point2D { x = 4.0, y = 3.0 }
    
                    hypotenuse: f32 = original.hypotenuse()
                    assert_eq<f32>(hypotenuse, 5.0)
    
                    scaled_by_2: Point2D = original.scaled(1.5)
                    assert_eq<f32>(scaled_by_2.x, 6.0)
                    assert_eq<f32>(scaled_by_2.y, 4.5)
    
                    ; 0
                }
                ",
        );

        unsafe { unit.get_fn_by_name::<(), i32>("main")(()) };
    }

    #[test]
    fn arrays() {
        let context = LLVMContext::new();
        let mut unit = unit::Unit::new(&context);

        unit.add_lib(TestLib).push_script(
            "
                main(): i32 {
                    original: i32[7] = [1, 4, 8, 15, 16, 23, 42]
    
                    assert_eq<i32>(original[3], 15)
                    assert_eq<i32>(original[0], 1)
                    assert_eq<i32>(original[6], 42)
    
                    index: i32 = 0
                    @ index < 7 {
                        original[index] = index * 2
    
                        index = index + 1
                    }
    
                    assert_eq<i32>(original[0], 0)
                    assert_eq<i32>(original[1], 2)
                    assert_eq<i32>(original[3], 6)
                    assert_eq<i32>(original[6], 12)
    
                    ; 0
                }
                ",
        );

        unsafe { unit.get_fn_by_name::<(), i32>("main")(()) };
    }

    #[test]
    fn struct_arrays() {
        let context = LLVMContext::new();
        let mut unit = unit::Unit::new(&context);

        unit.add_lib(TestLib).push_script(
            "
                Point2D {
                    x: i32
                    y: i32
                }
    
                main(): i32 {
                    points: Point2D[3] = [
                        Point2D { x = 43, y = 15 },
                        Point2D { x = 327, y = 413 },
                        Point2D { x = 1672, y = 2526 },
                    ]
    
                    assert_eq<i32>(points[0].x, 43)
                    assert_eq<i32>(points[0].y, 15)
    
                    points[0].x = 94
    
                    assert_eq<i32>(points[0].x, 94)
                    assert_eq<i32>(points[0].y, 15)
    
                    points[1] = Point2D { x = 4, y = 6 }
    
                    assert_eq<i32>(points[1].x, 4)
                    assert_eq<i32>(points[1].y, 6)
    
                    ; 0
                }
                ",
        );

        unsafe { unit.get_fn_by_name::<(), i32>("main")(()) };
    }

    #[test]
    fn backwards_struct_dependency() {
        let context = LLVMContext::new();
        let mut unit = unit::Unit::new(&context);

        unit.add_lib(TestLib).push_script(
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
                ",
        );

        unsafe { unit.get_fn_by_name::<(), i32>("main")(()) };
    }

    #[test]
    fn backwards_call() {
        let context = LLVMContext::new();
        let mut unit = unit::Unit::new(&context);

        unit.add_lib(TestLib).push_script(
            "
                courthouse_1955(): i32 {
                    gigawatt_count: f32 = courthouse_1985()
                    assert_eq<f32>(gigawatt_count, 1.21) # great scott!
    
                    ; 1
                }
    
                courthouse_1985(): f32 {
                    ; 1.21
                }
                ",
        );

        unsafe { unit.get_fn_by_name::<(), i32>("courthouse_1955")(()) };
    }

    #[test]
    fn function_generics() {
        let context = LLVMContext::new();
        let mut unit = unit::Unit::new(&context);

        unit.add_lib(TestLib).push_script(
            "
                double<T>(in: T): T {
                    ; in + in
                }
    
                main(): i32 {
                    assert_eq<i32>(double<i32>(4), 8)
    
                    assert_eq<f32>(double<f32>(4.0), 8.0)
    
                    ; 0
                }
            ",
        );

        unsafe { unit.get_fn_by_name::<(), i32>("main")(()) };
    }

    #[test]
    fn generic_methods() {
        #[cfg(not(target_pointer_width = "64"))]
        panic!("this test does not work on non-64 bit architecture");

        let context = LLVMContext::new();
        let mut unit = unit::Unit::new(&context);

        unit.add_lib(TestLib).push_script(
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

                buy_las_vegas(): i32 {
                    after_this: Roll<f32> = Roll<f32> { val = 7.0 }
                    assert_eq<f32>(after_this.come_on<f32>(), 7_7_7.0) # let's go!
    
                    roll: Roll<i32> = Roll<i32> { val = 7 }
                    assert_eq<f32>(roll.come_on<i32>(), 7_7_7) # i like it, i like it!
    
                    ; 0
                }
                ",
        );

        unsafe { unit.get_fn_by_name::<(), i32>("buy_las_vegas")(()) };
    }

    #[test]
    fn std_lib() {
        let context = LLVMContext::new();
        let mut unit = unit::Unit::new(&context);
        unit.add_lib(TestLib);
        unit.add_lib(StdLib);

        unit.push_script(
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

test_vec(): i32 {
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
}",
        );

        unsafe { unit.get_fn_by_name::<(), i32>("test_vec")(()) };
    }

    #[test]
    fn derivations() {
        let context = LLVMContext::new();
        let mut unit = unit::Unit::new(&context);
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

    #[derive(Clone, Copy, Debug)]
    struct CXCVec<T> {
        data_loc: *const T,
        capacity: i64,
        len: isize,
    }

    #[test]
    fn strings() {
        let context = LLVMContext::new();
        let mut unit = unit::Unit::new(&context);

        unit.add_lib(StdLib);

        unit.push_script(
            r#"
            dude_thats_cray(output: &String): i32 {
                output = "that's cray"
                ; 0
            }
            "#,
        );

        let mut output = String::new();
        unsafe { unit.get_fn_by_name::<_, ()>("dude_thats_cray")(&mut output) };
        assert_eq!(output, "that's cray");
    }

    #[test]
    fn to_string() {
        let context = LLVMContext::new();
        let mut unit = unit::Unit::new(&context);

        unit.add_lib(StdLib);
        unit.add_lib(TestLib);

        unit.push_script(
            "
            Point2D<T> {
                x: T
                y: T
            }

            TwoPoint2Ds {
                one: Point2D<i32>
                two: Point2D<f32>
            }

            meaning(output: &String): i32 {
                some_points: TwoPoint2Ds = TwoPoint2Ds { 
                    one = Point2D<i32> {
                        x = 20,
                        y = 303,
                    }, 
                    two = Point2D<f32> {
                        x = 3.2,
                        y = 40.54,
                    }
                }
                
                output = some_points.to_string()

                ; 0
            }
        ",
        );

        let mut output = String::new();
        unsafe { unit.get_fn_by_name::<_, ()>("meaning")(&mut output) };
        assert_eq!(
            output,
            String::from("TwoPoint2Ds {one = Point2D {x = 20, y = 303}, two = Point2D {x = 3.2, y = 40.54}}")
        );
    }

    #[test]
    fn hello_world() {
        let context = LLVMContext::new();
        let mut unit = unit::Unit::new(&context);

        unit.add_lib(StdLib);
        unit.add_lib(TestLib);

        unit.push_script(
            r#"
            hello_world(): i32 {
                print<&String>(&"hello, world!")
                ; 0
            }
        "#,
        );

        unsafe { unit.get_fn_by_name::<(), ()>("hello_world")(()) };
    }
}
