#![allow(dead_code)]
#![feature(let_else)]
#![feature(let_chains)]
#![feature(once_cell)]
#![feature(type_alias_impl_trait)]
#![feature(box_syntax)]

pub static DEBUG: bool = true;

mod hlr;
mod lex;
mod parse;
mod to_llvm;
mod unit;

mod indent_parens;

#[cfg(test)]
mod tests {
    use inkwell::context::Context;

    use super::*;

    #[test]
    fn basic_test() {
        let context = Context::create();
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

                ! output
            }
        ",
        );

        let output = unsafe { unit.get_fn::<i32, i32>("factorial")(5) };
        assert_eq!(output, 120);
    }

    #[test]
    fn pointer_test() {
        let context = Context::create();
        let mut unit = unit::Unit::new(&context);

        unit.push_script(
            "
            square(num: &i32): i32 {
                num = *num * *num
                ! 0
            }
            ",
        );

        let mut num = 4;
        unsafe { unit.get_fn::<&mut i32, i32>("square")(&mut num) };
        assert_eq!(num, 16);
    }

    #[test]
    fn multiple_args() {
        let context = Context::create();
        let mut unit = unit::Unit::new(&context);

        unit.push_script(
            "
            sum(a: i32, b: i32): i32 {
                output: i32 = a + b
                ! output
            }
            ",
        );

        let output: i32 = unsafe { unit.get_fn("sum")(4, 5) };
        assert_eq!(output, 9);
    }

    #[test]
    fn float_test() {
        let context = Context::create();
        let mut unit = unit::Unit::new(&context);

        unit.push_script(
            "
            seventy(): f32 {
                output: f32 = 60.0 + 10.0
                ! output
            }
            ",
        );

        let output: f32 = unsafe { unit.get_fn("seventy")(()) };
        assert_eq!(output, 70.0);
    }

    #[test]
    fn call_test() {
        let context = Context::create();
        let mut unit = unit::Unit::new(&context);
        unit.add_test_lib();

        unit.push_script(
            "
            divide_by_two(num: f32): f32 {
                output: f32 = num / 2.0
                ! output
            }

            mul_by_two(num: i32): i32 {
                output: i32 = num * 2
                ! output
            }
    
            everything_works(): i32 {
                six_times_two: i32 = mul_by_two(6)
                assert_eq(six_times_two, 12) 

                six_div_two: f32 = divide_by_two(6.0)
                assert_eq(six_div_two, 3.0)

                ! 0
            }
            ",
        );

        unsafe { unit.get_fn::<(), i32>("everything_works")(()) };
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

    #[test]
    fn return_struct() {
        let context = Context::create();
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
                
                ! &new_point
            }
            ",
        );

        let new_point_y: Point3D =
            unsafe { *unit.get_fn::<(), &mut Point3D>("main")(()) };
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
        let context = Context::create();
        let mut unit = unit::Unit::new(&context);

        unit.push_script(
            "
            Point2D {
                x: i32
                y: i32
            }

            sqr_magnitude_of(in_ptr: &Point2D): i32 {
                in: Point2D = *in_ptr

                ! in.x * in.x + in.y * in.y
            }
            ",
        );

        let point = Point2D { x: 2, y: 3 };

        let sqr_mag: i32 = unsafe { unit.get_fn("sqr_magnitude_of")(&point) };
        assert_eq!(sqr_mag, 13);
    }

    #[test]
    fn type_aliasing() {
        let context = Context::create();
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

                ! &new_square
            }
            ",
        );

        let square = Square {
            position: Point2D { x: 43, y: 92 },
            size: 4,
        };

        let new_square: Square =
            unsafe { *unit.get_fn::<(), &Square>("make_square")(()) };
        assert_eq!(new_square, square);
    }

    #[test]
    fn generics() {
        let context = Context::create();
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

                ! &new_point
            }

            float_point(): &Point2D<f32> {
                new_point: Point2D<f32> = Point2D<f32> {
                    x = 42.8, y = 32.2
                }

                ! &new_point
            }
        ",
        );

        let int_point =
            unsafe { *unit.get_fn::<(), &GenPoint2D<i32>>("int_point")(()) };
        assert_eq!(int_point.x, 42);
        assert_eq!(int_point.y, 32);

        let float_point =
            unsafe { *unit.get_fn::<(), &GenPoint2D<f32>>("float_point")(()) };

        assert_eq!(float_point.x, 42.8);
        assert_eq!(float_point.y, 32.2);
    }

    #[test]
    fn external_function() {
        use crate::hlr::prelude::Type;

        pub fn print_num(input: i64) {
            println!("{input}");
        }

        let context = Context::create();
        let mut unit = unit::Unit::new(&context);

        unit.add_external_function(
            "print_num",
            print_num as *const usize,
            vec![Type::int_of_size(64)],
            Type::never(),
        );

        unit.push_script(
            "
            call(): i64 {
                x: i64 = 0
                @ x < 100 {
                    print_num(x)
                    x = x + 1
                }

                ! x
            }
        ",
        );

        unsafe { unit.get_fn::<(), i64>("call")(()) };
    }

    #[test]
    fn function_overload() {
        let context = Context::create();
        let mut unit = unit::Unit::new(&context);

        unit.push_script(
            "
            # method overload 1
            confusion() : i32 {
                ! 40
            }

            # method overload 2
            confusion(in: i32) : f32 {
                ? in == 40 {
                    ! 34.9
                }

                ! 43.3
            }

            # method overload 3
            confusion(in: f32) : i32 {
                ? in == 34.9 {
                    ! 42
                }

                ! 6
            }

            shine_a_little_love(): i32 {
                # should pass output of 1 into 2, 
                # and then ouput of 2 into 3
                ! confusion(confusion(confusion()))
            }
            ",
        );

        let output = unsafe { unit.get_fn::<(), i32>("shine_a_little_love")(()) };

        assert_eq!(output, 42);
    }

    #[test]
    fn methods() {
        let context = Context::create();
        let mut unit = unit::Unit::new(&context);
        unit.add_test_lib();

        unit.push_script(
            "
            Point2D {
                x: f32
                y: f32

                .hypotenuse(): f32 {
                    ! sqrt(self.x * self.x + self.y * self.y)
                }

                .scaled(by: f32): Point2D {
                    ! Point2D { x = self.x * by, y = self.y * by }
                }
            }

            main(): i32 {
                original: Point2D = Point2D { x = 4.0, y = 3.0 }

                hypotenuse: f32 = original.hypotenuse()
                assert_eq(hypotenuse, 5.0)

                scaled_by_2: Point2D = original.scaled(1.5)
                assert_eq(scaled_by_2.x, 6.0)
                assert_eq(scaled_by_2.y, 4.5)

                ! 0
            }
            ",
        );

        unsafe { unit.get_fn::<(), i32>("main")(()) };
    }

    #[test]
    fn arrays() {
        let context = Context::create();
        let mut unit = unit::Unit::new(&context);
        unit.add_test_lib();

        unit.push_script(
            "
            main(): i32 {
                original: i32[7] = [1, 4, 8, 15, 16, 23, 42]

                assert_eq(original[3], 15)
                assert_eq(original[0], 1)
                assert_eq(original[6], 42)
                
                index: i32 = 0
                @ index < 7 {
                    original[index] = index * 2

                    index = index + 1
                }

                assert_eq(original[0], 0)
                assert_eq(original[1], 2)
                assert_eq(original[3], 6)
                assert_eq(original[6], 12)

                ! 0
            }
            ",
        );

        unsafe { unit.get_fn::<(), i32>("main")(()) };
    }

    #[test]
    fn struct_arrays() {
        let context = Context::create();
        let mut unit = unit::Unit::new(&context);
        unit.add_test_lib();

        unit.push_script(
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

                assert_eq(points[0].x, 43)
                assert_eq(points[0].y, 15)

                points[0].x = 94

                assert_eq(points[0].x, 94)
                assert_eq(points[0].y, 15)

                points[1] = Point2D { x = 4, y = 6 }

                assert_eq(points[1].x, 4)
                assert_eq(points[1].y, 6)

                ! 0
            }
            ",
        );

        unsafe { unit.get_fn::<(), i32>("main")(()) };
    }

    #[test]
    fn backwards_struct_dependency() {
        let context = Context::create();
        let mut unit = unit::Unit::new(&context);
        unit.add_test_lib();

        unit.push_script(
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

                assert_eq(julie.letter.location.time, 2095)

                ! 0
            }
            ",
        );

        unsafe { unit.get_fn::<(), i32>("main")(()) };
    }

    #[test]
    fn backwards_call() {
        let context = Context::create();
        let mut unit = unit::Unit::new(&context);
        unit.add_test_lib();

        unit.push_script(
            "
            courthouse_1955(): i32 {
                gigawatt_count: f32 = courthouse_1985()
                assert_eq(gigawatt_count, 1.21) # great scott!

                ! 1
            }
            
            courthouse_1985(): f32 {
                ! 1.21
            }
            ",
        );

        unsafe { unit.get_fn::<(), i32>("courthouse_1955")(()) };
    }

    #[test]
    fn function_generics() {
        let context = Context::create();
        let mut unit = unit::Unit::new(&context);
        unit.add_test_lib();

        unit.push_script(
            "
            copy<T>(in: T): T {
                copy: T = in
                ! copy 
            }

            main(): i32 {
                assert_eq(copy<i32>(4), 4)

                assert_eq(copy<f32>(4.0), 4.0)

                ! 0 
            }
        ",
        );

        unsafe { unit.get_fn::<(), i32>("main")(()) };
    }
}
