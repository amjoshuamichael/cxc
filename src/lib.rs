#![allow(warnings, dead_code)]
#![feature(let_else)]
#![feature(let_chains)]
#![feature(once_cell)]
#![feature(type_alias_impl_trait)]
#![feature(box_syntax)]
#[macro_use]

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

        let output: i32 = unit.run_fn("factorial", 5);
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
        unit.run_fn::<&mut i32, i32>("square", &mut num);
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

        let mut output: f32 = unsafe { unit.get_fn("seventy")(()) };
        assert_eq!(output, 70.0);
    }

    #[test]
    fn call_test() {
        let context = Context::create();
        let mut unit = unit::Unit::new(&context);

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
                ? six_times_two != 12 {
                    ! 0
                }

                six_div_two_f: f32 = divide_by_two(6.0)
                ? six_div_two_f != 3.0 {
                    ! 0
                }

                ! 1
            }
            ",
        );

        let mut does_it_work: i32 = unsafe { unit.get_fn("everything_works")(()) };
        assert_eq!(does_it_work, 1);
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
                x: i32,
                y: i32,
                z: i32,
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

        let mut new_point_y: Point3D =
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
                x: i32,
                y: i32,
            }

            sqr_magnitude_of(in_ptr: &Point2D): i32 {
                in: Point2D = *in_ptr

                ! in.x * in.x + in.y * in.y
            }
            ",
        );

        let point = Point2D { x: 2, y: 3 };

        let mut sqr_mag: i32 = unsafe { unit.get_fn("sqr_magnitude_of")(&point) };
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
                    x: i32,
                    y: i32,
                },
                size: i32,
            }

            Point2D {
                x: i32,
                y: i32,
            }

            make_square(): &{
                position: {
                    x: i32,
                    y: i32,
                },
                size: i32,
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

        let mut new_square: Square =
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
                x: T,
                y: T,
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

        let mut int_point =
            unsafe { *unit.get_fn::<(), &GenPoint2D<i32>>("int_point")(()) };
        assert_eq!(int_point.x, 42);
        assert_eq!(int_point.y, 32);

        let mut float_point =
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

        unsafe {
            unit.add_external_function(
                "print_num",
                print_num as *const usize,
                &[Type::int_of_size(64)],
                Type::never(),
            );
        }

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
}
