#![allow(warnings, dead_code)]
#![feature(let_else)]
#![feature(let_chains)]
#![feature(once_cell)]
#![feature(type_alias_impl_trait)]
#![feature(box_syntax)]
#[macro_use]
extern crate lazy_static;

pub static DEBUG: bool = true;

mod hlr;
mod lex;
mod parse;
mod to_llvm;
mod unit;

mod core_lib;
mod indent_parens;

// pub fn compile_and_run(input: &str) -> f32 {
//    let lexed = lex::lex(input);
//    let parsed = parse::parse(lexed);
//
//    0.0
//}

#[cfg(test)]
mod tests {
    use inkwell::context::Context;

    use super::*;

    #[test]
    fn full_test() {
        let context = Context::create();
        let mut unit = unit::Unit::new(&context);

        unit.push_script(
            "
            factorial: prim:i32 (of_num : prim:i32) {
                current: prim:i32 = of_num
                output: prim:i32 = 1

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
            square: prim:i32 (num : &prim:i32) {
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
            sum: prim:i32 (a: prim:i32, b: prim:i32) {
                output: prim:i32 = a + b
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
            seventy: prim:f32 () {
                output: prim:f32 = 60.0 + 10.0
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
            divide_by_two: prim:f32 (num: prim:f32) {
                output: prim:f32 = num / 2.0
                ! output
            }

            mul_by_two: prim:i32 (num: prim:i32) {
                output: prim:i32 = num * 2
                ! output
            }
    
            main: prim:i32 () {
                correct_count: prim:i32 = 0
                
                six_times_two: prim:i32 = mul_by_two(6)
                ? six_times_two == 12 {
                    correct_count = correct_count + 1
                }

                six_div_two_f: prim:f32 = divide_by_two(6.0)
                ? six_div_two_f == 3.0 {
                    correct_count = correct_count + 1
                }

                ! correct_count
            }
            ",
        );

        let mut correct_count: i32 = unsafe { unit.get_fn("main")(()) };
        assert_eq!(correct_count, 2);
    }

    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    struct Point2D {
        x: i32,
        y: i32,
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
                x: prim:i32,
                y: prim:i32,
                z: prim:i32
            }

            main: &Point3D () {
                new_point: Point3D = Point3D { x = 30 * 2, y = 52, z = 99999 }
                
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
                x: prim:i32,
                y: prim:i32
            }

            sqr_magnitude_of: prim:i32 (in_ptr: &Point2D) {
                in: Point2D = *in_ptr

                ! in.x * in.x + in.y * in.y
            }
            ",
        );

        let point = Point2D { x: 2, y: 3 };

        let mut sqr_mag: i32 = unsafe { unit.get_fn("sqr_magnitude_of")(&point) };
        assert_eq!(sqr_mag, 13);
    }
}
