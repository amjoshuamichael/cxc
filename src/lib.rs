#![allow(warnings, unused)]
#[macro_use]
extern crate lalrpop_util;
#[macro_use]
extern crate lazy_static;

lalrpop_mod!(pub serf_parser);

pub static DEBUG: bool = true;

mod hlr;
mod lex;
mod parse;
mod parser;
mod to_llvm;
mod unit;

mod core_lib;
mod indent_parens;

pub fn compile_and_run(input: &str) -> f32 {
    let lexed = lex::lex(input);
    let parsed = parse::parse(lexed);

    0.0
}

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
            factorial : prim::i32 (of_num : prim::i32) {
                current : prim::i32 = of_num
                output : prim::i32 = 1

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
            square : prim::i32 (num : &prim::i32) {
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
            sum : prim::i32 (a : prim::i32, b : prim::i32) {
                output : prim::i32 = a + b
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
            seventy : prim::f32 () { 
                # this calculates 70.0
                output : prim::f32 = 60.0 + 10.0 
                ! output 
            }
            ",
        );

        let mut output: f32 = unsafe { unit.get_fn("seventy")(()) };
        assert_eq!(output, 70.0);
    }

    //   #[test]
    fn call_test() {
        let context = Context::create();
        let mut unit = unit::Unit::new(&context);

        unit.push_script(
            "
            divide_by_two : prim::i32 (num : prim::i32) {
                output : prim::i32 = num / 2
                ! output
            }

            main : prim::i32 () {
                output : prim::i32 = divide_by_two(6)
                ! output
            }
            ",
        );

        let mut output: f32 = unsafe { unit.get_fn("main")(()) };
        assert_eq!(output, 70.0);
    }

    fn struct_test() {
        let context = Context::create();
        let mut unit = unit::Unit::new(&context);

        unit.push_script(
            "
            Point2D {
                x: prim::i32,
                y: prim::i32,
            }

            ",
        );
    }
}
