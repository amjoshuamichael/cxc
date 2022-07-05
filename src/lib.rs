#![allow(dead_code)]
#[macro_use]
extern crate lalrpop_util;
#[macro_use]
extern crate lazy_static;

lalrpop_mod!(pub serf_parser);

pub static DEBUG: bool = true;

mod hlr;
mod lex;
mod parse;
mod to_llvm;
mod unit;

mod core_lib;
mod indent_parens;

pub fn compile_and_run(input: &str) -> f32 {
    let lexed = lex::lex(input);
    let parsed = parse::parse(lexed);
    let hlr = hlr::hlr(parsed);
    let llvm = to_llvm::to_llvm(hlr);

    llvm
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn full_test() {
        let unit = unit::Unit::new();

        unit.push_script(
            "
            main: prim::i32 () {
                num_in_seq: prim::i32 = 1
                prev_num_in_seq: prim::i32 = 2
                acc: prim::i32 = 0

                i: prim::i32 = 0
                @ i < 20 {
                    i = i + 1

                    acc = acc + num_in_seq

                    prev_num_in_seq = num_in_seq
                    num_in_seq = num_in_seq + prev_num_in_seq
                }

                i
            }
        ",
        );

        let output = unit.run_fn::<i32>("main", &[]);

        println!(output);
    }
}
