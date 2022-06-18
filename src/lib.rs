#[allow(dead_code)]
#[macro_use]
extern crate lalrpop_util;
#[macro_use]
extern crate lazy_static;

lalrpop_mod!(pub serf_parser);

mod hlr;
mod lex;
mod parse;
mod to_llvm;

mod core_lib;
mod indent_parens;

pub fn compile_and_run(input: &str) -> i32 {
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
        assert_eq!(
            compile_and_run(
                r#"
            {
                x = 0
                x < 1000 @ {
                    x = x + 1
                }
                x
            }
            "#
            ),
            1000
        );
    }
}
