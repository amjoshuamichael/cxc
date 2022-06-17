use super::prelude::*;
use crate::core_lib::CORE_LIB;
use crate::hlr::hlr;
use crate::lex::lex;
use crate::parse::prelude as p;

#[test]
fn basic_expr() {
    let hlr = hlr(p::parse(lex("{ x + 42 }")));
    let u32_t = CORE_LIB.force_get(&"#prim::u32".into());

    assert_eq!(
        hlr.expressions,
        vec![Expr {
            return_type: u32_t.clone(),
            kind: ExprKind::BinOp(
                Box::new(Expr {
                    return_type: u32_t.clone(),
                    kind: ExprKind::VarDecl("x".into()),
                }),
                p::Opcode::Plus,
                Box::new(Expr {
                    return_type: u32_t.clone(),
                    kind: ExprKind::Literal(LiteralKind::Number(42)),
                }),
            )
        }]
    );
}
