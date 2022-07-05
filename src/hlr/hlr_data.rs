use super::expr_tree::*;
use super::prelude::*;
use crate::core_lib::CORE_LIB;
use crate::parse::prelude::*;
use std::collections::HashMap;
use std::sync::Arc;

///The HLR Data Type.
#[derive(Default)]
pub struct HLR {
    pub tree: ExprTree,
    pub types: TypeGroup,
    pub identifiers: Vec<Arc<str>>,

    // used to find where variables are declared.
    pub data_flow: HashMap<Arc<str>, (Type, Vec<ExprID>)>,
}

impl HLR {
    pub fn from(expr: Expr) -> Self {
        let mut new_hlr = HLR::with_core_lib();
        new_hlr.add_expr(expr, ExprID::ROOT);

        new_hlr
    }

    pub fn with_core_lib() -> Self {
        let mut output = HLR::default();
        output.types = TypeGroup::with_core_lib();
        output
    }

    fn add_expr(&mut self, expr: Expr, parent: ExprID) -> ExprID {
        match expr {
            Expr::Number(n) => self.tree.insert(parent, NodeData::Number(n.into())),
            Expr::Float(n) => self.tree.insert(parent, NodeData::Float(n.into())),
            Expr::Ident(name) => {
                let ident = self.identifiers.iter().find(|i| ***i == *name).unwrap();
                self.tree.insert(
                    parent,
                    NodeData::Ident {
                        var_type: CORE_LIB.force_get(&"_::none".into()),
                        name: ident.clone(),
                    },
                )
            }
            Expr::VarDecl(t, n, e) => {
                let space = self.tree.make_one_space(parent);

                let ident_arc: Arc<str> = Arc::from(&*n);

                self.identifiers.push(ident_arc.clone());
                let new_decl = NodeData::VarDecl {
                    type_spec: t,
                    var_type: CORE_LIB.force_get(&"_::none".into()),
                    name: ident_arc,
                    rhs: self.add_expr(*e, space),
                };

                self.tree.replace(space, new_decl);
                space
            }
            Expr::UnarOp(op, hs) => {
                let space = self.tree.make_one_space(parent);

                let new_binop = NodeData::UnarOp {
                    ret_type: CORE_LIB.force_get(&"_::none".into()),
                    op,
                    hs: self.add_expr(*hs, space),
                };

                self.tree.replace(space, new_binop);
                space
            }
            Expr::BinOp(lhs, op, rhs) => {
                let space = self.tree.make_one_space(parent);

                let new_binop = NodeData::BinOp {
                    ret_type: CORE_LIB.force_get(&"_::none".into()),
                    lhs: self.add_expr(*lhs, space),
                    op,
                    rhs: self.add_expr(*rhs, space),
                };

                self.tree.replace(space, new_binop);
                space
            }
            Expr::IfThen(i, t) => {
                let space = self.tree.make_one_space(parent);

                let new_binop = NodeData::IfThen {
                    ret_type: CORE_LIB.force_get(&"_::none".into()),
                    i: self.add_expr(*i, space),
                    t: self.add_expr(*t, space),
                };

                self.tree.replace(space, new_binop);
                space
            }
            Expr::IfThenElse(i, t, e) => {
                let space = self.tree.make_one_space(parent);

                let new_binop = NodeData::IfThenElse {
                    ret_type: CORE_LIB.force_get(&"_::none".into()),
                    i: self.add_expr(*i, space),
                    t: self.add_expr(*t, space),
                    e: self.add_expr(*e, space),
                };

                self.tree.replace(space, new_binop);
                space
            }
            Expr::ForWhile(w, d) => {
                let space = self.tree.make_one_space(parent);

                let new_binop = NodeData::While {
                    w: self.add_expr(*w, space),
                    d: self.add_expr(*d, space),
                };

                self.tree.replace(space, new_binop);
                space
            }
            Expr::Block(stmts) => {
                let space = self.tree.make_one_space(parent);

                let mut statment_ids = Vec::new();

                for stmt in stmts {
                    statment_ids.push(self.add_expr(stmt, space));
                }

                let new_binop = NodeData::Block {
                    ret_type: CORE_LIB.force_get(&"_::none".into()),
                    stmts: statment_ids,
                };

                self.tree.replace(space, new_binop);
                space
            }
            _ => todo!(),
        }
    }
}
