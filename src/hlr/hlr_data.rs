use super::expr_tree::*;
use super::prelude::TypeGroup;
use crate::core_lib::CORE_LIB;
use crate::parse::prelude::*;
use std::sync::Arc;

///The HLR Data Type.
#[derive(Default)]
pub struct HLR {
    pub tree: ExprTree,
    types: TypeGroup,
    pub identifiers: Vec<Arc<str>>,

    // used to find where variables are declared.
    pub assignments: Vec<ExprID>,
}

impl HLR {
    fn from(expr: Expr) -> Self {
        let mut new_hlr = HLR::default();
        new_hlr.add_expr(expr, ExprID::ROOT);

        new_hlr
    }

    fn add_expr(&mut self, expr: Expr, parent: ExprID) -> ExprID {
        match expr {
            Expr::Number(n) => self.tree.insert(parent, NodeData::Number(n.into())),
            Expr::Ident(name) => match self.identifiers.iter().find(|i| ***i == *name) {
                Some(ident) => self.tree.insert(parent, NodeData::Ident(ident.clone())),
                None => {
                    let ident_arc: Arc<str> = Arc::from(&*name);
                    self.identifiers.push(ident_arc.clone());
                    self.tree.insert(
                        parent,
                        NodeData::VarDecl {
                            var_type: CORE_LIB.force_get(&"#prim::u32".into()),
                            name: ident_arc,
                        },
                    )
                }
            },
            Expr::BinOp(lhs, op, rhs) => {
                let space = self.tree.make_one_space(parent);

                let new_binop = NodeData::BinOp {
                    ret_type: CORE_LIB.force_get(&"#prim::u32".into()),
                    lhs: self.add_expr(*lhs, space),
                    op,
                    rhs: self.add_expr(*rhs, space),
                };

                self.tree.replace_data(space, new_binop);
                space
            }
            Expr::IfThen(i, t) => {
                let space = self.tree.make_one_space(parent);

                let new_binop = NodeData::IfThen {
                    ret_type: CORE_LIB.force_get(&"#prim::u32".into()),
                    i: self.add_expr(*i, space),
                    t: self.add_expr(*t, space),
                };

                self.tree.replace_data(space, new_binop);
                space
            }
            Expr::IfThenElse(i, t, e) => {
                let space = self.tree.make_one_space(parent);

                let new_binop = NodeData::IfThenElse {
                    ret_type: CORE_LIB.force_get(&"#prim::u32".into()),
                    i: self.add_expr(*i, space),
                    t: self.add_expr(*t, space),
                    e: self.add_expr(*e, space),
                };

                self.tree.replace_data(space, new_binop);
                space
            }
            Expr::ForWhile(w, d) => {
                let space = self.tree.make_one_space(parent);

                let new_binop = NodeData::While {
                    w: self.add_expr(*w, space),
                    d: self.add_expr(*d, space),
                };

                self.tree.replace_data(space, new_binop);
                space
            }
            Expr::Block(stmts) => {
                let space = self.tree.make_one_space(parent);

                let mut statment_ids = Vec::new();

                for stmt in stmts {
                    statment_ids.push(self.add_expr(stmt, space));
                }

                let new_binop = NodeData::Block {
                    ret_type: CORE_LIB.force_get(&"#prim::u32".into()),
                    stmts: statment_ids,
                };

                self.tree.replace_data(space, new_binop);
                space
            }
            _ => todo!(),
        }
    }
}

pub struct Identifier {
    name: Arc<String>,
    ident_type: IdentType,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum IdentType {
    Variable,
    Goto,
}
