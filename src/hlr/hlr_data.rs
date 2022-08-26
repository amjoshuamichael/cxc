use super::expr_tree::*;
use super::prelude::*;
use crate::parse::*;
use indexmap::IndexMap;
use std::sync::Arc;

/// The HLR Data Type.
#[derive(Debug, Default)]
pub struct FuncRep {
    pub tree: ExprTree,
    pub types: TypeGroup,
    pub identifiers: Vec<Arc<str>>,

    // used to find where variables are declared.
    pub data_flow: IndexMap<Arc<str>, DataFlowInfo>,
}

#[derive(Debug)]
pub struct DataFlowInfo {
    pub typ: Type,
    pub ids: Vec<ExprID>,
    pub is_func_param: bool,
}

impl FuncRep {
    pub fn from(args: Vec<VarDecl>, expr: Expr, types: &TypeGroup) -> Self {
        let mut new_hlr = FuncRep::with_types(types.clone());

        for arg in args.iter() {
            let name: Arc<str> = Arc::from(&*arg.var_name);

            new_hlr.identifiers.push(name.clone());

            let typ = match arg.type_spec {
                Some(ref type_spec) => new_hlr.types.get_spec(&type_spec).unwrap(),
                None => todo!(),
            };

            new_hlr.data_flow.insert(
                name,
                DataFlowInfo {
                    typ,
                    ids: Vec::new(),
                    is_func_param: true,
                },
            );
        }

        new_hlr.add_expr(expr, ExprID::ROOT);

        new_hlr
    }

    pub fn with_core_lib() -> Self {
        let mut output = FuncRep::default();
        output.types = TypeGroup::default();
        output
    }

    fn with_types(types: TypeGroup) -> Self {
        FuncRep {
            types,
            ..Default::default()
        }
    }

    fn add_expr(&mut self, expr: Expr, parent: ExprID) -> ExprID {
        match expr {
            Expr::Number(n) => self.tree.insert(
                parent,
                NodeData::Number {
                    value: n.into(),
                    size: 32,
                },
            ),
            Expr::Float(n) => self.tree.insert(
                parent,
                NodeData::Float {
                    value: n.into(),
                    size: 32,
                },
            ),
            Expr::Ident(name) => {
                match self.identifiers.iter().find(|i| ***i == *name) {
                    Some(name) => {
                        let space = self.tree.make_one_space(parent);

                        let name = name.clone();
                        let data_flow_info = self.data_flow.get_mut(&name).unwrap();
                        data_flow_info.ids.push(space);

                        self.tree.replace(
                            space,
                            NodeData::Ident {
                                var_type: Type::never(),
                                name,
                            },
                        );

                        space
                    },
                    None => self.tree.insert(
                        parent,
                        NodeData::Ident {
                            var_type: Type::never(),
                            name: Arc::from(&*name),
                        },
                    ),
                }
            },
            Expr::MakeVar(decl, e) => {
                let space = self.tree.make_one_space(parent);

                let var_name: Arc<str> = Arc::from(&*decl.var_name);

                let var_type = Type::never();

                if !self.data_flow.contains_key(&var_name) {
                    let new_data_flow_info = DataFlowInfo {
                        typ: var_type.clone(),
                        ids: vec![space],
                        is_func_param: false,
                    };

                    self.data_flow.insert(var_name.clone(), new_data_flow_info);
                }

                self.identifiers.push(var_name.clone());
                let new_decl = NodeData::MakeVar {
                    type_spec: decl.type_spec,
                    var_type,
                    name: var_name.clone(),
                    rhs: self.add_expr(*e, space),
                };

                self.tree.replace(space, new_decl);

                space
            },
            Expr::SetVar(lhs, rhs) => {
                let space = self.tree.make_one_space(parent);

                let new_set = NodeData::SetVar {
                    ret_type: Type::never(),
                    lhs: self.add_expr(*lhs, space),
                    rhs: self.add_expr(*rhs, space),
                };

                self.tree.replace(space, new_set);

                space
            },
            Expr::UnarOp(op, hs) => {
                let space = self.tree.make_one_space(parent);

                let new_binop = NodeData::UnarOp {
                    ret_type: Type::never(),
                    op,
                    hs: self.add_expr(*hs, space),
                };

                self.tree.replace(space, new_binop);
                space
            },
            Expr::BinOp(op, lhs, rhs) => {
                let space = self.tree.make_one_space(parent);

                let new_binop = NodeData::BinOp {
                    ret_type: Type::never(),
                    lhs: self.add_expr(*lhs, space),
                    op,
                    rhs: self.add_expr(*rhs, space),
                };

                self.tree.replace(space, new_binop);
                space
            },
            Expr::IfThen(i, t) => {
                let space = self.tree.make_one_space(parent);

                let new_binop = NodeData::IfThen {
                    ret_type: Type::never(),
                    i: self.add_expr(*i, space),
                    t: self.add_expr(*t, space),
                };

                self.tree.replace(space, new_binop);
                space
            },
            Expr::IfThenElse(i, t, e) => {
                let space = self.tree.make_one_space(parent);

                let new_binop = NodeData::IfThenElse {
                    ret_type: Type::never(),
                    i: self.add_expr(*i, space),
                    t: self.add_expr(*t, space),
                    e: self.add_expr(*e, space),
                };

                self.tree.replace(space, new_binop);
                space
            },
            Expr::ForWhile(w, d) => {
                let space = self.tree.make_one_space(parent);

                let new_binop = NodeData::While {
                    w: self.add_expr(*w, space),
                    d: self.add_expr(*d, space),
                };

                self.tree.replace(space, new_binop);
                space
            },
            Expr::Block(stmts) => {
                let space = self.tree.make_one_space(parent);

                let mut statment_ids = Vec::new();

                for stmt in stmts {
                    statment_ids.push(self.add_expr(stmt, space));
                }

                let new_binop = NodeData::Block {
                    ret_type: Type::never(),
                    stmts: statment_ids,
                };

                self.tree.replace(space, new_binop);
                space
            },
            Expr::Call(f, args) => {
                let space = self.tree.make_one_space(parent);

                let mut arg_ids = Vec::new();

                for arg in args {
                    arg_ids.push(self.add_expr(arg, space));
                }

                let new_data = NodeData::Call {
                    ret_type: Type::never(),
                    f,
                    a: arg_ids,
                    def: None,
                };

                self.tree.replace(space, new_data);
                space
            },
            Expr::Member(object, field) => {
                let space = self.tree.make_one_space(parent);

                let new_member = NodeData::Member {
                    ret_type: Type::never(),
                    object: self.add_expr(*object, space),
                    field,
                };

                self.tree.replace(space, new_member);
                space
            },
            Expr::Struct(type_alias, expr_fields) => {
                let space = self.tree.make_one_space(parent);

                let mut fields = Vec::new();

                for field in expr_fields {
                    fields.push((field.0, self.add_expr(field.1, space)));
                }

                let new_struct = NodeData::StructLit {
                    var_type: self.types.get_spec(&type_alias).unwrap(),
                    fields,
                };

                self.tree.replace(space, new_struct);
                space
            },
            Expr::Return(to_return) => {
                let space = self.tree.make_one_space(parent);

                let new_return = NodeData::Return {
                    ret_type: Type::never(),
                    to_return: self.add_expr(*to_return, space),
                };

                self.tree.replace(space, new_return);
                space
            },
            Expr::Op(_) | Expr::ArgList(_) => unreachable!(),
        }
    }
}
