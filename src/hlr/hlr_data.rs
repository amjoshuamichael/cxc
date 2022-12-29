use std::collections::HashMap;
use std::rc::Rc;

use super::expr_tree::*;
use super::hlr_data_output::FuncOutput;
use crate::lex::VarName;
use crate::parse::*;
use crate::typ::FloatType;
use crate::unit::{CompData, UniqueFuncInfo};
use crate::Type;

/// The HLR Data Type.
#[derive(Debug)]
pub struct FuncRep<'a> {
    pub tree: ExprTree,
    pub comp_data: Rc<CompData<'a>>,
    pub identifiers: Vec<VarName>,
    pub info: UniqueFuncInfo,
    pub ret_type: Type,

    // used to find where variables are declared.
    pub data_flow: HashMap<VarName, DataFlowInfo>,
}

#[derive(Default, Debug, Clone)]
pub struct DataFlowInfo {
    pub typ: Type,
    pub arg_index: Option<u32>,
}

impl DataFlowInfo {
    pub fn is_arg(&self) -> bool { self.arg_index.is_some() }
}

impl<'a> FuncRep<'a> {
    pub fn from_code(
        code: FuncCode,
        comp_data: Rc<CompData<'a>>,
        info: UniqueFuncInfo,
    ) -> Self {
        let mut new = FuncRep {
            tree: ExprTree::default(),
            ret_type: comp_data.get_spec(&code.ret_type, &info.generics).unwrap(),
            comp_data,
            identifiers: Vec::new(),
            info,
            data_flow: HashMap::new(),
        };

        for (a, arg) in code.args.iter().enumerate() {
            new.identifiers.push(arg.name.clone());

            let typ = match arg.type_spec {
                Some(ref type_spec) => new
                    .comp_data
                    .get_spec(type_spec, &new.info.generics)
                    .unwrap(),
                None => todo!(),
            };

            new.data_flow.insert(
                arg.name.clone(),
                DataFlowInfo {
                    typ,
                    arg_index: Some(a as u32),
                },
            );
        }

        for expr in code.code.iter() {
            if let Expr::Ident(name) = expr {
                if !new.data_flow.contains_key(name) {
                    new.data_flow.insert(name.clone(), DataFlowInfo::default());
                }
            }
        }

        let new_root = new.add_expr(code.code, new.tree.root);
        new.tree.root = new_root;

        new
    }

    fn args(&self) -> Vec<(VarName, DataFlowInfo)> {
        let mut names_and_flow = self
            .data_flow
            .iter()
            .map(|(n, d)| (n.clone(), d.clone()))
            .filter(|f| f.1.is_arg())
            .collect::<Vec<_>>();

        names_and_flow
            .sort_by(|(_, df), (_, df2)| df.arg_index.unwrap().cmp(&df2.arg_index.unwrap()));

        names_and_flow
    }

    pub fn arg_types(&self) -> Vec<Type> {
        self.args().drain(..).map(|f| f.1.typ).collect::<Vec<_>>()
    }

    pub fn arg_names(&self) -> Vec<VarName> {
        self.args().drain(..).map(|f| f.0).collect::<Vec<_>>()
    }

    pub fn arg_count(&self) -> u32 { self.args().len() as u32 }

    pub fn get_type_spec(&self, alias: &TypeSpec) -> Option<Type> {
        self.comp_data.get_spec(alias, &self.info.generics)
    }

    pub fn output(self) -> FuncOutput {
        let out = FuncOutput {
            func_type: self.ret_type.clone().func_with_args(self.arg_types()),
            arg_names: Some(self.arg_names()),
            tree: Some(self.tree),
            info: Some(self.info),
        };

        out
    }

    pub fn uniqueify_varname(&mut self, name: &str) -> VarName {
        let name = VarName::from(name);
        let mut uniqueified = name.clone();

        while self.identifiers.contains(&uniqueified) {
            let mut inner_name = uniqueified.to_string();
            inner_name += "_";
            uniqueified = VarName::from(&*inner_name);
        }

        self.identifiers.push(uniqueified.clone());

        uniqueified
    }

    fn add_expr(&mut self, expr: Expr, parent: ExprID) -> ExprID {
        match expr {
            Expr::Number(value) => self
                .tree
                .insert(parent, NodeData::Number { value, size: 32 }),
            Expr::Float(value) => self.tree.insert(
                parent,
                NodeData::Float {
                    value,
                    size: FloatType::F32,
                },
            ),
            Expr::Bool(value) => self.tree.insert(parent, NodeData::Bool { value }),
            Expr::Strin(value) => {
                let call_space = self.tree.make_one_space(parent);
                let ref_space = self.tree.make_one_space(call_space);
                let array_space = self.tree.make_one_space(ref_space);

                let mut byte_ids = Vec::new();
                for b in value.bytes() {
                    let byte_id = self.tree.insert(
                        array_space,
                        NodeData::Number {
                            size: 8,
                            value: b as u128,
                        },
                    );

                    byte_ids.push(byte_id);
                }

                let arr_type = Type::i(8).get_array(value.bytes().count() as u32);

                self.tree.replace(
                    array_space,
                    NodeData::ArrayLit {
                        var_type: arr_type.clone(),
                        parts: byte_ids.clone(),
                    },
                );
                self.tree.replace(
                    ref_space,
                    NodeData::UnarOp {
                        ret_type: Type::void_ptr(),
                        op: Opcode::Ref(1),
                        hs: array_space,
                    },
                );

                let len_arg = self.tree.insert(
                    call_space,
                    NodeData::Number {
                        value: byte_ids.len() as u128,
                        size: 64,
                    },
                );

                let string_type = self.comp_data.get_by_name(&"String".into()).unwrap();

                let call_data = NodeData::Call {
                    ret_type: string_type.clone(),
                    f: "from_bytes".into(),
                    a: vec![ref_space, len_arg],
                    generics: Vec::new(),
                    relation: TypeRelationGeneric::Static(string_type),
                };
                self.tree.replace(call_space, call_data);
                call_space
            },
            Expr::Ident(name) => match self.identifiers.iter().find(|i| i == &&name) {
                Some(name) => {
                    let space = self.tree.make_one_space(parent);

                    let name = name.clone();

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
                        name,
                    },
                ),
            },
            Expr::MakeVar(decl, e) => {
                let space = self.tree.make_one_space(parent);

                let var_type = if self.data_flow.contains_key(&decl.name) {
                    self.data_flow.get(&decl.name).unwrap().typ.clone()
                } else {
                    let var_type = self.get_type_spec(&decl.type_spec.unwrap()).unwrap();

                    let new_data_flow_info = DataFlowInfo {
                        typ: var_type.clone(),
                        arg_index: None,
                    };

                    self.data_flow.insert(decl.name.clone(), new_data_flow_info);
                    var_type
                };

                self.identifiers.push(decl.name.clone());
                let new_decl = NodeData::MakeVar {
                    var_type,
                    name: decl.name,
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
            Expr::Call(f, generics, args, is_method) => {
                let space = self.tree.make_one_space(parent);

                let mut arg_ids = Vec::new();

                for arg in args {
                    arg_ids.push(self.add_expr(arg, space));
                }

                let generics = generics
                    .iter()
                    .map(|spec| self.get_type_spec(spec).unwrap())
                    .collect();

                let new_data = if let Expr::Ident(ref func_name) = *f && 
                    !self.data_flow.contains_key(func_name) {
                        let relation = if is_method {
                            TypeRelation::MethodOf(Type::never())
                        } else {
                            TypeRelation::Unrelated
                        };

                        NodeData::Call {
                            ret_type: Type::never(),
                            f: func_name.clone(),
                            generics,
                            a: arg_ids,
                            relation,
                        }
                } else if let Expr::StaticMethodPath(ref type_or_alias, ref func_name) = *f {
                    let type_origin = self.get_type_spec(type_or_alias).unwrap();

                    NodeData::Call {
                        ret_type: Type::never(),
                        f: func_name.clone(),
                        generics,
                        a: arg_ids,
                        relation: TypeRelation::Static(type_origin),
                    }
                } else {
                    let f = self.add_expr(*f, space);
                    NodeData::FirstClassCall {
                        ret_type: Type::never(),
                        f,
                        a: arg_ids,
                    }
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
            Expr::Struct(type_spec, expr_fields, initialize) => {
                let space = self.tree.make_one_space(parent);

                let mut fields = Vec::new();

                for field in expr_fields {
                    fields.push((field.0, self.add_expr(field.1, space)));
                }

                let new_struct = NodeData::StructLit {
                    var_type: self.comp_data.get_spec(&type_spec, &Vec::new()).unwrap(),
                    fields,
                    initialize,
                };

                self.tree.replace(space, new_struct);
                space
            },
            Expr::Tuple(type_spec, exprs, initialize) => {
                let space = self.tree.make_one_space(parent);

                let mut fields = Vec::new();

                for (index, expr) in exprs.into_iter().enumerate() {
                    fields.push((index.to_string().into(), self.add_expr(expr, space)));
                }

                let new_struct = NodeData::StructLit {
                    var_type: self.comp_data.get_spec(&type_spec, &Vec::new()).unwrap(),
                    fields,
                    initialize,
                };

                self.tree.replace(space, new_struct);
                space
            },
            Expr::Return(to_return) => {
                let space = self.tree.make_one_space(parent);

                let new_return = NodeData::Return {
                    ret_type: Type::never(),
                    to_return: Some(self.add_expr(*to_return, space)),
                };

                self.tree.replace(space, new_return);
                space
            },
            Expr::Array(expr_parts) => {
                let space = self.tree.make_one_space(parent);

                let mut parts = Vec::new();

                for part in expr_parts {
                    parts.push(self.add_expr(part, space));
                }

                let new_array = NodeData::ArrayLit {
                    var_type: Type::never(),
                    parts,
                };

                self.tree.replace(space, new_array);
                space
            },
            Expr::Index(object, index) => {
                let space = self.tree.make_one_space(parent);

                let new_index = NodeData::Index {
                    ret_type: Type::never(),
                    object: self.add_expr(*object, space),
                    index: self.add_expr(*index, space),
                };

                self.tree.replace(space, new_index);
                space
            },
            Expr::Enclosed(expr) => self.add_expr(*expr, parent),
            Expr::Op(_) | Expr::ArgList(..) | Expr::StaticMethodPath(..) => {
                unreachable!()
            },
        }
    }
}
