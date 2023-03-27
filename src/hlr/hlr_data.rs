use std::collections::HashMap;

use super::expr_tree::*;
use super::hlr_data_output::FuncOutput;
use crate::errors::{CResult, TResult};
use crate::lex::VarName;
use crate::parse::*;
use crate::typ::ReturnStyle;
use crate::unit::{CompData, UniqueFuncInfo};
use crate::Type;

#[derive(Debug)]
pub struct FuncRep<'a> {
    pub tree: ExprTree,
    pub comp_data: &'a CompData,
    pub identifiers: Vec<VarName>,
    pub info: UniqueFuncInfo,
    pub ret_type: Type,

    // used to find where variables are declared.
    pub data_flow: DataFlow,
}

pub type DataFlow = HashMap<VarName, DataFlowInfo>;

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
        comp_data: &'a CompData,
        info: UniqueFuncInfo,
    ) -> CResult<Self> {
        let mut new = FuncRep {
            tree: ExprTree::default(),
            ret_type: comp_data
                .get_spec(&code.ret_type, &info)
                .unwrap(),
            comp_data,
            identifiers: Vec::new(),
            info,
            data_flow: HashMap::new(),
        };

        for (a, arg) in code.args.iter().enumerate() {
            new.identifiers.push(arg.name.clone());

            let typ = new.comp_data.get_spec(&arg.type_spec, &new.info)?;

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

        Ok(new)
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

    pub fn get_type_spec(&self, alias: &TypeSpec) -> TResult<Type> {
        self.comp_data.get_spec(alias, &self.info)
    }

    pub fn comp_data(&self) -> &CompData { self.comp_data }

    pub fn output(self) -> FuncOutput {
        let mut func_arg_types = self.arg_types();

        if self.ret_type.return_style() == ReturnStyle::Sret {
            func_arg_types.remove(0);
        }

        FuncOutput {
            func_type: self.ret_type.clone().func_with_args(func_arg_types),
            arg_names: Some(self.arg_names()),
            tree: Some(self.tree),
            data_flow: Some(self.data_flow),
            info: Some(self.info),
        }
    }

    fn add_expr(&mut self, expr: Expr, parent: ExprID) -> ExprID {
        match expr {
            Expr::Number(value) => self.tree.insert(
                parent,
                HNodeData::Number {
                    value,
                    lit_type: Type::i(32),
                },
            ),
            Expr::Float(value) => self.tree.insert(
                parent,
                HNodeData::Float {
                    lit_type: Type::f32(),
                    value,
                },
            ),
            Expr::Bool(value) => self.tree.insert(parent, HNodeData::Bool { value }),
            Expr::Strin(value) => {
                let call_space = self.tree.make_one_space(parent);
                let ref_space = self.tree.make_one_space(call_space);
                let array_space = self.tree.make_one_space(ref_space);

                let mut byte_ids = Vec::new();
                for b in value.bytes() {
                    let byte_id = self.tree.insert(
                        array_space,
                        HNodeData::Number {
                            lit_type: Type::i(8),
                            value: b as u64,
                        },
                    );

                    byte_ids.push(byte_id);
                }

                let arr_type = Type::i(8).get_array(value.bytes().count() as u32);

                self.tree.replace(
                    array_space,
                    HNodeData::ArrayLit {
                        var_type: arr_type.clone(),
                        parts: byte_ids.clone(),
                        initialize: InitOpts::NoFill,
                    },
                );
                self.tree.replace(
                    ref_space,
                    HNodeData::UnarOp {
                        ret_type: arr_type.get_ref(),
                        op: Opcode::Ref,
                        hs: array_space,
                    },
                );

                let len_arg = self.tree.insert(
                    call_space,
                    HNodeData::Number {
                        lit_type: Type::i(64),
                        value: byte_ids.len() as u64,
                    },
                );

                let string_type = self.comp_data.get_by_name(&"String".into()).unwrap();

                let call_data = HNodeData::Call {
                    ret_type: string_type.clone(),
                    f: "from_bytes".into(),
                    a: vec![ref_space, len_arg],
                    generics: Vec::new(),
                    relation: TypeRelationGeneric::Static(string_type),
                };
                self.tree.replace(call_space, call_data);
                call_space
            },
            Expr::Ident(name) => {
                let var_type = match self.data_flow.get(&name) {
                    Some(data_flow) => data_flow.typ.clone(),
                    None => self
                        .comp_data
                        .globals
                        .get(&name.clone())
                        .unwrap_or_else(|| panic!("could not find identifier {name}"))
                        .0
                        .clone(),
                };

                self.tree.insert(parent, HNodeData::Ident { var_type, name })
            },
            Expr::SetVar(decl, e) => {
                let space = self.tree.make_one_space(parent);

                let var_type = if self.data_flow.contains_key(&decl.name) {
                    self.data_flow.get(&decl.name).unwrap().typ.clone()
                } else {
                    let var_type = self.get_type_spec(&decl.type_spec).unwrap();

                    let new_data_flow_info = DataFlowInfo {
                        typ: var_type.clone(),
                        arg_index: None,
                    };

                    self.data_flow.insert(decl.name.clone(), new_data_flow_info);
                    var_type
                };

                let statement = if !self.identifiers.contains(&decl.name) {
                    HNodeData::MakeVar {
                        var_type,
                        name: decl.name,
                        rhs: self.add_expr(*e, space),
                    }
                } else {
                    let lhs = self.tree.insert(
                        space,
                        HNodeData::Ident {
                            var_type: var_type.clone(),
                            name: decl.name,
                        },
                    );

                    HNodeData::Set {
                        ret_type: var_type,
                        lhs,
                        rhs: self.add_expr(*e, space),
                    }
                };

                self.tree.replace(space, statement);

                space
            },
            Expr::Set(lhs, rhs) => {
                let space = self.tree.make_one_space(parent);

                let new_set = HNodeData::Set {
                    ret_type: Type::unknown(),
                    lhs: self.add_expr(*lhs, space),
                    rhs: self.add_expr(*rhs, space),
                };

                self.tree.replace(space, new_set);

                space
            },
            Expr::UnarOp(op, hs) => {
                let space = self.tree.make_one_space(parent);

                let new_binop = HNodeData::UnarOp {
                    ret_type: Type::unknown(),
                    op,
                    hs: self.add_expr(*hs, space),
                };

                self.tree.replace(space, new_binop);
                space
            },
            Expr::BinOp(op, lhs, rhs) => {
                let space = self.tree.make_one_space(parent);

                let new_binop = HNodeData::BinOp {
                    ret_type: Type::unknown(),
                    lhs: self.add_expr(*lhs, space),
                    op,
                    rhs: self.add_expr(*rhs, space),
                };

                self.tree.replace(space, new_binop);
                space
            },
            Expr::IfThen(i, t) => {
                let space = self.tree.make_one_space(parent);

                let new_binop = HNodeData::IfThen {
                    ret_type: Type::void(),
                    i: self.add_expr(*i, space),
                    t: self.add_expr(*t, space),
                };

                self.tree.replace(space, new_binop);
                space
            },
            Expr::IfThenElse(i, t, e) => {
                let space = self.tree.make_one_space(parent);

                let new_binop = HNodeData::IfThenElse {
                    ret_type: Type::unknown(),
                    i: self.add_expr(*i, space),
                    t: self.add_expr(*t, space),
                    e: self.add_expr(*e, space),
                };

                self.tree.replace(space, new_binop);
                space
            },
            Expr::ForWhile(w, d) => {
                let space = self.tree.make_one_space(parent);

                let new_binop = HNodeData::While {
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

                let new_binop = HNodeData::Block {
                    ret_type: Type::unknown(),
                    stmts: statment_ids,
                };

                self.tree.replace(space, new_binop);
                space
            },
            Expr::Call {
                func: name,
                generics,
                args,
                is_method,
            } => {
                let space = self.tree.make_one_space(parent);

                let mut arg_ids = Vec::new();

                for arg in args {
                    arg_ids.push(self.add_expr(arg, space));
                }

                let generics = generics
                    .iter()
                    .map(|spec| self.get_type_spec(spec).unwrap())
                    .collect();

                let new_data = if let Expr::Ident(ref func_name) = *name {
                    let relation = if is_method {
                        TypeRelation::MethodOf(Type::unknown())
                    } else {
                        TypeRelation::Unrelated
                    };

                    HNodeData::Call {
                        ret_type: Type::unknown(),
                        f: func_name.clone(),
                        generics,
                        a: arg_ids,
                        relation,
                    }
                } else if let Expr::StaticMethodPath(ref type_spec, ref func_name) = *name {
                    let type_origin = self.get_type_spec(type_spec).unwrap_or(Type::unknown());

                    HNodeData::Call {
                        ret_type: Type::unknown(),
                        f: func_name.clone(),
                        generics,
                        a: arg_ids,
                        relation: TypeRelation::Static(type_origin),
                    }
                } else {
                    let f = self.add_expr(*name, space);
                    HNodeData::IndirectCall {
                        ret_type: Type::unknown(),
                        f,
                        a: arg_ids,
                    }
                };

                self.tree.replace(space, new_data);
                space
            },
            Expr::Member(object, field) => {
                let space = self.tree.make_one_space(parent);

                let new_member = HNodeData::Member {
                    ret_type: Type::unknown(),
                    object: self.add_expr(*object, space),
                    field,
                };

                self.tree.replace(space, new_member);
                space
            },
            Expr::Struct(expr_fields, initialize) => {
                let space = self.tree.make_one_space(parent);

                let mut fields = Vec::new();

                for field in expr_fields {
                    fields.push((field.0, self.add_expr(field.1, space)));
                }

                let new_struct = HNodeData::StructLit {
                    var_type: Type::unknown(),
                    fields,
                    initialize,
                };

                self.tree.replace(space, new_struct);
                space
            },
            Expr::Tuple(exprs, initialize) => {
                let space = self.tree.make_one_space(parent);

                let mut fields = Vec::new();

                for (index, expr) in exprs.into_iter().enumerate() {
                    fields.push((VarName::TupleIndex(index as _), self.add_expr(expr, space)));
                }

                let new_struct = HNodeData::StructLit {
                    var_type: Type::unknown(),
                    fields,
                    initialize,
                };

                self.tree.replace(space, new_struct);
                space
            },
            Expr::Return(to_return) => {
                let space = self.tree.make_one_space(parent);

                let new_return = HNodeData::Return {
                    ret_type: Type::unknown(),
                    to_return: Some(self.add_expr(*to_return, space)),
                };

                self.tree.replace(space, new_return);
                space
            },
            Expr::Array(expr_parts, initialize) => {
                let space = self.tree.make_one_space(parent);

                let mut parts = Vec::new();

                for part in expr_parts {
                    parts.push(self.add_expr(part, space));
                }

                let new_array = HNodeData::ArrayLit {
                    var_type: Type::unknown(),
                    parts,
                    initialize,
                };

                self.tree.replace(space, new_array);
                space
            },
            Expr::Index(object, index) => {
                let space = self.tree.make_one_space(parent);

                let new_index = HNodeData::Index {
                    ret_type: Type::unknown(),
                    object: self.add_expr(*object, space),
                    index: self.add_expr(*index, space),
                };

                self.tree.replace(space, new_index);
                space
            },
            Expr::Enclosed(expr) => self.add_expr(*expr, parent),
            Expr::TypedValue(type_spec, expr) => {
                let typ = self.get_type_spec(&type_spec).unwrap();
                let expr_id = self.add_expr(*expr, parent);

                let node = self.tree.get_mut(expr_id);
                *node.ret_type_mut().unwrap() = typ;

                expr_id
            },
            Expr::StaticMethodPath(..) | Expr::Error { .. } => {
                unreachable!()
            },
        }
    }
}
