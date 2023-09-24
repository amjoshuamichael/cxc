use std::collections::{HashSet, HashMap};

use super::expr_tree::*;
use super::hlr_data_output::HLR;
use crate::errors::{CResult, TResult};
use crate::lex::VarName;
use crate::typ::ReturnStyle;
use crate::{parse::*, TypeEnum};
use crate::unit::{CompData, FuncQuery, OwnedFuncCodeQuery, ProcessedFuncInfo, FuncCodeId, Global};
use crate::Type;
use indexmap::IndexMap;
use slotmap::SlotMap;

#[derive(Debug)]
pub struct FuncRep<'a> {
    pub tree: ExprTree,
    pub comp_data: &'a CompData,
    pub name: VarName,
    pub relation: TypeRelation,
    pub generics: Vec<Type>,
    pub ret_type: Type,
    pub variables: SlotMap<VarID, VariableInfo>,
    pub specified_dependencies: HashSet<OwnedFuncCodeQuery>,
}

impl<'a> ToString for FuncRep<'a> {
    fn to_string(&self) -> String {
        let data = self.tree.get(self.tree.root);
        data.to_string(self)
    }
}

slotmap::new_key_type! { pub struct VarID; }

#[derive(Default, Debug, Clone)]
pub struct VariableInfo {
    pub typ: Type,
    pub arg_index: ArgIndex,

    // specially marks variables that definitely should not be dropped. if this is marked
    // as false, traditional drop rules still apply. this is not a way to check if a
    // variable will be dropped.
    pub do_not_drop: bool,
    pub name: VarName,
}

#[derive(Copy, Default, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ArgIndex {
    #[default]
    None,
    SRet,
    Some(usize),
}

impl VariableInfo {
    pub fn is_arg_or_sret(&self) -> bool { self.arg_index != ArgIndex::None }
}

impl<'a> FuncRep<'a> {
    pub fn from_code(
        code: &FuncCode,
        comp_data: &'a CompData,
        info: FuncQuery,
    ) -> CResult<Self> {
        let mut new = FuncRep {
            name: code.name.clone(),
            relation: match code.relation.clone() {
                TypeSpecRelation::MethodOf(typ) => {
                    TypeRelation::MethodOf(comp_data.get_spec(&typ, &info)?)
                },
                TypeSpecRelation::Static(typ) => {
                    TypeRelation::Static(comp_data.get_spec(&typ, &info)?)
                },
                TypeSpecRelation::Unrelated => TypeRelation::Unrelated,
            },
            ret_type: comp_data
                .get_spec(&code.ret_type, &info)
                .unwrap(),
            generics: info.generics,
            tree: ExprTree::default(),
            comp_data,
            variables: SlotMap::with_key(),
            specified_dependencies: HashSet::new(),
        };

        for (a, arg) in code.args.iter().enumerate() {
            let typ = new.get_type_spec(&arg.type_spec)?;

            new.variables.insert(VariableInfo {
                typ,
                arg_index: ArgIndex::Some(a),
                name: arg.name.clone(),
                do_not_drop: false,
            });
        }

        new.tree.root = new.tree.insert(
            ExprID::default(),
            HNodeData::Block {
                ret_type: Type::unknown(),
                stmts: Vec::new(),
                // only thing in new.variables is the args
                declared: new.variables.keys().collect(), 
                aliases: HashMap::new(),
                withs: HashSet::new(),
            }
        );

        let mut statment_ids = Vec::new();

        {
            let Expr::Block(ref stmts) = code.code else { unreachable!() };

            for stmt in stmts {
                statment_ids.push(new.add_expr(&stmt, new.tree.root));
            }

            let HNodeData::Block { stmts, .. } = new.tree.get_mut(new.tree.root)
                else { unreachable!() };

            *stmts = statment_ids;
        }

        Ok(new)
    }

    // includes sret argument
    pub fn args<'b>(&'b mut self) -> Vec<(VarID, &VariableInfo)> {
        let mut names_and_flow = self
            .variables
            .iter()
            .filter(|(_, v_info)| v_info.is_arg_or_sret())
            .collect::<Vec<_>>();

        names_and_flow.sort_by(|(_, linfo), (_, rinfo)| { 
            linfo.arg_index.cmp(&rinfo.arg_index) 
        });

        names_and_flow
    }

    // includes sret argument
    pub fn arg_types(&mut self) -> Vec<Type> {
        self.args().into_iter().map(|(_, v_info)| v_info.typ.clone()).collect::<Vec<_>>()
    }

    // includes sret argument
    pub fn arg_ids(&mut self) -> Vec<VarID> {
        self.args().into_iter().map(|(id, _)| id).collect::<Vec<_>>()
    }

    // includes sret argument
    pub fn arg_count(&mut self) -> u32 { self.args().len() as u32 }

    pub fn get_type_spec(&self, spec: &TypeSpec) -> TResult<Type> {
        self.comp_data.get_spec(spec, &(&self.generics, &self.relation))
    }

    pub fn output(mut self) -> (HLR, ProcessedFuncInfo) {
        let mut func_arg_types = self.arg_types();

        if self.ret_type.return_style() == ReturnStyle::Sret {
            func_arg_types.remove(0);
        }

        let func_type = self.ret_type.clone().func_with_args(func_arg_types);
        let TypeEnum::Func(func_type) = func_type.clone_type_enum() else { unreachable!() };

        let mut specified_dependencies = 
            IndexMap::<OwnedFuncCodeQuery, Option<FuncCodeId>>::new();

        for code_query in self.specified_dependencies.drain() {
            specified_dependencies.insert(code_query, None);
        }

        let mut dependencies = HashSet::<FuncQuery>::new();

        for (_call_id, call_data) in self.tree.iter() {
            let (
                HNodeData::Call { query, .. } 
                | HNodeData::GlobalLoad { global: Global::Func(query), .. }
            ) = call_data else { continue };
            dependencies.insert(query.clone());
        }

        (
            HLR {
                func_type: func_type.clone(),
                tree: self.tree,
                data_flow: self.variables,
                dependencies,
            },
            ProcessedFuncInfo {
                name: self.name,
                relation: self.relation,
                generics: self.generics,
                specified_dependencies,
                typ: func_type,
            }
        )
    }

    fn add_expr(&mut self, expr: &Expr, parent: ExprID) -> ExprID {
        match expr {
            Expr::Number(value) => self.tree.insert(
                parent,
                HNodeData::Number {
                    value: *value,
                    lit_type: Type::unknown(),
                },
            ),
            Expr::Float(value) => self.tree.insert(
                parent,
                HNodeData::Float {
                    value: *value,
                    lit_type: Type::unknown(),
                },
            ),
            Expr::Bool(value) => self.tree.insert(parent, HNodeData::Bool { value: *value }),
            Expr::String(value) => {
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
                    a: vec![ref_space, len_arg],
                    query: FuncQuery {
                        name: "from_bytes".into(),
                        relation: TypeRelationGeneric::Static(string_type),
                        generics: vec![arr_type.clone()],
                    },
                    sret: None,
                };

                self.tree.replace(call_space, call_data);
                call_space
            },
            Expr::Ident(name) => {
                let mut var_id = None;
                let mut block: ExprID = parent;

                'find_var: loop {
                    (_, block) = self.tree.statement_and_block(block);

                    let HNodeData::Block { declared, .. } = self.tree.get_ref(block)
                        else { unreachable!() };

                    for declared_var in declared {
                        if &self.variables[*declared_var].name == name {
                            var_id = Some(*declared_var);
                            break 'find_var;
                        }
                    }

                    if block == self.tree.root {
                        break 'find_var;
                    }
                }

                if let Some(var_id) = var_id {
                    let var_type = self.variables[var_id].typ.clone();

                    self.tree.insert(parent, HNodeData::Ident { var_type, var_id })
                } else if let Some(global) = self.comp_data.globals.get(name) {
                    let var_type = self.comp_data.get_type_of_global(&global).unwrap();
                    self.tree.insert(parent, HNodeData::GlobalLoad { 
                        var_type, 
                        global: global.clone(),
                    })
                } else {
                    self.tree.insert(parent, HNodeData::AccessAlias(name.clone()))
                }
            },
            Expr::Label(name) => {
                self.tree.insert(parent, HNodeData::GotoLabel(name.clone()))
            },
            Expr::SetVar(decl, e) => {
                let space = self.tree.make_one_space(parent);

                let var_type = self.get_type_spec(&decl.type_spec).unwrap(); // TODO: error

                let id = self.variables.insert(VariableInfo {
                    name: decl.name.clone(),
                    typ: var_type.clone(),
                    arg_index: ArgIndex::None,
                    do_not_drop: false,
                });

                let (_, block) = self.tree.statement_and_block(space);
                let HNodeData::Block { declared, .. } = self.tree.get_mut(block)
                    else { unreachable!() };
                declared.insert(id);

                let ident = self.tree.insert(
                    space,
                    HNodeData::Ident { var_type, var_id: id },
                );

                let statement = HNodeData::Set {
                    lhs: ident,
                    rhs: self.add_expr(&**e, space),
                };

                self.tree.replace(space, statement);

                space
            },
            Expr::Set(lhs, rhs) => {
                let space = self.tree.make_one_space(parent);

                let new_set = HNodeData::Set {
                    lhs: self.add_expr(&**lhs, space),
                    rhs: self.add_expr(&**rhs, space),
                };

                self.tree.replace(space, new_set);

                space
            },
            Expr::UnarOp(op, hs) => {
                let space = self.tree.make_one_space(parent);

                let new_binop = HNodeData::UnarOp {
                    ret_type: Type::unknown(),
                    op: *op,
                    hs: self.add_expr(&**hs, space),
                };

                self.tree.replace(space, new_binop);
                space
            },
            Expr::BinOp(op, lhs, rhs) => {
                let space = self.tree.make_one_space(parent);

                let new_binop = HNodeData::BinOp {
                    ret_type: Type::unknown(),
                    lhs: self.add_expr(&**lhs, space),
                    op: *op,
                    rhs: self.add_expr(&**rhs, space),
                };

                self.tree.replace(space, new_binop);
                space
            },
            Expr::IfThen(i, t) => {
                let space = self.tree.make_one_space(parent);

                let new_binop = HNodeData::IfThen {
                    ret_type: Type::void(),
                    i: self.add_expr(&**i, space),
                    t: self.add_expr(&**t, space),
                };

                self.tree.replace(space, new_binop);
                space
            },
            Expr::IfThenElse(i, t, e) => {
                let space = self.tree.make_one_space(parent);

                let new_binop = HNodeData::IfThenElse {
                    ret_type: Type::unknown(),
                    i: self.add_expr(&**i, space),
                    t: self.add_expr(&**t, space),
                    e: self.add_expr(&**e, space),
                };

                self.tree.replace(space, new_binop);
                space
            },
            Expr::While(w, d) => {
                let space = self.tree.make_one_space(parent);

                let new_binop = HNodeData::While {
                    w: self.add_expr(&**w, space),
                    d: self.add_expr(&**d, space),
                };

                self.tree.replace(space, new_binop);
                space
            },
            Expr::Block(stmts) => {
                let space = self.tree.make_one_space(parent);

                self.tree.replace(space, HNodeData::Block {
                    ret_type: Type::unknown(),
                    stmts: Vec::new(),
                    declared: HashSet::new(),
                    aliases: HashMap::new(),
                    withs: HashSet::new(),
                });

                let mut statment_ids = Vec::new();

                for stmt in stmts {
                    statment_ids.push(self.add_expr(stmt, space));
                }

                let HNodeData::Block { stmts, .. } = self.tree.get_mut(space)
                    else { unreachable!() };
                *stmts = statment_ids;

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
                    .collect::<Vec<_>>();

                let new_data = if let Expr::Ident(ref func_name) = &**name {
                    let relation = if *is_method {
                        TypeRelation::MethodOf(Type::unknown())
                    } else {
                        TypeRelation::Unrelated
                    };

                    HNodeData::Call {
                        ret_type: Type::unknown(),
                        query: FuncQuery {
                            name: func_name.clone(),
                            generics,
                            relation,
                        },
                        a: arg_ids,
                        sret: None,
                    }
                } else if let Expr::StaticMethodPath(ref type_spec, ref func_name) = 
                    &**name {
                    let type_origin = self.get_type_spec(type_spec).unwrap_or_default();

                    HNodeData::Call {
                        ret_type: Type::unknown(),
                        query: FuncQuery {
                            name: func_name.clone(),
                            generics: generics.clone(),
                            relation: TypeRelation::Static(type_origin),
                        },
                        a: arg_ids,
                        sret: None,
                    }
                } else {
                    let f = self.add_expr(&**name, space);
                    HNodeData::IndirectCall {
                        ret_type: Type::unknown(),
                        f,
                        a: arg_ids,
                        sret: None,
                    }
                };

                self.tree.replace(space, new_data);
                space
            },
            Expr::Member(object, field) => {
                let space = self.tree.make_one_space(parent);

                let new_member = HNodeData::Member {
                    ret_type: Type::unknown(),
                    object: self.add_expr(&**object, space),
                    field: field.clone(),
                };

                self.tree.replace(space, new_member);
                space
            },
            Expr::Struct(expr_fields, initialize) => {
                let space = self.tree.make_one_space(parent);

                let mut fields = Vec::new();

                for field in expr_fields {
                    fields.push((field.0.clone(), self.add_expr(&field.1, space)));
                }

                let new_struct = HNodeData::StructLit {
                    var_type: Type::unknown(),
                    fields,
                    initialize: *initialize,
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
                    initialize: *initialize,
                };

                self.tree.replace(space, new_struct);
                space
            },
            Expr::Return(to_return) => {
                if let Expr::Label(label) = &**to_return {
                    return self.tree.insert(parent, HNodeData::Goto(label.clone()));
                }

                let space = self.tree.make_one_space(parent);

                let new_return = HNodeData::Return {
                    ret_type: Type::unknown(),
                    to_return: Some(self.add_expr(&**to_return, space)),
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
                    initialize: *initialize,
                };

                self.tree.replace(space, new_array);
                space
            },
            Expr::Index(object, index) => {
                let space = self.tree.make_one_space(parent);

                let new_index = HNodeData::Index {
                    ret_type: Type::unknown(),
                    object: self.add_expr(&**object, space),
                    index: self.add_expr(&**index, space),
                };

                self.tree.replace(space, new_index);
                space
            },
            Expr::Enclosed(expr) => self.add_expr(&**expr, parent),
            Expr::TypedValue(type_spec, expr) => {
                let typ = self.get_type_spec(&type_spec).unwrap();
                let expr_id = self.add_expr(&**expr, parent);

                let node = self.tree.get_mut(expr_id);
                *node.ret_type_mut().unwrap() = typ;

                expr_id
            },
            Expr::With(expr) => {
                let realized_expr = self.add_expr(&**expr, parent);

                let (_, block) = self.tree.statement_and_block(realized_expr);
                let HNodeData::Block { withs, .. } = self.tree.get_mut(block)
                    else { unreachable!() };
                withs.insert(realized_expr);

                realized_expr
            },
            Expr::WithAs(expr, name) => {
                let blank = self.tree.insert(parent, HNodeData::void());

                let realized_expr = self.add_expr(&**expr, self.tree.root);

                let (_, block) = self.tree.statement_and_block(blank);
                let HNodeData::Block { aliases, .. } = self.tree.get_mut(block)
                    else { unreachable!() };
                aliases.insert(name.clone(), realized_expr);

                blank
            },
            Expr::StaticMethodPath(..) | Expr::Error { .. } => {
                unreachable!()
            },
        }
    }
}
