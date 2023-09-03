use crate::lex::{indent_parens, TypeName, VarName};
use crate::parse::*;
use crate::Type;
use crate::unit::FuncId;
use std::fmt::{Debug, Formatter};

mod expr_tree_helper;
mod quick;

pub use quick::*;

#[derive(Default, Clone)]
pub struct ExprTree {
    pub root: ExprID,
    pub nodes: SlotMap<ExprID, ExprNode>,
}

impl ToString for ExprTree {
    fn to_string(&self) -> String {
        let data = self.get(self.root);
        let not_indented = data.to_string(self);
        indent_parens(not_indented)
    }
}

impl Debug for ExprTree {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), std::fmt::Error> {
        writeln!(fmt)?;
        for expr in &self.nodes {
            writeln!(
                fmt,
                "{:?} <- {:?} : {:?} : {:?}",
                expr.0,
                expr.1.parent,
                expr.1.data,
                expr.1.data.ret_type(),
            )?
        }

        Ok(())
    }
}

new_key_type! {
    pub struct ExprID;
}

#[derive(Clone)]
pub struct ExprNode {
    parent: ExprID,
    data: HNodeData,
}

impl ToString for ExprNode {
    fn to_string(&self) -> String {
        let code = match &self.data {
            Number { value, .. } => format!("{value}"),
            Float { value, .. } => format!("{value:?}"),
            Bool { value, .. } => format!("{value:?}"),
            StructLit {
                var_type, fields, ..
            } => {
                format!("{var_type:?} {{ {fields:?} }}")
            },
            ArrayLit { parts, .. } => {
                format!("{parts:?}")
            },
            Call { f, a, .. } => format!("{f:?}({a:?})"),
            IndirectCall { f, a, .. } => format!("{f:?}({a:?})"),
            Ident { name, .. } => format!("{name}"),
            Set { lhs, rhs, .. } => format!("{lhs:?} = {rhs:?}"),
            Member { object, field, .. } => format!("{object:?}.{field}"),
            Index { object, index, .. } => format!("{object:?}[{index:?}]"),
            UnarOp { op, hs, .. } => format!("{op:?} {hs:?}"),
            BinOp { lhs, op, rhs, .. } => format!("{lhs:?} {op:?} {rhs:?}"),
            IfThen { i, t, .. } => format!("? {i:?} {t:?}"),
            IfThenElse { i, t, e, .. } => {
                format!("? {i:?} {t:?} : {e:?}")
            },
            While { w, d, .. } => format!("@ {w:?} {d:?}"),
            Block { stmts, .. } => format!("{{{stmts:?}}}"),
            Return { to_return, .. } => format!("! {to_return:?}"),
        };

        format!("{code} :: {:?}", self.data.ret_type())
    }
}

#[derive(Clone, Debug)]
pub enum HNodeData {
    Number {
        lit_type: Type,
        value: u64,
    },
    Float {
        lit_type: Type,
        value: f64,
    },
    Bool {
        value: bool,
    },
    StructLit {
        var_type: Type,
        fields: Vec<(VarName, ExprID)>,
        initialize: InitOpts,
    },
    ArrayLit {
        var_type: Type,
        parts: Vec<ExprID>,
        initialize: InitOpts,
    },
    Ident {
        var_type: Type,
        name: VarName,
    },
    Set {
        lhs: ExprID,
        rhs: ExprID,
    },
    // TODO: combine Call and IndirectCall, making Call take a FuncQuery and 
    // abstracting over the differences between ExprID and a FuncQuery using an 
    // HCallable enum
    Call {
        ret_type: Type,
        f: VarName,
        generics: Vec<Type>,
        a: Vec<ExprID>,
        sret: Option<ExprID>,
        relation: TypeRelation,
    },
    IndirectCall {
        ret_type: Type,
        f: ExprID,
        a: Vec<ExprID>,
        sret: Option<ExprID>,
    },
    Member {
        ret_type: Type,
        object: ExprID,
        field: VarName,
    },
    Index {
        ret_type: Type,
        object: ExprID,
        index: ExprID,
    },
    UnarOp {
        ret_type: Type,
        op: Opcode,
        hs: ExprID,
    },
    BinOp {
        ret_type: Type,
        lhs: ExprID,
        op: Opcode,
        rhs: ExprID,
    },
    IfThen {
        ret_type: Type,
        i: ExprID,
        t: ExprID,
    },
    IfThenElse {
        ret_type: Type,
        i: ExprID,
        t: ExprID,
        e: ExprID,
    },
    While {
        w: ExprID,
        d: ExprID,
    },
    Block {
        ret_type: Type,
        stmts: Vec<ExprID>,
    },
    Return {
        ret_type: Type,
        to_return: Option<ExprID>,
    },
}

use slotmap::{new_key_type, SlotMap};
use HNodeData::*;

impl HNodeData {
    pub fn ret_type(&self) -> Type {
        match self {
            Number { lit_type, .. } | Float { lit_type, .. } => lit_type.clone(),
            Bool { .. } => Type::bool(),
            While { .. } | Set { .. } => Type::void(),
            Ident { var_type, .. }
            | StructLit { var_type, .. }
            | ArrayLit { var_type, .. } => var_type.clone(),
            BinOp { ret_type, .. }
            | Return { ret_type, .. }
            | UnarOp { ret_type, .. }
            | IfThen { ret_type, .. }
            | IfThenElse { ret_type, .. }
            | Call { ret_type, .. }
            | IndirectCall { ret_type, .. }
            | Block { ret_type, .. }
            | Index { ret_type, .. }
            | Member { ret_type, .. } => ret_type.clone(),
        }
    }

    pub fn ret_type_mut(&mut self) -> Option<&mut Type> {
        match self {
            Number {
                ref mut lit_type, ..
            }
            | Float {
                ref mut lit_type, ..
            } => Some(lit_type),
            Bool { .. } => None,
            While { .. } | Set { .. } => None,
            Ident {
                ref mut var_type, ..
            }
            | StructLit {
                ref mut var_type, ..
            }
            | ArrayLit {
                ref mut var_type, ..
            } => Some(var_type),
            BinOp {
                ref mut ret_type, ..
            }
            | Return {
                ref mut ret_type, ..
            }
            | UnarOp {
                ref mut ret_type, ..
            }
            | IfThen {
                ref mut ret_type, ..
            }
            | IfThenElse {
                ref mut ret_type, ..
            }
            | Call {
                ref mut ret_type, ..
            }
            | IndirectCall {
                ref mut ret_type, ..
            }
            | Block {
                ref mut ret_type, ..
            }
            | Index {
                ref mut ret_type, ..
            }
            | Member {
                ref mut ret_type, ..
            } => Some(ret_type),
        }
    }

    pub fn to_string(&self, tree: &ExprTree) -> String {
        match self {
            Number { value, lit_type } => format!("{value}{lit_type:?}"),
            Float { value, lit_type } => format!("{value}{lit_type:?}"),
            Bool { value, .. } => value.to_string(),
            Ident { name, .. } => name.to_string(),
            StructLit {
                var_type,
                fields,
                initialize,
            } => {
                let mut lit = match var_type.name() {
                    TypeName::Anonymous => format!("{var_type:?}"),
                    other => other.to_string(),
                } + " ";

                lit += "{ \n";

                for field in fields.iter() {
                    lit += &*field.0.to_string();
                    lit += " = ";
                    lit += &*tree.get(field.1).to_string(tree);
                    lit += "\n";
                }

                match initialize {
                    InitOpts::Default => lit += "++ \n",
                    InitOpts::Uninit => lit += "-- \n",
                    InitOpts::NoFill => {},
                }

                lit += "} \n";

                lit
            },
            ArrayLit {
                parts, initialize, ..
            } => {
                let mut lit = "[".into();

                for (p, part) in parts.iter().enumerate() {
                    if p > 0 {
                        lit += ", ";
                    }
                    lit += &*tree.get(*part).to_string(tree);
                }

                match initialize {
                    InitOpts::Default => lit += ", ++ ",
                    InitOpts::Uninit => lit += ", -- ",
                    InitOpts::NoFill => {},
                }

                lit += "]";

                lit
            },
            Set { lhs, rhs, .. } => {
                let mut lit = tree.get(*lhs).to_string(tree);
                lit += " = ";
                lit += &*tree.get(*rhs).to_string(tree);
                lit
            },
            Call {
                f,
                generics,
                a: args,
                relation,
                sret,
                ..
            } => {
                let mut call = match relation {
                    TypeRelation::Static(typ) => format!("{typ:?}") + "::",
                    TypeRelation::MethodOf(typ) => format!("({typ:?})") + ".",
                    TypeRelation::Unrelated => String::default(),
                };

                call += &*f.to_string();

                if !generics.is_empty() {
                    call += "<";
                    for (g, generic) in generics.iter().enumerate() {
                        if g > 0 {
                            call += ", ";
                        }
                        call += &*format!("{:?}", generic);
                    }
                    call += ">";
                }

                call += "(";

                if let Some(sret) = sret {
                    call += "-> ";
                    call += &*tree.get(*sret).to_string(tree);
                    call += " | ";
                }

                for (a, arg) in args.iter().enumerate() {
                    if a > 0 {
                        call += ", ";
                    }
                    call += &*tree.get(*arg).to_string(tree);
                }

                call += ")";
                call
            },
            IndirectCall { f, a: args, .. } => {
                let mut call = "(".into();
                call += &*tree.get(*f).to_string(tree);
                call += ")";

                call += "(";
                for (a, arg) in args.iter().enumerate() {
                    if a > 0 {
                        call += ", ";
                    }
                    call += &*tree.get(*arg).to_string(tree);
                }
                call += ")";
                call
            },
            Member { object, field, .. } => {
                let mut member = String::from("(");
                member += &*tree.get(*object).to_string(tree);
                member += ")";
                member += ".";
                member += &*field.to_string();
                member
            },
            Index { object, index, .. } => {
                let mut object = tree.get(*object).to_string(tree);
                object += "[";
                object += &*tree.get(*index).to_string(tree);
                object += "]";
                object
            },
            UnarOp { op, hs, .. } => {
                op.to_string() + &*tree.get(*hs).to_string(tree)
            },
            BinOp { lhs, op, rhs, .. } => {
                let mut binop = tree.get(*lhs).to_string(tree);
                binop += " ";
                binop += &*op.to_string();
                binop += " ";
                binop + &*tree.get(*rhs).to_string(tree)
            },
            IfThen { i, t, .. } => {
                let mut it = "? ".into();
                it += &*tree.get(*i).to_string(tree);
                it += " ";
                it += &*tree.get(*t).to_string(tree);
                it
            },
            IfThenElse { i, t, e, .. } => {
                let mut ite = "? ".into();
                ite += &*tree.get(*i).to_string(tree);
                ite += " ";
                ite += &*tree.get(*t).to_string(tree);
                ite += " : ";
                ite += &*tree.get(*e).to_string(tree);
                ite
            },
            While { w, d, .. } => {
                let mut wh = "@ ".into();
                wh += &*tree.get(*w).to_string(tree);
                wh += " ";
                wh += &*tree.get(*d).to_string(tree);
                wh
            },
            Block { stmts, .. } => {
                let mut bl = "{".into();
                for (s, stmt) in stmts.iter().enumerate() {
                    bl += &*tree.get(*stmt).to_string(tree);

                    if s != stmts.len() - 1 {
                        bl += "\n"
                    }
                }
                bl += "}";
                bl
            },
            Return { to_return, .. } => {
                let mut ret = "; ".into();
                if let Some(to_return) = to_return {
                    ret += &*tree.get(*to_return).to_string(tree);
                }
                ret
            },
        }
    }
}
