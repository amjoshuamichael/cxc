use super::prelude::*;
use crate::core_lib::CORE_LIB;
use crate::parse::Opcode;
use num_bigint::BigInt;
use std::fmt::{Debug, Formatter};
use std::sync::Arc;

#[derive(Default)]
pub struct ExprTree {
    nodes: Vec<ExprNode>,
}

impl ExprTree {
    pub fn top_down_iter<'a>(&'a mut self) -> Box<dyn Iterator<Item = (ExprID, &mut NodeData)> + 'a> {
        Box::new(
            self.nodes
                .iter_mut()
                .enumerate()
                .map(|(id, node)| (ExprID(id), &mut node.data)),
        )
    }

    pub fn bottom_up_iter<'a>(&'a mut self) -> Box<dyn Iterator<Item = (ExprID, &mut NodeData)> + 'a> {
        Box::new(
            self.nodes
                .iter_mut()
                .enumerate()
                .rev()
                .map(|(id, node)| (ExprID(id), &mut node.data)),
        )
    }

    pub fn insert(&mut self, parent: ExprID, data: NodeData) -> ExprID {
        self.nodes.push(ExprNode { parent, data });
        ExprID(self.nodes.len() - 1)
    }

    pub fn replace(&mut self, at: ExprID, with: NodeData) {
        self.nodes[at.0].data = with;
    }

    pub fn make_one_space(&mut self, parent: ExprID) -> ExprID {
        self.nodes.push(ExprNode {
            parent,
            data: NodeData::Empty,
        });
        ExprID(self.nodes.len() - 1)
    }

    pub fn get(&self, at: ExprID) -> NodeData {
        self.nodes[at.0].data.clone()
    }

    pub fn parent(&self, of: ExprID) -> ExprID {
        self.nodes[of.0].parent
    }

    pub fn node_count(&self) -> u32 {
        self.nodes.len().try_into().unwrap()
    }
}

impl Debug for ExprTree {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), std::fmt::Error> {
        for (e, expr) in self.nodes.iter().enumerate() {
            print!("{e}: {expr:?}");
        }

        Ok(())
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct ExprID(usize);

impl ExprID {
    pub const ROOT: ExprID = ExprID(0);
}

struct ExprNode {
    parent: ExprID,
    data: NodeData,
}

impl Debug for ExprNode {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        match &self.data {
            Empty => println!("Empty"),
            Number(n) => println!("{n}"),
            Float(fl) => println!("{fl:?}"),
            Strin(s) => println!("{s}"),
            Ident { name, .. } => println!("{name}"),
            VarDecl {
                var_type, name, rhs, ..
            } => println!("{name}: {var_type:?} = {rhs:?}"),
            UnarOp { op, hs, .. } => println!("{op:?} {hs:?}"),
            BinOp { lhs, op, rhs, .. } => println!("{lhs:?} {op:?} {rhs:?}"),
            IfThen { i, t, .. } => println!("if {i:?} then {t:?}"),
            IfThenElse { i, t, e, .. } => println!("if {i:?} then {t:?} else {e:?}"),
            While { w, d, .. } => println!("while {w:?} do {d:?}"),
            Block { stmts, .. } => println!("{{{stmts:?}}}"),
        }

        Ok(())
    }
}

#[derive(Clone, Debug)]
pub enum NodeData {
    Empty,
    Number(BigInt),
    Float(f64),
    Strin(String),
    Ident {
        var_type: Type,
        name: Arc<str>,
    },
    VarDecl {
        type_spec: Option<(u8, String)>,
        var_type: Type,
        name: Arc<str>,
        rhs: ExprID,
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
}

pub enum GeneralReturnType {
    PrimInt,
    PrimFloat,
    PrimString,
    PrimRef(Box<GeneralReturnType>),
    Struct,
}

use GeneralReturnType::*;
use NodeData::*;

impl NodeData {
    pub fn gen_ret_type(&self) -> GeneralReturnType {
        if matches!(self, Number(_)) {
            return PrimInt;
        }

        if matches!(self, Float(_)) {
            return PrimFloat;
        }

        fn type_from_name(input: &str) -> GeneralReturnType {
            match input {
                "prim::i8" | "prim::i16" | "prim::i32" | "prim::i64" => PrimInt,
                "prim::f8" | "prim::f16" | "prim::f32" | "prim::f64" => PrimFloat,
                s if s.chars().next() == Some('&') => PrimRef(Box::new(type_from_name(&input[1..]))),
                _ => todo!(),
            }
        }

        type_from_name(&*self.ret_type().unwrap().name())
    }

    pub fn ret_type(&self) -> Option<Type> {
        match self {
            Number(_) => Some(CORE_LIB.force_get(&"prim::i32".into())),
            Float(_) => Some(CORE_LIB.force_get(&"prim::f32".into())),
            Strin(_) => todo!(),
            Ident { var_type, .. } | VarDecl { var_type, .. } => Some(var_type.clone()),
            BinOp { ret_type, .. }
            | UnarOp { ret_type, .. }
            | IfThen { ret_type, .. }
            | IfThenElse { ret_type, .. }
            | Block { ret_type, .. } => Some(ret_type.clone()),
            Empty => unreachable!(),
            While { .. } => None,
        }
    }
}
