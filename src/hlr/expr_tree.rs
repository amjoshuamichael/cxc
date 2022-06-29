use super::Type;
use crate::parse::Opcode;
use num_bigint::BigInt;
use std::fmt::{Debug, Formatter};
use std::iter::Map;
use std::slice::Iter;
use std::sync::Arc;

#[derive(Default, Debug)]
pub struct ExprTree {
    nodes: Vec<ExprNode>,
}

impl ExprTree {
    pub fn iter<'a>(&'a self) -> Box<dyn Iterator<Item = (ExprID, NodeData)> + 'a> {
        Box::new(
            self.nodes
                .iter()
                .enumerate()
                .map(|(id, node)| (ExprID(id), node.data.clone())),
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
            data: NodeData::NoData,
        });
        ExprID(self.nodes.len() - 1)
    }

    pub fn get(&self, at: ExprID) -> NodeData {
        self.nodes[at.0].data.clone()
    }

    pub fn parent(&self, of: ExprID) -> ExprID {
        self.nodes[of.0].parent
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct ExprID(usize);

impl ExprID {
    pub const ROOT: ExprID = ExprID(0);
}

struct ExprNode {
    parent: ExprID,
    data: NodeData,
}

impl Debug for ExprNode {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(fmt, "{:?}\n", self.data)
    }
}

#[derive(Clone, Debug)]
pub enum NodeData {
    NoData,
    Number(BigInt),
    Strin(String),
    Ident {
        var_type: Arc<Type>,
        name: Arc<str>,
    },
    VarDecl {
        var_type: Arc<Type>,
        name: Arc<str>,
    },
    BinOp {
        ret_type: Arc<Type>,
        lhs: ExprID,
        op: Opcode,
        rhs: ExprID,
    },
    IfThen {
        ret_type: Arc<Type>,
        i: ExprID,
        t: ExprID,
    },
    IfThenElse {
        ret_type: Arc<Type>,
        i: ExprID,
        t: ExprID,
        e: ExprID,
    },
    While {
        w: ExprID,
        d: ExprID,
    },
    Block {
        ret_type: Arc<Type>,
        stmts: Vec<ExprID>,
    },
}

pub enum GeneralReturnType {
    PrimInt,
    PrimFloat,
}

use GeneralReturnType::*;
use NodeData::*;

impl NodeData {
    pub fn gen_ret_type(&self) -> GeneralReturnType {
        if matches!(self, Number(_)) {
            return PrimInt;
        }

        if self.returns_primitive_int() {
            return PrimInt;
        } else if self.returns_primitive_float() {
            return PrimFloat;
        } else {
            todo!()
        }
    }

    fn returns_primitive_int(&self) -> bool {
        match &*self.ret_type().unwrap().name {
            "#prim::u8" | "#prim::u16" | "#prim::u32" | "#prim::u64" => true,
            _ => false,
        }
    }

    fn returns_primitive_float(&self) -> bool {
        todo!();
    }

    fn is_primitive(&self) -> bool {
        &self.ret_type().unwrap().name[0..5] == "#prim"
    }

    pub fn ret_type(&self) -> Option<Arc<Type>> {
        match self {
            Number(_) | Strin(_) | GotoMarker(_) | Goto(_) | Block { .. } => None,
            Ident { var_type, .. } | VarDecl { var_type, .. } => Some(var_type.clone()),
            BinOp { ret_type, .. }
            | IfThen { ret_type, .. }
            | IfThenElse { ret_type, .. }
            | Block { ret_type, .. } => Some(ret_type.clone()),
            _ => unreachable!(),
        }
    }
}
