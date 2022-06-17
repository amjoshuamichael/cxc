use super::Type;
use crate::parse::Opcode;
use core::num::NonZeroUsize;
use num_bigint::BigInt;
use std::sync::Arc;

#[derive(Default)]
pub struct ExprTree {
    nodes: Vec<ExprNode>,
}

impl ExprTree {
    pub fn empty() -> ExprTree {
        ExprTree::default()
    }

    pub fn insert(&mut self, parent: ExprID, data: NodeData) -> ExprID {
        self.nodes.push(ExprNode { parent, data });
        ExprID(self.nodes.len() - 1)
    }

    pub fn replace_data(&mut self, at: ExprID, with: NodeData) {
        self.nodes[at.0].data = with;
    }

    pub fn make_one_space(&mut self, parent: ExprID) -> ExprID {
        self.nodes.push(ExprNode {
            parent,
            data: NodeData::None,
        });
        ExprID(self.nodes.len() - 1)
    }
}

#[derive(Copy, Clone)]
pub struct ExprID(usize);

impl ExprID {
    pub const ROOT: ExprID = ExprID(0);
}

struct ExprNode {
    parent: ExprID,
    data: NodeData,
}

pub enum NodeData {
    None,
    Number(BigInt),
    Strin(String),
    Ident(Arc<str>),
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
    GotoMarker(Arc<str>),
    Goto(Arc<str>),
}
