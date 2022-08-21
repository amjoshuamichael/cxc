use super::prelude::*;
use crate::parse::*;
use num_bigint::BigInt;
use std::fmt::{Debug, Formatter};
use std::sync::Arc;

#[derive(Default)]
pub struct ExprTree {
    nodes: Vec<ExprNode>,
}

impl ExprTree {
    pub fn top_down_iter<'a>(
        &'a mut self,
    ) -> Box<dyn DoubleEndedIterator<Item = (ExprID, &mut NodeData)> + 'a> {
        Box::new(
            self.nodes
                .iter_mut()
                .enumerate()
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
            writeln!(fmt, "{e}: {expr:?}");
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
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), std::fmt::Error> {
        match &self.data {
            Empty => write!(fmt, "Empty"),
            Number { value, .. } => write!(fmt, "{value}"),
            Float { value, .. } => write!(fmt, "{value:?}"),
            Strin(s) => write!(fmt, "{s}"),
            StructLit {
                var_type: struct_type,
                fields,
                ..
            } => {
                write!(fmt, "{struct_type:?} {{ {fields:?} }}")
            },
            Call { f, a, .. } => write!(fmt, "{f:?}({a:?})"),
            Ident { name, .. } => write!(fmt, "{name}"),
            Global { name, .. } => write!(fmt, "{name}"),
            MakeVar {
                var_type,
                name,
                rhs,
                ..
            } => write!(fmt, "{name}: {var_type:?} = {rhs:?}"),
            SetVar { lhs, rhs, .. } => write!(fmt, "{lhs:?} = {rhs:?}"),
            Member { object, field, .. } => write!(fmt, "{object:?}.{field}"),
            UnarOp { op, hs, .. } => write!(fmt, "{op:?} {hs:?}"),
            BinOp { lhs, op, rhs, .. } => write!(fmt, "{lhs:?} {op:?} {rhs:?}"),
            IfThen { i, t, .. } => write!(fmt, "? {i:?} {t:?}"),
            IfThenElse { i, t, e, .. } => {
                write!(fmt, "? {i:?} {t:?} : {e:?}")
            },
            While { w, d, .. } => write!(fmt, "@ {w:?} {d:?}"),
            Block { stmts, .. } => write!(fmt, "{{{stmts:?}}}"),
        };

        writeln!(fmt, " :: {:?}", self.data.ret_type())
    }
}

#[derive(Clone, Debug)]
pub enum NodeData {
    Empty,
    Number {
        value: BigInt,
        size: u32,
    },
    Float {
        value: f64,
        size: u32,
    },
    StructLit {
        var_type: Type,
        fields: Vec<(String, ExprID)>,
    },
    Strin(String),
    Ident {
        var_type: Type,
        name: Arc<str>,
    },
    Global {
        var_type: Type,
        name: String,
    },
    MakeVar {
        type_spec: Option<TypeAlias>,
        var_type: Type,
        name: Arc<str>,
        rhs: ExprID,
    },
    SetVar {
        ret_type: Type,
        lhs: ExprID,
        rhs: ExprID,
    },
    Call {
        ret_type: Type,
        f: ExprID,
        a: Vec<ExprID>,
    },
    Member {
        ret_type: Type,
        object: ExprID,
        field: String,
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

use NodeData::*;

impl NodeData {
    pub fn ret_type(&self) -> Type {
        match self {
            Number { size, .. } => Type::int_of_size(*size),
            Float { size, .. } => Type::float_of_size(*size),
            StructLit {
                var_type: struct_type,
                ..
            } => struct_type.clone(),
            Strin(_) => todo!(),
            Ident { var_type, .. }
            | MakeVar { var_type, .. }
            | Global { var_type, .. } => var_type.clone(),
            BinOp { ret_type, .. }
            | UnarOp { ret_type, .. }
            | IfThen { ret_type, .. }
            | IfThenElse { ret_type, .. }
            | SetVar { ret_type, .. }
            | Call { ret_type, .. }
            | Block { ret_type, .. }
            | Member { ret_type, .. } => ret_type.clone(),
            Empty => unreachable!(),
            While { .. } => Type::never(),
        }
    }
}
