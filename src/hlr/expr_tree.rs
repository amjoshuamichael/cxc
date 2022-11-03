use crate::lex::VarName;
use crate::parse::*;
use crate::typ::FloatType;
use crate::unit::UniqueFuncInfo;
use crate::Type;
use std::fmt::{Debug, Formatter};

#[derive(Default, Clone)]
pub struct ExprTree {
    nodes: Vec<ExprNode>,
}

impl ExprTree {
    pub fn iter_mut<'a>(
        &'a mut self,
    ) -> Box<dyn DoubleEndedIterator<Item = (ExprID, &mut NodeData)> + 'a> {
        Box::new(
            self.nodes
                .iter_mut()
                .enumerate()
                .map(|(id, node)| (ExprID(id), &mut node.data)),
        )
    }

    pub fn iter<'a>(
        &'a self,
    ) -> Box<dyn DoubleEndedIterator<Item = (ExprID, &NodeData)> + 'a> {
        Box::new(
            self.nodes
                .iter()
                .enumerate()
                .map(|(id, node)| (ExprID(id), &node.data)),
        )
    }

    pub fn ids(&self) -> impl DoubleEndedIterator<Item = ExprID> {
        (0..self.node_count()).map(ExprID)
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

    pub fn get(&self, at: ExprID) -> NodeData { self.nodes[at.0].data.clone() }

    pub fn parent(&self, of: ExprID) -> ExprID { self.nodes[of.0].parent }

    pub fn statement_and_block(&self, of: ExprID) -> (ExprID, ExprID) {
        let parent = self.parent(of);

        match self.get(parent) {
            Block { .. } => (of, parent),
            _ => self.statement_and_block(parent),
        }
    }

    fn node_count(&self) -> usize { self.nodes.len() }

    pub fn unique_func_info_of_call(&self, call: &NodeData) -> UniqueFuncInfo {
        let NodeData::Call { f, generics, a, is_method, return_by_ref, .. } = call.clone()
            else { panic!() };

        let method_of = if is_method {
            let object = a.last().unwrap();

            Some(self.get(*object).ret_type())
        } else {
            None
        };

        UniqueFuncInfo {
            name: f,
            method_of,
            generics,
        }
    }
}

impl Debug for ExprTree {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), std::fmt::Error> {
        for (e, expr) in self.nodes.iter().enumerate() {
            writeln!(fmt, "{e}: {expr:?}")?
        }

        Ok(())
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct ExprID(usize);

impl ToString for ExprID {
    fn to_string(&self) -> String { self.0.to_string() }
}

impl Debug for ExprID {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "ID_{}", self.0)
    }
}

impl ExprID {
    pub const ROOT: ExprID = ExprID(0);
}

#[derive(Clone)]
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
            Bool { value, .. } => write!(fmt, "{value:?}"),
            StructLit {
                var_type, fields, ..
            } => {
                write!(fmt, "{var_type:?} {{ {fields:?} }}")
            },
            ArrayLit { parts, .. } => {
                write!(fmt, "{parts:?}")
            },
            Call { f, a, .. } => write!(fmt, "{f:?}({a:?})"),
            Ident { name, .. } => write!(fmt, "{name}"),
            MakeVar {
                var_type,
                name,
                rhs,
                ..
            } => write!(fmt, "{name}: {var_type:?} = {rhs:?}"),
            SetVar { lhs, rhs, .. } => write!(fmt, "{lhs:?} = {rhs:?}"),
            Member { object, field, .. } => write!(fmt, "{object:?}.{field}"),
            Index { object, index, .. } => write!(fmt, "{object:?}[{index:?}]"),
            UnarOp { op, hs, .. } => write!(fmt, "{op:?} {hs:?}"),
            BinOp { lhs, op, rhs, .. } => write!(fmt, "{lhs:?} {op:?} {rhs:?}"),
            IfThen { i, t, .. } => write!(fmt, "? {i:?} {t:?}"),
            IfThenElse { i, t, e, .. } => {
                write!(fmt, "? {i:?} {t:?} : {e:?}")
            },
            While { w, d, .. } => write!(fmt, "@ {w:?} {d:?}"),
            Block { stmts, .. } => write!(fmt, "{{{stmts:?}}}"),
            Return { to_return, .. } => write!(fmt, "! {to_return:?}"),
        }?;

        write!(fmt, " :: {:?}", self.data.ret_type())
    }
}

// TODO: refactor Call to use UniqueFuncInfo
// TODO: combine SetVar and MakeVar
#[derive(Clone, Debug)]
pub enum NodeData {
    Empty,
    Number {
        value: u128,
        size: u32,
    },
    Float {
        value: f64,
        size: FloatType,
    },
    Bool {
        value: bool,
    },
    StructLit {
        var_type: Type,
        fields: Vec<(VarName, ExprID)>,
    },
    ArrayLit {
        var_type: Type,
        parts: Vec<ExprID>,
    },
    Ident {
        var_type: Type,
        name: VarName,
    },
    MakeVar {
        var_type: Type,
        name: VarName,
        rhs: ExprID,
    },
    SetVar {
        ret_type: Type,
        lhs: ExprID,
        rhs: ExprID,
    },
    Call {
        ret_type: Type,
        f: VarName,
        generics: Vec<Type>,
        a: Vec<ExprID>,
        is_method: bool,
        return_by_ref: bool,
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

use NodeData::*;

impl NodeData {
    pub fn ret_type(&self) -> Type {
        match self {
            Number { size, .. } => Type::i(*size),
            Float { size, .. } => Type::f(*size),
            Bool { .. } => Type::bool(),
            Ident { var_type, .. }
            | StructLit { var_type, .. }
            | ArrayLit { var_type, .. }
            | MakeVar { var_type, .. } => var_type.clone(),
            BinOp { ret_type, .. }
            | Return { ret_type, .. }
            | UnarOp { ret_type, .. }
            | IfThen { ret_type, .. }
            | IfThenElse { ret_type, .. }
            | SetVar { ret_type, .. }
            | Call { ret_type, .. }
            | Block { ret_type, .. }
            | Index { ret_type, .. }
            | Member { ret_type, .. } => ret_type.clone(),
            Empty => unreachable!(),
            While { .. } => Type::never(),
        }
    }
}
