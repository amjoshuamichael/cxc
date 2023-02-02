use crate::{
    hlr::hlr_data::{DataFlowInfo, FuncRep},
    lex::VarName,
    parse::{Opcode, StructFill},
    FuncType, Type, TypeEnum, UniqueFuncInfo,
};

use super::{ExprID, NodeData};

impl FuncRep {
    pub fn insert_quick(&mut self, parent: ExprID, gen: impl NodeDataGen) -> ExprID {
        gen.add_to_expr_tree(self, parent)
    }
}

pub trait NodeDataGen: std::fmt::Debug {
    fn add_to_expr_tree(&self, hlr: &mut FuncRep, parent: ExprID) -> ExprID;
}

impl NodeDataGen for NodeData {
    fn add_to_expr_tree(&self, hlr: &mut FuncRep, parent: ExprID) -> ExprID {
        hlr.tree.insert(parent, self.clone())
    }
}

impl NodeDataGen for usize {
    fn add_to_expr_tree(&self, hlr: &mut FuncRep, parent: ExprID) -> ExprID {
        hlr.tree.insert(
            parent,
            NodeData::Number {
                value: *self as u64,
                size: std::mem::size_of::<usize>() as u32,
            },
        )
    }
}

impl Default for Box<dyn NodeDataGen> {
    fn default() -> Self { box NodeData::Number { value: 0, size: 32 } }
}

#[derive(Debug, Default)]
pub struct MakeVarGen {
    pub set: VarName,
    pub arg_index: Option<u32>,
    pub to: Box<dyn NodeDataGen>,
    pub var_type: Type,
}

impl NodeDataGen for MakeVarGen {
    fn add_to_expr_tree(&self, hlr: &mut FuncRep, parent: ExprID) -> ExprID {
        let space = hlr.tree.make_one_space(parent);

        let rhs = self.to.add_to_expr_tree(hlr, space);

        hlr.data_flow.insert(
            self.set.clone(),
            DataFlowInfo {
                typ: self.var_type.clone(),
                arg_index: self.arg_index,
            },
        );

        hlr.tree.replace(
            space,
            NodeData::MakeVar {
                name: self.set.clone(),
                var_type: self.var_type.clone(),
                rhs,
            },
        );

        space
    }
}

#[derive(Debug, Default)]
pub struct SetVarGen {
    pub lhs: Box<dyn NodeDataGen>,
    pub rhs: Box<dyn NodeDataGen>,
}

impl NodeDataGen for SetVarGen {
    fn add_to_expr_tree(&self, hlr: &mut FuncRep, parent: ExprID) -> ExprID {
        let space = hlr.tree.make_one_space(parent);

        let lhs = self.lhs.add_to_expr_tree(hlr, space);
        let rhs = self.rhs.add_to_expr_tree(hlr, space);

        hlr.tree.replace(
            space,
            NodeData::Set {
                lhs: lhs.clone(),
                rhs: rhs.clone(),
                ret_type: hlr.tree.get_ref(lhs).ret_type(),
            },
        );

        space
    }
}

#[derive(Debug, Default)]
pub struct BinOpGen {
    pub lhs: Box<dyn NodeDataGen>,
    pub op: Opcode,
    pub rhs: Box<dyn NodeDataGen>,
    pub ret_type: Type,
}

impl NodeDataGen for BinOpGen {
    fn add_to_expr_tree(&self, hlr: &mut FuncRep, parent: ExprID) -> ExprID {
        let space = hlr.tree.make_one_space(parent);

        let lhs = self.lhs.add_to_expr_tree(hlr, space);
        let rhs = self.rhs.add_to_expr_tree(hlr, space);

        hlr.tree.replace(
            space,
            NodeData::BinOp {
                lhs,
                rhs,
                op: self.op,
                ret_type: self.ret_type.clone(),
            },
        );

        space
    }
}

#[derive(Debug, Default)]
pub struct UnarOpGen {
    pub op: Opcode,
    pub hs: Box<dyn NodeDataGen>,
    pub ret_type: Type,
}

impl NodeDataGen for UnarOpGen {
    fn add_to_expr_tree(&self, hlr: &mut FuncRep, parent: ExprID) -> ExprID {
        let space = hlr.tree.make_one_space(parent);

        let hs = self.hs.add_to_expr_tree(hlr, space);

        hlr.tree.replace(
            space,
            NodeData::UnarOp {
                op: self.op,
                hs,
                ret_type: self.ret_type.clone(),
            },
        );

        space
    }
}

#[derive(Debug, Default)]
pub struct CallGen {
    pub info: UniqueFuncInfo,
    pub args: Vec<Box<dyn NodeDataGen>>,
}

impl NodeDataGen for CallGen {
    fn add_to_expr_tree(&self, hlr: &mut FuncRep, parent: ExprID) -> ExprID {
        let space = hlr.tree.make_one_space(parent);

        let args = self
            .args
            .iter()
            .map(|gen| gen.add_to_expr_tree(hlr, space))
            .collect();

        let func_type = hlr.comp_data.get_type(&self.info).unwrap();
        let TypeEnum::Func(FuncType { ret_type, .. } ) = func_type.as_type_enum()
            else { panic!() };

        hlr.tree.replace(
            space,
            NodeData::Call {
                f: self.info.name.clone(),
                generics: self.info.own_generics.clone(),
                relation: self.info.relation.clone(),
                ret_type: ret_type.clone(),
                a: args,
            },
        );

        space
    }
}

#[derive(Debug, Default)]
pub struct StructLitGen {
    pub var_type: Type,
    pub fields: Vec<(VarName, Box<dyn NodeDataGen>)>,
    pub initialize: StructFill,
}

impl NodeDataGen for StructLitGen {
    fn add_to_expr_tree(&self, hlr: &mut FuncRep, parent: ExprID) -> ExprID {
        let space = hlr.tree.make_one_space(parent);

        let added_fields = self
            .fields
            .iter()
            .map(|(name, data)| (name.clone(), data.add_to_expr_tree(hlr, space)))
            .collect();

        hlr.tree.replace(
            space,
            NodeData::StructLit {
                var_type: self.var_type.clone(),
                fields: added_fields,
                initialize: self.initialize,
            },
        );

        space
    }
}

#[derive(Debug, Default)]
pub struct MemberGen {
    pub object: Box<dyn NodeDataGen>,
    pub field: VarName,
    pub ret_type: Type,
}

impl NodeDataGen for MemberGen {
    fn add_to_expr_tree(&self, hlr: &mut FuncRep, parent: ExprID) -> ExprID {
        let space = hlr.tree.make_one_space(parent);

        let object = self.object.add_to_expr_tree(hlr, space);

        hlr.tree.replace(
            space,
            NodeData::Member {
                object,
                field: self.field.clone(),
                ret_type: self.ret_type.clone(),
            },
        );

        space
    }
}

#[derive(Debug, Default)]
pub struct IndexGen {
    pub object: Box<dyn NodeDataGen>,
    pub index: Box<dyn NodeDataGen>,
    pub ret_type: Type,
}

impl NodeDataGen for IndexGen {
    fn add_to_expr_tree(&self, hlr: &mut FuncRep, parent: ExprID) -> ExprID {
        let space = hlr.tree.make_one_space(parent);

        let object = self.object.add_to_expr_tree(hlr, space);
        let index = self.index.add_to_expr_tree(hlr, space);

        hlr.tree.replace(
            space,
            NodeData::Index {
                object,
                index,
                ret_type: self.ret_type.clone(),
            },
        );

        space
    }
}
