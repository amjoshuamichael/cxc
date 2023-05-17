use crate::{
    hlr::hlr_data::FuncRep,
    lex::VarName,
    parse::{InitOpts, Opcode, TypeSpec},
    Type, UniqueFuncInfo,
};

use super::{ExprID, HNodeData};

impl<'a> FuncRep<'a> {
    pub fn insert_quick(&mut self, parent: ExprID, gen: impl NodeDataGen) -> ExprID {
        gen.add_to_expr_tree(self, parent)
    }

    pub fn replace_quick(&mut self, replacee: ExprID, gen: impl NodeDataGen) {
        let parent = self.tree.parent(replacee);
        let new = gen.add_to_expr_tree(self, parent);
        self.tree.replace(replacee, self.tree.get(new));
    }
}

pub trait NodeDataGen: std::fmt::Debug {
    fn add_to_expr_tree(&self, hlr: &mut FuncRep, parent: ExprID) -> ExprID;
}

impl NodeDataGen for Box<dyn NodeDataGen> {
    fn add_to_expr_tree(&self, hlr: &mut FuncRep, parent: ExprID) -> ExprID {
        self.as_ref().add_to_expr_tree(hlr, parent)
    }
}

impl NodeDataGen for HNodeData {
    fn add_to_expr_tree(&self, hlr: &mut FuncRep, parent: ExprID) -> ExprID {
        hlr.tree.insert(parent, self.clone())
    }
}

impl NodeDataGen for usize {
    fn add_to_expr_tree(&self, hlr: &mut FuncRep, parent: ExprID) -> ExprID {
        hlr.tree.insert(
            parent,
            HNodeData::Number {
                value: *self as u64,
                lit_type: Type::i(std::mem::size_of::<usize>() as u32),
            },
        )
    }
}

impl NodeDataGen for VarName {
    fn add_to_expr_tree(&self, hlr: &mut FuncRep, parent: ExprID) -> ExprID {
        hlr.tree.insert(
            parent,
            HNodeData::Ident {
                name: self.clone(),
                var_type: hlr.variables.get(self).unwrap().typ.clone(),
            },
        )
    }
}

impl Default for Box<dyn NodeDataGen> {
    fn default() -> Self {
        box HNodeData::Number {
            value: 0,
            lit_type: Type::i(32),
        }
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
            HNodeData::Set {
                lhs,
                rhs,
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
            HNodeData::BinOp {
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
            HNodeData::UnarOp {
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

        let func_type = hlr.comp_data.get_func_type(&self.info).unwrap();

        hlr.tree.replace(
            space,
            HNodeData::Call {
                f: self.info.name.clone(),
                generics: self.info.generics.clone(),
                relation: self.info.relation.clone(),
                ret_type: func_type.ret,
                a: args,
            },
        );

        space
    }
}

impl NodeDataGen for UniqueFuncInfo {
    fn add_to_expr_tree(&self, hlr: &mut FuncRep, parent: ExprID) -> ExprID {
        let func_type = hlr.comp_data.get_func_type(self).unwrap();

        hlr.tree.insert(
            parent,
            HNodeData::Call {
                f: self.name.clone(),
                generics: self.generics.clone(),
                relation: self.relation.clone(),
                ret_type: func_type.ret,
                a: Vec::new(),
            },
        )
    }
}

#[derive(Debug, Default)]
pub struct StructLitGen {
    pub var_type: Type,
    pub fields: Vec<(VarName, Box<dyn NodeDataGen>)>,
    pub initialize: InitOpts,
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
            HNodeData::StructLit {
                var_type: self.var_type.clone(),
                fields: added_fields,
                initialize: self.initialize,
            },
        );

        space
    }
}

impl StructLitGen {
    pub fn tuple(elems: Vec<Box<dyn NodeDataGen>>) -> StructLitGen {
        StructLitGen {
            fields: elems.into_iter()
                .enumerate()
                .map(|(i, e)| ((i as u32).into(), e))
                .collect(),
            ..Default::default()
        }
    }
}

#[derive(Debug, Default)]
pub struct ArrayLitGen {
    pub var_type: Type,
    pub parts: Vec<Box<dyn NodeDataGen>>,
    pub initialize: InitOpts,
}

impl NodeDataGen for ArrayLitGen {
    fn add_to_expr_tree(&self, hlr: &mut FuncRep, parent: ExprID) -> ExprID {
        let space = hlr.tree.make_one_space(parent);

        let parts = self
            .parts
            .iter()
            .map(|data| data.add_to_expr_tree(hlr, space))
            .collect();

        hlr.tree.replace(
            space,
            HNodeData::ArrayLit {
                var_type: self.var_type.clone(),
                parts,
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
}

impl NodeDataGen for MemberGen {
    fn add_to_expr_tree(&self, hlr: &mut FuncRep, parent: ExprID) -> ExprID {
        let space = hlr.tree.make_one_space(parent);

        let object = self.object.add_to_expr_tree(hlr, space);
        let object_type = hlr.tree.get(object).ret_type();
        let member_type = 
            hlr.get_type_spec(&TypeSpec::StructMember(box object_type.into(), self.field.clone()))
                .unwrap();

        hlr.tree.replace(
            space,
            HNodeData::Member {
                object,
                field: self.field.clone(),
                ret_type: member_type,

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
            HNodeData::Index {
                object,
                index,
                ret_type: self.ret_type.clone(),
            },
        );

        space
    }
}

#[derive(Debug, Default)]
pub struct RefGen {
    pub object: Box<dyn NodeDataGen>,
}

impl NodeDataGen for RefGen {
    fn add_to_expr_tree(&self, hlr: &mut FuncRep, parent: ExprID) -> ExprID {
        let space = hlr.tree.make_one_space(parent);

        let object = self.object.add_to_expr_tree(hlr, space);
        let obj_typ = hlr.tree.get(object).ret_type();
        let obj_typ_ref = obj_typ.get_ref();
        
        hlr.tree.replace(
            space, 
            HNodeData::UnarOp {
                ret_type: obj_typ_ref,
                op: Opcode::Ref,
                hs: object,
            }
        );

        space
    }
}

pub fn get_ref<T: NodeDataGen + 'static>(node_data_gen: T) -> Box<RefGen> {
    Box::new(RefGen {
        object: box node_data_gen,
    })
}

#[derive(Debug, Default)]
pub struct DerefGen<T: NodeDataGen> {
    pub object: T,
}

impl<T: NodeDataGen> NodeDataGen for DerefGen<T> {
    fn add_to_expr_tree(&self, hlr: &mut FuncRep, parent: ExprID) -> ExprID {
        let space = hlr.tree.make_one_space(parent);

        let object = self.object.add_to_expr_tree(hlr, space);
        let obj_typ = hlr.tree.get(object).ret_type();
        let obj_typ_deref = obj_typ.get_deref().unwrap();

        hlr.tree.replace(
            space,
            HNodeData::UnarOp {
                ret_type: obj_typ_deref,
                op: Opcode::Deref,
                hs: object,
            }
        );

        space
    }
}

pub fn get_deref<T: NodeDataGen + 'static>(node_data_gen: T) -> Box<DerefGen<T>> {
    Box::new(DerefGen {
        object: node_data_gen,
    })
}
