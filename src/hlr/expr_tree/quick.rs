use crate::{
    hlr::hlr_data::FuncRep,
    lex::VarName,
    parse::{InitOpts, Opcode, TypeSpec, TypeRelationGeneric},
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

impl<T: NodeDataGen> NodeDataGen for Box<T> {
    fn add_to_expr_tree(&self, hlr: &mut FuncRep, parent: ExprID) -> ExprID {
        self.as_ref().add_to_expr_tree(hlr, parent)
    }
}

impl NodeDataGen for HNodeData {
    fn add_to_expr_tree(&self, hlr: &mut FuncRep, parent: ExprID) -> ExprID {
        hlr.tree.insert(parent, self.clone())
    }
}

impl NodeDataGen for ExprID {
    fn add_to_expr_tree(&self, hlr: &mut FuncRep, parent: ExprID) -> ExprID {
        hlr.tree.set_parent(*self, parent);
        *self
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

#[derive(Debug, Default)]
pub struct SetVarGen<T: NodeDataGen, U: NodeDataGen> {
    pub lhs: T,
    pub rhs: U,
}

impl<T: NodeDataGen, U: NodeDataGen> NodeDataGen for SetVarGen<T, U> {
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

#[derive(Debug)]
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
pub struct UnarOpGen<T: NodeDataGen> {
    pub op: Opcode,
    pub hs: T,
    pub ret_type: Type,
}

impl<T: NodeDataGen> NodeDataGen for UnarOpGen<T> {
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
    pub sret: Option<Box<dyn NodeDataGen>>,
}

impl NodeDataGen for CallGen {
    fn add_to_expr_tree(&self, hlr: &mut FuncRep, parent: ExprID) -> ExprID {
        let space = hlr.tree.make_one_space(parent);

        let args = self
            .args
            .iter()
            .map(|arg| arg.add_to_expr_tree(hlr, space))
            .collect();

        let sret = self.sret.as_ref().map(|sret| sret.add_to_expr_tree(hlr, space));

        let func_type = hlr.comp_data.get_func_type(&self.info).unwrap();

        hlr.tree.replace(
            space,
            HNodeData::Call {
                f: self.info.name.clone(),
                generics: self.info.generics.clone(),
                relation: self.info.relation.clone(),
                ret_type: func_type.ret,
                a: args,
                sret,
            },
        );

        space
    }
}

// TODO: is this nescessary
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
                sret: None,
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
                .map(|(i, e)| (VarName::TupleIndex(i), e))
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
pub struct MemberGen<T: NodeDataGen> {
    pub object: T,
    pub field: VarName,
}

impl<T: NodeDataGen> NodeDataGen for MemberGen<T> {
    fn add_to_expr_tree(&self, hlr: &mut FuncRep, parent: ExprID) -> ExprID {
        let space = hlr.tree.make_one_space(parent);

        let object = self.object.add_to_expr_tree(hlr, space);
        let object_type = hlr.tree.get(object).ret_type();
        let member_type = 
            hlr.get_type_spec(&TypeSpec::StructMember(
                    Box::new(object_type.into()),
                    self.field.clone()
                ))
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

#[derive(Debug)]
pub struct IndexGen<T: NodeDataGen, U: NodeDataGen> {
    pub object: T,
    pub index: U,
    pub ret_type: Type,
}

impl<T: NodeDataGen, U: NodeDataGen> NodeDataGen for IndexGen<T, U> {
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
pub struct RefGen<T: NodeDataGen> {
    pub object: T,
}

impl<T: NodeDataGen> NodeDataGen for RefGen<T> {
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

pub fn get_ref<T: NodeDataGen + 'static>(node_data_gen: T) -> Box<RefGen<T>> {
    Box::new(RefGen {
        object: node_data_gen,
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

#[derive(Debug, Default)]
pub struct MemCpyGen<T: NodeDataGen, U: NodeDataGen, V: NodeDataGen> {
    pub from: T,
    pub to: U,
    pub size: V,
}

impl<T: NodeDataGen, U: NodeDataGen, V: NodeDataGen> NodeDataGen for MemCpyGen<T, U, V> {
    fn add_to_expr_tree(&self, hlr: &mut FuncRep, parent: ExprID) -> ExprID {
        let space = hlr.tree.make_one_space(parent);

        let from = self.from.add_to_expr_tree(hlr, space);
        let to = self.to.add_to_expr_tree(hlr, space);
        let size = self.size.add_to_expr_tree(hlr, space);

        let generics = vec![
            hlr.tree.get_ref(from).ret_type().get_deref().unwrap(),
            hlr.tree.get_ref(to).ret_type().get_deref().unwrap(),
        ];

        hlr.tree.replace(
            space,
            HNodeData::Call {
                f: "memcpy".into(),
                relation: TypeRelationGeneric::Unrelated,
                generics,
                ret_type: Type::void(),
                a: vec![from, to, size],
                sret: None,
            }
        );

        space
    }
}
