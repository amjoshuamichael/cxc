use std::rc::Rc;

use crate::{
    hlr::{hlr_data::{FuncRep, VarID}, do_transformations::desugar_transformation},
    lex::VarName,
    parse::{InitOpts, Opcode, TypeSpec, TypeRelationGeneric},
    Type, FuncQuery, typ::{spec_from_type::type_to_type_spec, can_transform::TransformationList}, TypeRelation, ArrayType, TypeEnum, Field,
};

use super::{ExprID, HNodeData};

impl<'a> FuncRep<'a> {
    pub fn insert_quick(&mut self, parent: ExprID, gen: impl NodeDataGen) -> ExprID {
        gen.add_to_expr_tree(self, parent)
    }

    pub fn replace_quick(&mut self, replacee: ExprID, gen: impl NodeDataGen) {
        gen.put_in_id(self, replacee);
    }
}

pub trait NodeDataGen: std::fmt::Debug + std::any::Any {
    fn add_to_expr_tree(&self, hlr: &mut FuncRep, parent: ExprID) -> ExprID {
        let new_spot = hlr.tree.make_one_space(parent);
        self.put_in_id(hlr, new_spot);
        new_spot
    }
    fn put_in_id(&self, hlr: &mut FuncRep, spot: ExprID);
}

impl NodeDataGen for Box<dyn NodeDataGen> {
    fn add_to_expr_tree(&self, hlr: &mut FuncRep, parent: ExprID) -> ExprID {
        self.as_ref().add_to_expr_tree(hlr, parent)
    }

    fn put_in_id(&self, hlr: &mut FuncRep, spot: ExprID) {
        self.as_ref().put_in_id(hlr, spot)
    }
}

impl NodeDataGen for Rc<dyn NodeDataGen> {
    fn put_in_id(&self, hlr: &mut FuncRep, spot: ExprID) {
        self.as_ref().put_in_id(hlr, spot)
    }
}

impl<T: NodeDataGen> NodeDataGen for Box<T> {
    fn put_in_id(&self, hlr: &mut FuncRep, spot: ExprID) {
        let inner: &T = &**self;
        inner.put_in_id(hlr, spot)
    }
}

impl NodeDataGen for HNodeData {
    fn put_in_id(&self, hlr: &mut FuncRep, spot: ExprID) {
        use HNodeData::*;
        let mut data = self.clone();
        match &mut data {
            Number { .. } | 
            Float { .. } | 
            Bool { .. } | 
            GlobalLoad { .. } | 
            AccessAlias(_) | 
            GotoLabel(_) | 
            Goto(_) |
            Ident { .. } |
            Return { to_return: None, .. } => {},
            StructLit { fields, .. } => {
                for (_, field_id) in fields {
                    *field_id = hlr.tree.get(*field_id).add_to_expr_tree(hlr, spot);
                }
            },
            ArrayLit { parts: many, .. } |
            Block { stmts: many, .. } => {
                for id in many {
                    *id = hlr.tree.get(*id).add_to_expr_tree(hlr, spot);
                }
            },
            Call { a, sret, .. } | IndirectCall { a, sret, .. } => {
                for a in a {
                    *a = hlr.tree.get(*a).add_to_expr_tree(hlr, spot);
                }
                if let Some(sret) = sret {
                    *sret = hlr.tree.get(*sret).add_to_expr_tree(hlr, spot);
                }
            },
            UnarOp { hs: x, .. } | 
            Transform { hs: x, .. } | 
            Member { object: x, .. } |
            Return { to_return: Some(x), .. } => {
                *x = hlr.tree.get(*x).add_to_expr_tree(hlr, spot);
            }
            Set { lhs: l, rhs: r } |
            BinOp { lhs: l, rhs: r, .. } | 
            Set { lhs: l, rhs: r, .. } | 
            While { w: l, d: r, .. } | 
            Index { object: l, index: r, .. } => {
                *l = hlr.tree.get(*l).add_to_expr_tree(hlr, spot);
                *r = hlr.tree.get(*r).add_to_expr_tree(hlr, spot);
            }
            IfThenElse { i, t, e, .. } => {
                *i = hlr.tree.get(*i).add_to_expr_tree(hlr, spot);
                *t = hlr.tree.get(*t).add_to_expr_tree(hlr, spot);
                *e = hlr.tree.get(*e).add_to_expr_tree(hlr, spot);
            },
        }

        hlr.tree.replace(spot, data);
    }
}

impl NodeDataGen for VarID {
    fn put_in_id(&self, hlr: &mut FuncRep, spot: ExprID) {
        hlr.tree.replace(
            spot,
            HNodeData::Ident {
                var_id: *self,
                var_type: hlr.variables.get(*self).unwrap().typ.clone(),
            },
        )
    }
}

#[derive(Debug, Default)]
pub struct SetGen<T: NodeDataGen, U: NodeDataGen> {
    pub lhs: T,
    pub rhs: U,
}

impl<T: NodeDataGen, U: NodeDataGen> NodeDataGen for SetGen<T, U> {
    fn put_in_id(&self, hlr: &mut FuncRep, spot: ExprID) {
        let lhs = self.lhs.add_to_expr_tree(hlr, spot);
        let rhs = self.rhs.add_to_expr_tree(hlr, spot);

        hlr.tree.replace(
            spot,
            HNodeData::Set {
                lhs,
                rhs,
            },
        );
    }
}

#[derive(Debug)]
pub struct BinOpGen {
    pub lhs: Box<dyn NodeDataGen>,
    pub op: Opcode,
    pub rhs: Box<dyn NodeDataGen>,
}

impl NodeDataGen for BinOpGen {
    fn put_in_id(&self, hlr: &mut FuncRep, spot: ExprID) {
        let lhs = self.lhs.add_to_expr_tree(hlr, spot);
        let rhs = self.rhs.add_to_expr_tree(hlr, spot);

        hlr.tree.replace(
            spot,
            HNodeData::BinOp {
                lhs,
                rhs,
                op: self.op,
                ret_type: hlr.tree.get_ref(lhs).ret_type(),
            },
        );
    }
}

#[derive(Debug, Default)]
pub struct UnarOpGen<T: NodeDataGen> {
    pub op: Opcode,
    pub hs: T,
    pub ret_type: Type,
}

impl<T: NodeDataGen> NodeDataGen for UnarOpGen<T> {
    fn put_in_id(&self, hlr: &mut FuncRep, spot: ExprID) {
        let hs = self.hs.add_to_expr_tree(hlr, spot);

        hlr.tree.replace(
            spot,
            HNodeData::UnarOp {
                op: self.op,
                hs,
                ret_type: self.ret_type.clone(),
            },
        );
    }
}

#[derive(Debug)]
pub struct CallGen {
    pub query: FuncQuery,
    pub args: Vec<Box<dyn NodeDataGen>>,
    pub sret: Option<Box<dyn NodeDataGen>>,
    pub infer_types: bool,
}

impl Default for CallGen {
    fn default() -> Self {
        CallGen {
            query: FuncQuery::default(),
            args: Vec::default(),
            sret: None,
            infer_types: true,
        }
    }
}

impl NodeDataGen for CallGen {
    fn put_in_id(&self, hlr: &mut FuncRep, spot: ExprID) {
        let args = self
            .args
            .iter()
            .map(|arg| arg.add_to_expr_tree(hlr, spot))
            .collect();

        let sret = self.sret.as_ref().map(|sret| sret.add_to_expr_tree(hlr, spot));

        let ret_type = if self.infer_types {
            hlr.comp_data.get_func_type(&self.query).unwrap().ret
        } else {
            Type::unknown()
        };

        hlr.tree.replace(
            spot,
            HNodeData::Call {
                query: self.query.clone(),
                ret_type,
                a: args,
                sret,
            },
        );
    }
}

#[derive(Debug)]
pub struct CastGen<T: NodeDataGen> {
    pub cast: T,
    pub to: Type,
}

impl<T: NodeDataGen> NodeDataGen for CastGen<T> {
    fn put_in_id(&self, hlr: &mut FuncRep, spot: ExprID) {
        let cast = self.cast.add_to_expr_tree(hlr, spot);
        let from_type = hlr.tree.get_ref(cast).ret_type();

        hlr.tree.replace(
            spot,
            HNodeData::Call {
                query: FuncQuery {
                    name: "cast".into(),
                    relation: TypeRelation::Unrelated,
                    generics: vec![from_type, self.to.clone()],
                },
                ret_type: self.to.clone(),
                a: vec![cast],
                sret: None,
            },
        );
    }
}

#[derive(Debug)]
pub struct WhileGen<T: NodeDataGen, U: NodeDataGen> {
    pub(crate) w: T,
    pub(crate) d: U,
}

impl<T: NodeDataGen, U: NodeDataGen> NodeDataGen for WhileGen<T, U> {
    fn put_in_id(&self, hlr: &mut FuncRep, spot: ExprID) {
        let w = self.w.add_to_expr_tree(hlr, spot);
        let d = self.d.add_to_expr_tree(hlr, spot);
        hlr.tree.replace(spot, HNodeData::While { w, d })
    }
}

impl NodeDataGen for Vec<Box<dyn NodeDataGen>> {
    fn put_in_id(&self, hlr: &mut FuncRep, spot: ExprID) {
        let new_stmts = self.iter().map(|gen| gen.add_to_expr_tree(hlr, spot)).collect();
        let mut block = HNodeData::new_block();
        let HNodeData::Block { ref mut stmts, .. } = &mut block else { unreachable!() };
        *stmts = new_stmts;
        hlr.tree.replace(spot, block)
    }
}

#[derive(Debug, Default)]
pub struct StructLitGen {
    pub var_type: Type,
    pub fields: Vec<(VarName, Box<dyn NodeDataGen>)>,
    pub initialize: InitOpts,
}

impl NodeDataGen for StructLitGen {
    fn put_in_id(&self, hlr: &mut FuncRep, spot: ExprID) {
        let added_fields = self
            .fields
            .iter()
            .map(|(name, data)| (name.clone(), data.add_to_expr_tree(hlr, spot)))
            .collect::<Vec<_>>();

        let var_type = if self.var_type.is_unknown() {
            Type::new_struct(
                added_fields.iter().map(|(name, id)| Field { 
                    name: name.clone(), 
                    typ: hlr.tree.get_ref(*id).ret_type(),
                    inherited: false,
                }).collect::<Vec<_>>()
            )
        } else {
            self.var_type.clone()
        };

        hlr.tree.replace(
            spot,
            HNodeData::StructLit {
                var_type,
                fields: added_fields,
                initialize: self.initialize,
            },
        );
    }
}

#[derive(Debug, Default)]
pub struct MemberGen<T: NodeDataGen> {
    pub object: T,
    pub field: VarName,
}

impl<T: NodeDataGen> NodeDataGen for MemberGen<T> {
    fn put_in_id(&self, hlr: &mut FuncRep, spot: ExprID) {
        let object = self.object.add_to_expr_tree(hlr, spot);
        let object_type = hlr.tree.get(object).ret_type();
        let member_type = 
            hlr.get_type_spec(&TypeSpec::StructMember(
                    Box::new(type_to_type_spec(object_type)),
                    self.field.clone()
                ))
                .unwrap();

        hlr.tree.replace(
            spot,
            HNodeData::Member {
                object,
                field: self.field.clone(),
                ret_type: member_type,

            },
        );
    }
}

#[derive(Debug)]
pub struct IndexGen<T: NodeDataGen, U: NodeDataGen> {
    pub object: T,
    pub index: U,
}

impl<T: NodeDataGen, U: NodeDataGen> NodeDataGen for IndexGen<T, U> {
    fn put_in_id(&self, hlr: &mut FuncRep, spot: ExprID) {
        let object = self.object.add_to_expr_tree(hlr, spot);
        let object_type = hlr.tree.get_ref(object).ret_type();
        let TypeEnum::Array(ArrayType { base, .. }) = object_type.as_type_enum()
            else { unreachable!() };
        let index = self.index.add_to_expr_tree(hlr, spot);

        hlr.tree.replace(
            spot,
            HNodeData::Index {
                object,
                index,
                ret_type: base.clone(),
            },
        );
    }
}

#[derive(Debug)]
pub struct TransformationGen<T: NodeDataGen> {
    pub object: T,
    pub steps: TransformationList,
}

impl<T: NodeDataGen> NodeDataGen for TransformationGen<T> {
    fn put_in_id(&self, hlr: &mut FuncRep, spot: ExprID) {
        let hs = self.object.add_to_expr_tree(hlr, spot);

        let mut transform_data = HNodeData::Transform {
            hs,
            steps: Some(self.steps.clone()),
            ret_type: Type::unknown(),
        };

        desugar_transformation(spot, &mut transform_data, hlr);
    }
}

#[derive(Debug, Default)]
pub struct RefGen<T: NodeDataGen>(pub T);

impl<T: NodeDataGen> NodeDataGen for RefGen<T> {
    fn put_in_id(&self, hlr: &mut FuncRep, spot: ExprID) {
        let object = self.0.add_to_expr_tree(hlr, spot);
        let obj_typ = hlr.tree.get(object).ret_type();
        let obj_typ_ref = obj_typ.get_ref();
        
        hlr.tree.replace(
            spot, 
            HNodeData::UnarOp {
                ret_type: obj_typ_ref,
                op: Opcode::Ref,
                hs: object,
            }
        );
    }
}

#[derive(Debug, Default)]
pub struct DerefGen<T: NodeDataGen>(pub T);

impl<T: NodeDataGen> NodeDataGen for DerefGen<T> {
    fn put_in_id(&self, hlr: &mut FuncRep, spot: ExprID) {
        let object = self.0.add_to_expr_tree(hlr, spot);
        let obj_typ = hlr.tree.get(object).ret_type();
        let obj_typ_deref = obj_typ.get_deref().unwrap();

        hlr.tree.replace(
            spot,
            HNodeData::UnarOp {
                ret_type: obj_typ_deref,
                op: Opcode::Deref,
                hs: object,
            }
        );
    }
}

#[derive(Debug, Default)]
pub struct MemCpyGen<T: NodeDataGen, U: NodeDataGen, V: NodeDataGen> {
    pub from: T,
    pub to: U,
    pub size: V,
}

impl<T: NodeDataGen, U: NodeDataGen, V: NodeDataGen> NodeDataGen for MemCpyGen<T, U, V> {
    fn put_in_id(&self, hlr: &mut FuncRep, spot: ExprID) {
        let from = self.from.add_to_expr_tree(hlr, spot);
        let to = self.to.add_to_expr_tree(hlr, spot);
        let size = self.size.add_to_expr_tree(hlr, spot);

        let generics = vec![
            hlr.tree.get_ref(from).ret_type().get_deref().unwrap(),
            hlr.tree.get_ref(to).ret_type().get_deref().unwrap(),
        ];

        hlr.tree.replace(
            spot,
            HNodeData::Call {
                query: FuncQuery {
                    name: "memcpy".into(),
                    relation: TypeRelationGeneric::Unrelated,
                    generics,
                },
                ret_type: Type::void(),
                a: vec![from, to, size],
                sret: None,
            }
        );
    }
}

#[derive(Clone, Copy, Debug)]
pub struct GenSlot;
impl NodeDataGen for GenSlot {
    fn put_in_id(&self, hlr: &mut FuncRep, spot: ExprID) {
        let data_in_slot = hlr.gen_slot.last().unwrap().clone();
        hlr.replace_quick(spot, data_in_slot);
    }
}

#[derive(Clone, Copy, Debug)]
pub struct SetGenSlot<T: NodeDataGen + Clone, U: NodeDataGen> {
    pub set_to: T,
    pub then: U,
}
impl<T: NodeDataGen + Clone, U: NodeDataGen> NodeDataGen for SetGenSlot<T, U> {
    fn put_in_id(&self, hlr: &mut FuncRep, spot: ExprID) {
        let slot_data = self.set_to.clone();
        hlr.gen_slot.push(Rc::new(slot_data));
        self.then.put_in_id(hlr, spot);
        hlr.gen_slot.pop();
    }
}

impl NodeDataGen for ExprID {
    fn put_in_id(&self, hlr: &mut FuncRep, spot: ExprID) {
        hlr.tree.get(*self).put_in_id(hlr, spot);
    }
}

pub struct With<T: NodeDataGen, U: NodeDataGen> {
    pub generate: T,
    pub then: Box<dyn Fn(Box<dyn NodeDataGen>, Type) -> U>,
}

impl<T: NodeDataGen, U: NodeDataGen> std::fmt::Debug for With<T, U> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", &self.generate)
    }
}

impl<T: NodeDataGen, U: NodeDataGen> NodeDataGen for With<T, U> {
    fn put_in_id(&self, hlr: &mut FuncRep, spot: ExprID) {
        let object = self.generate.add_to_expr_tree(hlr, spot);
        let object_type = hlr.tree.get_ref(object).ret_type();
        
        let object_gen = Box::new(CopyID(object));
        (self.then)(object_gen, object_type).put_in_id(hlr, spot);
    }
}

#[derive(Clone, Copy, Debug)]
struct CopyID(ExprID);
impl NodeDataGen for CopyID {
    fn add_to_expr_tree(&self, hlr: &mut FuncRep, parent: ExprID) -> ExprID {
        self.0
    }
    fn put_in_id(&self, hlr: &mut FuncRep, spot: ExprID) {
        panic!();
    }
}
