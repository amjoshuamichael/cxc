use crate::{parse::Opcode, TypeEnum, TypeRelation, VarName};

use super::{expr_tree::NodeData, hlr_data::FuncRep};

pub fn op_overloading(hlr: &mut FuncRep) {
    hlr.modify_many_rev(
        |data| matches!(data, NodeData::UnarOp { .. }),
        |op_id, op_data, hlr| {
            let NodeData::UnarOp { hs, op, ret_type } = op_data.clone() 
                else { unreachable!() };
            let hs_type = hlr.tree.get(hs).ret_type();

            match op {
                Opcode::Deref if !matches!(hs_type.as_type_enum(), TypeEnum::Ref(_)) => {
                    let reffed_hs = hlr.insert_quick(
                        op_id,
                        NodeData::UnarOp {
                            op: Opcode::Ref,
                            hs,
                            ret_type: hs_type.get_ref(),
                        },
                    );
                    *op_data = NodeData::Call {
                        ret_type: ret_type.clone(),
                        f: VarName::from("deref"),
                        generics: hs_type.generics().clone(),
                        a: vec![reffed_hs],
                        relation: TypeRelation::MethodOf(hs_type.clone().get_ref()),
                    };
                },
                _ => {},
            }
        },
    );

    // TODO: binary operator overloading
}
