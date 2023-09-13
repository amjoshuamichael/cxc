use crate::{parse::Opcode, TypeEnum, TypeRelation, VarName, FuncQuery};

use super::{expr_tree::HNodeData, hlr_data::FuncRep};

#[cfg_attr(debug_assertions, inline(never))]
pub fn op_overloading(hlr: &mut FuncRep) {
    hlr.modify_many_infallible(
        |op_id, op_data, hlr| {
            let HNodeData::UnarOp { hs, op, ret_type } = op_data.clone() 
                else { return };
            let hs_type = hlr.tree.get(hs).ret_type();

            match op {
                Opcode::Deref if !matches!(hs_type.as_type_enum(), TypeEnum::Ref(_)) => {
                    let reffed_hs = hlr.insert_quick(
                        op_id,
                        HNodeData::UnarOp {
                            op: Opcode::Ref,
                            hs,
                            ret_type: hs_type.get_ref(),
                        },
                    );
                    *op_data = HNodeData::Call {
                        ret_type: ret_type.clone(),
                        query: FuncQuery {
                            name: VarName::from("deref"),
                            generics: hs_type.generics().clone(),
                            relation: TypeRelation::MethodOf(hs_type.clone().get_ref()),
                        },
                        a: vec![reffed_hs],
                        sret: None,
                    };
                },
                _ => {},
            };
        },
    );

    // TODO: binary operator overloading
}
