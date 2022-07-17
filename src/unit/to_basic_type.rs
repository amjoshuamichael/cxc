use crate::hlr::prelude::*;
use inkwell::context::Context;
use inkwell::types::*;
use inkwell::AddressSpace;

pub fn to_basic_type<'t>(context: &'t Context, typ: &Type) -> BasicTypeEnum<'t> {
    match typ.gen_ret_type() {
        PrimInt => context.i32_type().into(),
        PrimFloat => context.f32_type().into(),
        PrimRef => {
            let mut pointed_to_type = typ.clone();
            pointed_to_type.ref_count = 0;
            to_basic_type(context, &pointed_to_type)
                .ptr_type(AddressSpace::Global)
                .into()
        },
        _ => todo!(),
    }
}
