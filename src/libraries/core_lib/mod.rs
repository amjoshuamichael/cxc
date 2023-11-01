use crate::{Type, CompData, FuncType, TypeEnum, typ::ABI};

use super::Library;

pub struct CoreLib;

impl Library for CoreLib {
    fn add_to_unit(&self, unit: &mut crate::Unit) {
        unit.add_type_level_func("ExternRust".into(), extern_rust);
    }
}

fn extern_rust(input: Vec<Type>, _: &CompData) -> Type {
    assert_eq!(input.len(), 1);

    input[0].clone().modify_type_data(|type_data| {
        let TypeEnum::Func(FuncType { abi: ref mut abi_style, .. }) = type_data.type_enum
            else { panic!() };
        *abi_style = ABI::Rust;
    })
}
