use crate::{parse::FuncCode, unit::CompData, Type};

use crate::libraries::Library;

pub(super) struct ToStringLib;

impl Library for ToStringLib {
    fn add_to_unit(&self, unit: &mut crate::Unit) {
        unit.add_deriver("to_string".into(), derive_to_string);

        let string_type = unit.comp_data.get_by_name(&"String".into()).unwrap();

        unit.add_rust_func_explicit(
            "to_string",
            to_string::<i32> as *const usize,
            Type::never().func_with_args(vec![
                string_type.clone().get_ref(),
                Type::i(32).get_ref(),
            ]),
            Some(Type::i(32).get_ref()),
            Vec::new(),
        );
    }
}

pub fn derive_to_string(_: &CompData, typ: Type) -> Option<FuncCode> {
    match typ.get_deref().unwrap().clone().as_type_enum() {
        _ => todo!(),
    }
}

fn to_string<T: ToString>(output: &mut String, val: &T) {
    *output = val.to_string();
}
