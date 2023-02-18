use crate::{CompData, Type, TypeEnum};

pub struct TypeHelperLib;

impl crate::library::Library for TypeHelperLib {
    fn add_to_unit(&self, unit: &mut crate::Unit) {
        unit.add_type_level_func("ErasePtrTypes".into(), erase_ptr_types);
    }
}

fn erase_ptr_types(input: Vec<Type>, _: &CompData) -> Type {
    let TypeEnum::Struct(struc) = input[0].as_type_enum() else { panic!() };
    // TODO: enusre all field types are pointers
    let field_count = struc.fields.len() as u32;
    Type::void().get_ref().get_array(field_count)
}
