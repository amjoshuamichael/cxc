use crate::{CompData, Repr, Type, TypeEnum};

pub struct TypeHelperLib;

impl crate::library::Library for TypeHelperLib {
    fn add_to_unit(&self, unit: &mut crate::Unit) {
        unit.add_type_level_func("ErasePtrTypes".into(), erase_ptr_types);
        unit.add_type_level_func("Transparent".into(), transparent);
    }
}

fn erase_ptr_types(input: Vec<Type>, _: &CompData) -> Type {
    let TypeEnum::Struct(struc) = input[0].as_type_enum() else { panic!() };
    // TODO: enusre all field types are pointers
    let field_count = struc.fields.len() as u32;
    Type::void().get_ref().get_array(field_count)
}

fn transparent(input: Vec<Type>, _: &CompData) -> Type {
    let TypeEnum::Struct(struc) = input[0].as_type_enum() else { panic!() };
    let mut struc = struc.clone();
    struc.repr = Repr::C;
    Type::new(TypeEnum::Struct(struc))
}
