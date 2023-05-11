use inkwell::{types::AnyTypeEnum, values::{BasicValueEnum, ArrayValue}};

// https://github.com/TheDan64/inkwell/issues/394#issuecomment-1403077699
pub fn const_array(
    elem_type: AnyTypeEnum<'static>, 
    elems: impl Iterator<Item = BasicValueEnum<'static>>,
) -> ArrayValue<'static> {
    match elem_type {
        AnyTypeEnum::IntType(t) => {
            t.const_array(&elems.map(|p| p.into_int_value()).collect::<Vec<_>>())
        },
        AnyTypeEnum::FloatType(t) => {
            t.const_array(&elems.map(|p| p.into_float_value()).collect::<Vec<_>>())
        },
        AnyTypeEnum::PointerType(t) => {
            t.const_array(&elems.map(|p| p.into_pointer_value()).collect::<Vec<_>>())
        },
        AnyTypeEnum::VectorType(t) => {
            t.const_array(&elems.map(|p| p.into_vector_value()).collect::<Vec<_>>())
        },
        AnyTypeEnum::StructType(t) => {
            t.const_array(&elems.map(|p| p.into_struct_value()).collect::<Vec<_>>())
        },
        AnyTypeEnum::ArrayType(t) => {
            t.const_array(&elems.map(|p| p.into_array_value()).collect::<Vec<_>>())
        },
        _ => todo!(),
    }
}
