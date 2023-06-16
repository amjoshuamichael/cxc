use crate::{Type, IntType, TypeEnum, FloatType, ArrayType, StructType};

pub(super) fn size_of_type(typ: Type) -> usize {
    let base_size = match typ.as_type_enum() {
        TypeEnum::Int(IntType { size, .. }) => (*size / 8) as usize,
        TypeEnum::Float(FloatType::F16) => 2,
        TypeEnum::Float(FloatType::F32) => 4,
        TypeEnum::Float(FloatType::F64) => 8,
        TypeEnum::Bool(_) => 1,
        TypeEnum::Struct(StructType { fields, .. }) => {
            if fields.len() == 0 {
                return 0
            }
            
            let size_sum: usize = fields.iter().map(|(_, typ)| typ.size()).sum();
            let alignment = size_of_largest_field_in(&typ);

            size_sum.next_multiple_of(alignment)
        },
        TypeEnum::Sum(sum_type) => {
            sum_type.largest_variant_as_struct().size()
        },
        TypeEnum::Variant(variant_type) => {
            variant_type.as_struct().size()
        }
        TypeEnum::Ref(_) => 8,
        TypeEnum::Func(_) => 8,
        TypeEnum::Array(ArrayType { base, count }) => {
            base.size().next_power_of_two() * *count as usize
        }
        TypeEnum::Void => 0,
        TypeEnum::Unknown => panic!("cannot get size of unknown type"),
    };

    base_size
}

// TODO: make a function that converts a type to a struct type if it can be, and use that 
// in "to llvm type" and here.
//
// note: this wouldn't work because of large arrays??? sometimes i worry about past josh
fn size_of_largest_field_in(typ: &Type) -> usize {
    match typ.as_type_enum() {
        TypeEnum::Struct(StructType { fields, .. }) => {
            let mut sizes = fields.iter()
                .map(|(_, typ)| typ)
                .map(size_of_largest_field_in)
                .collect::<Vec<_>>();

            sizes.sort();
            sizes.reverse();

            sizes[0]
        },
        TypeEnum::Array(ArrayType { base, .. }) => base.size(),
        _ => typ.size(),
    }
}

impl StructType {
    pub fn field_offset_in_bytes(&self, field_index: usize) -> usize {
        let nescessary_fields = &self.fields[0..(field_index as usize)];
        let size_sum: usize = nescessary_fields.iter().map(|(_, typ)| typ.size()).sum();
        // TODO: make size_of_largest_field a trait? That way we wouldn't have to do this 
        // Type::new_struct hack
        let alignment = size_of_largest_field_in(&Type::new_struct(self.fields.clone()));
        size_sum.next_multiple_of(alignment)
    }   
}

