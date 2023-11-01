use crate::{Type, IntType, TypeEnum, FloatType, ArrayType, StructType, TypeName};

use super::{Field, DestructorType};

pub(super) fn size_of_type(typ: Type) -> usize {
    let base_size = match typ.as_type_enum() {
        TypeEnum::Int(IntType { size, .. }) => size.to_num() / 8,
        TypeEnum::Float(FloatType::F32) => 4,
        TypeEnum::Float(FloatType::F64) => 8,
        TypeEnum::Bool => 1,
        TypeEnum::Struct(StructType { fields, .. }) => {
            if fields.len() == 0 {
                return 0
            }
            
            let mut size_sum: usize = 0;

            for field in fields {
                let size = field.typ.size();
                let field_alignment = size_of_largest_field_in(&field.typ);

                size_sum = size_sum.next_multiple_of(field_alignment);
                size_sum += size;
            }

            let alignment = size_of_largest_field_in(&typ);
            size_sum.next_multiple_of(alignment)
        },
        TypeEnum::Ref(_) => 8,
        TypeEnum::Func(_) => 8,
        TypeEnum::Array(ArrayType { base, count }) => {
            base.size().next_multiple_of(size_of_largest_field_in(base)) * *count as usize
        }
        TypeEnum::Destructor(DestructorType { base, .. }) => base.size(),
        TypeEnum::Void => 0,
        TypeEnum::Unknown => panic!("cannot get size of unknown type"),
    };

    base_size
}

pub(super) fn size_of_largest_field_in(typ: &Type) -> usize {
    match typ.as_type_enum() {
        TypeEnum::Struct(StructType { fields, .. }) => {
            let mut sizes = fields.iter()
                .map(|Field { typ, .. }| typ)
                .map(size_of_largest_field_in)
                .collect::<Vec<_>>();

            sizes.sort();
            sizes.reverse();

            sizes[0]
        },
        TypeEnum::Array(ArrayType { base, .. }) => base.size(),
        TypeEnum::Destructor(DestructorType { base, .. }) => size_of_largest_field_in(base),
        _ => typ.size(),
    }
}
