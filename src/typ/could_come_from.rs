use crate::{parse::TypeSpec, *};

use super::fields_iter::FieldsIter;

impl Type {
    pub fn could_come_from(&self, spec: TypeSpec, comp_data: &CompData) -> bool {
        // we'll realize the spec with a temporary type to fill in the generics,
        // then we'll compare it to the self, and if we hit that
        // temporary type, we'll assume that that field is equal, and
        // move on.
        let filler = Type::empty().with_name("Filler".into());
        let filled_spec = comp_data
            .get_spec(&spec, &vec![filler.clone(); 10])
            .unwrap();

        let mut self_iter = FieldsIter::new(self.clone().wrap());
        let mut filled_spec_iter = FieldsIter::new(filled_spec.wrap());

        while let Some(next_field) = self_iter.next() {
            let filled_next = filled_spec_iter.next();
            if filled_next.is_none() {
                return false;
            };
            let filled_next = filled_next.unwrap();

            if filled_next == filler {
                self_iter.skip_children_of_last();
                continue;
            }

            match next_field.clone().as_type_enum() {
                TypeEnum::Array(ArrayType {
                    count: self_count, ..
                }) => {
                    let TypeEnum::Array(ArrayType { count: other_count, .. }) = filled_next.as_type_enum() else { return false };
                    if self_count != other_count {
                        return false;
                    };
                },
                TypeEnum::Struct(_)
                | TypeEnum::Sum(_)
                | TypeEnum::Variant(_)
                | TypeEnum::Ref(_)
                | TypeEnum::Bool(_)
                | TypeEnum::Func(_)
                | TypeEnum::Void => {
                    if std::mem::discriminant(next_field.as_type_enum())
                        != std::mem::discriminant(filled_next.as_type_enum())
                    {
                        return false;
                    }
                },
                TypeEnum::Int(_) | TypeEnum::Float(_) | TypeEnum::Opaque(_) => {
                    if next_field != filled_next {
                        return false;
                    };
                },
                TypeEnum::Unknown => return false,
            }
        }

        filled_spec_iter.next().is_none()
    }
}

#[cfg(test)]
mod tests {
    use crate::{parse::TypeSpec, Type, Unit};

    #[test]
    fn type_could_come_from() {
        let mut unit = Unit::new();

        assert!(Type::i(32).could_come_from(TypeSpec::Int(32), &unit.comp_data));
        assert!(!Type::i(32).could_come_from(TypeSpec::Int(64), &unit.comp_data));

        let i32i64 = Type::new_tuple(vec![Type::i(32), Type::i(64)]);
        assert!(i32i64.could_come_from(TypeSpec::from("{ i32, i64 }"), &unit.comp_data));
        assert!(!i32i64.could_come_from(TypeSpec::from("{ i32, i64, i8 }"), &unit.comp_data));

        assert!(i32i64.could_come_from(TypeSpec::from("{ i32, T }"), &unit.comp_data));
        assert!(!i32i64.could_come_from(TypeSpec::from("{ i32, T, i8 }"), &unit.comp_data));

        unit.push_script("Vec<T> = { capacity: i64, data_loc: &T, len: i64 }");

        let veci32 = unit
            .comp_data
            .get_spec(&TypeSpec::from("Vec<i32>"), &Vec::new())
            .unwrap();
        assert!(veci32.could_come_from(TypeSpec::from("Vec<i32>"), &unit.comp_data));
        assert!(!veci32.could_come_from(TypeSpec::from("Vec<f32>"), &unit.comp_data));
        assert!(veci32.could_come_from(TypeSpec::from("Vec<T>"), &unit.comp_data));
        assert!(!veci32.could_come_from(TypeSpec::from("Vec<&T>"), &unit.comp_data));
    }
}
