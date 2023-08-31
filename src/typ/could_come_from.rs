use crate::{parse::TypeSpec, *, errors::TResult};

use super::fields_iter::FieldsIter;

impl Type {
    pub fn could_come_from(&self, spec: TypeSpec, comp_data: &CompData) -> TResult<bool> {
        // we'll realize the spec with a temporary type to fill in the generics,
        // then we'll compare it to the self, and if we hit that
        // temporary type, we'll assume that that field is equal, and
        // move on.
        let filler = Type::empty().with_name("Filler".into());
        let filled_spec = comp_data.get_spec(&spec, &vec![filler; 10])?;

        Ok(could_come_from_filled(self, &filled_spec))
    }
}

fn could_come_from_filled(self_type: &Type, other_type: &Type) -> bool {
    if (self_type.name() != other_type.name() && self_type.name() != &TypeName::Anonymous)
        && other_type
            .generics()
            .iter()
            .zip(self_type.generics().iter())
            .all(|(other_gen, self_gen)| could_come_from_filled(self_gen, other_gen))
    {
        return false;
    }

    let filler = Type::empty().with_name("Filler".into());

    let mut self_iter = FieldsIter::new(self_type.clone().wrap());
    let mut other_iter = FieldsIter::new(other_type.clone().wrap());

    while let Some(self_next) = self_iter.next() {
        let other_next = other_iter.next();

        if other_next.is_none() {
            return false;
        };
        let other_next = other_next.unwrap();

        if other_next == filler {
            self_iter.skip_children_of_last();
            continue;
        }

        if other_next.name() == self_next.name()
            && self_next.name() != &TypeName::Anonymous
            && other_next
                .generics()
                .iter()
                .zip(self_next.generics().iter())
                .all(|(other_gen, self_gen)| could_come_from_filled(self_gen, other_gen))
        {
            self_iter.skip_children_of_last();
            other_iter.skip_children_of_last();
            continue;
        }

        match self_next.clone().as_type_enum() {
            TypeEnum::Array(ArrayType {
                count: self_count,
                base: self_base,
            }) => {
                let TypeEnum::Array(ArrayType { count: other_count, base: other_base }) = 
                        other_next.as_type_enum() else { return false };
                if other_count != self_count || !could_come_from_filled(self_base, other_base) {
                    return false;
                };
            },
            TypeEnum::Struct(_)
            | TypeEnum::Ref(_)
            | TypeEnum::Bool(_)
            | TypeEnum::Func(_)
            | TypeEnum::Void => {
                if std::mem::discriminant(self_next.as_type_enum())
                    != std::mem::discriminant(other_next.as_type_enum())
                {
                    return false;
                }
            },
            TypeEnum::Int(_) | TypeEnum::Float(_) => {
                if !self_next.is_subtype_of(&other_next) {
                    return false;
                };
            },
            TypeEnum::Unknown => return false,
        }
    }

    other_iter.next().is_none()
}

#[cfg(test)]
mod tests {
    use crate::{parse::TypeSpec, Type, Unit, errors::CResult};

    #[test]
    fn type_could_come_from() -> CResult<()> {
        let mut unit = Unit::new();

        assert!(Type::i(32).could_come_from("i32".into(), &unit.comp_data)?);
        assert!(!Type::i(32).could_come_from("i64".into(), &unit.comp_data)?);

        let i32i64 = Type::new_tuple(vec![Type::i(32), Type::i(64)]);
        assert!(i32i64.could_come_from(TypeSpec::from("{ i32, i64 }"), &unit.comp_data)?);
        assert!(!i32i64.could_come_from(TypeSpec::from("{ i32, i64, i8 }"), &unit.comp_data)?);

        assert!(i32i64.could_come_from(TypeSpec::from("{ i32, T }"), &unit.comp_data)?);
        assert!(!i32i64.could_come_from(TypeSpec::from("{ i32, T, i8 }"), &unit.comp_data)?);

        unit.push_script("Vec<T> = { capacity: i64, data_loc: &T, len: i64 }")
            .unwrap();

        let veci32 = unit
            .comp_data
            .get_spec(&TypeSpec::from("Vec<i32>"), &Vec::new())
            .unwrap();
        assert!(veci32.could_come_from(TypeSpec::from("Vec<i32>"), &unit.comp_data)?);
        assert!(!veci32.could_come_from(TypeSpec::from("Vec<f32>"), &unit.comp_data)?);
        assert!(veci32.could_come_from(TypeSpec::from("Vec<T>"), &unit.comp_data)?);
        assert!(!veci32.could_come_from(TypeSpec::from("Vec<&T>"), &unit.comp_data)?);

        unit.add_type_level_func("Copy".into(), |args, _| args[0].clone());
        let intified = unit
            .comp_data
            .get_spec(&"Copy({ i32, i64 })".into(), &Vec::new())
            .unwrap();
        assert!(intified.could_come_from("Copy(T)".into(), &unit.comp_data)?);
        assert!(intified.could_come_from("Copy({ i32, T })".into(), &unit.comp_data)?);
        assert!(!intified.could_come_from("Copy(Vec<T>)".into(), &unit.comp_data)?);

        Ok(())
    }
}
