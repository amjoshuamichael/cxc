use crate::{typ::TypeSpec, *};

impl Type {
    pub fn works_as_method_on(
        &self, 
        spec: TypeSpec, 
    ) -> Option<Vec<Type>> {
        let left = self.clone();
        let right = spec;

        let mut track_generics = Vec::new();

        for left in left.deref_chain() {
            if left.is_equivalent(right.clone(), &mut track_generics) {
                return Some(track_generics)
            }

            track_generics.clear();
        }

        None        
    }

    fn is_equivalent(&self, spec: TypeSpec, track_generics: &mut Vec<Type>) -> bool {
        let type_enum = self.as_type_enum();

        match spec {
            TypeSpec::Int(size) => 
                type_enum == &TypeEnum::Int(IntType { signed: true, size }),
            TypeSpec::UInt(size) => 
                type_enum == &TypeEnum::Int(IntType { signed: false, size }),
            TypeSpec::Float(float_type) => 
                type_enum == &TypeEnum::Float(float_type),
            TypeSpec::Named(name) => self.name() == &name,
            TypeSpec::Generic(name, generics) => {
                if self.name() != &name {
                    return false;
                }

                for (generic, generic_spec) 
                    in self.generics().iter().zip(generics.into_iter()) {
                    if !generic.is_equivalent(generic_spec, track_generics) {
                        return false;
                    }
                }

                return true;
            },
            TypeSpec::GenParam(index) => {
                let index = index as usize;

                if let Some(found_generic) = track_generics.get(index)
                    && found_generic != self {
                    false
                } else {
                    if track_generics.len() <= index as usize {
                        track_generics.resize(index + 1, Type::default());
                    }

                    track_generics[index] = self.clone();
                    true
                }
            },
            TypeSpec::Bool => type_enum == &TypeEnum::Bool,
            TypeSpec::Ref(spec) => {
                let TypeEnum::Ref(ref_type) = type_enum else { return false };
                ref_type.base.is_equivalent(*spec, track_generics)
            },
            TypeSpec::Struct(field_specs) => {
                let TypeEnum::Struct(StructType { fields, .. }) = type_enum 
                    else { return false };

                if fields.len() != field_specs.len() {
                    return false;
                }

                for ((field_name, field), (field_spec_name, field_spec)) 
                    in fields.iter().zip(field_specs.into_iter()) {

                    if field_name != &field_spec_name {
                        return false;
                    }

                    if !field.is_equivalent(field_spec, track_generics) {
                        return false;
                    }
                }

                return true;
            },
            TypeSpec::Tuple(field_specs) => {
                let TypeEnum::Struct(StructType { fields, .. }) = type_enum 
                    else { return false };

                if fields.len() != field_specs.len() {
                    return false;
                }

                for ((field_name, field), (field_spec_index, field_spec)) 
                    in fields.iter().zip(field_specs.into_iter().enumerate()) {
                    let VarName::TupleIndex(index) = field_name else { return false };

                    if *index != field_spec_index {
                        return false;
                    }

                    if !field.is_equivalent(field_spec, track_generics) {
                        return false;
                    }
                }

                return true;
            },
            TypeSpec::Sum(_) => todo!(),
            TypeSpec::Function(arg_specs, ret_spec) => {
                let TypeEnum::Func(FuncType { ret, args }) = type_enum 
                    else { return false };

                if args.len() != arg_specs.len() {
                    return false;
                }

                if !ret.is_equivalent(*ret_spec, track_generics) {
                    return false;
                }

                for (arg, arg_spec) in args.iter().zip(arg_specs.into_iter()) {
                    if !arg.is_equivalent(arg_spec, track_generics) {
                        return false;
                    }
                }

                return true;

            },
            TypeSpec::Array(base_spec, count_spec) => {
                let TypeEnum::Array(ArrayType { base, count }) = type_enum 
                    else { return false };

                *count == count_spec && base.is_equivalent(*base_spec, track_generics)
            },
            TypeSpec::Void => type_enum == &TypeEnum::Void,
            TypeSpec::Type(typ) => self == &typ,
            TypeSpec::Union(_, _) => todo!(),

            TypeSpec::GetGeneric(_, _) |
            TypeSpec::Deref(_) |
            TypeSpec::StructMember(_, _) |
            TypeSpec::SumMember(_, _) |
            TypeSpec::FuncReturnType(_) |
            TypeSpec::ArrayElem(_) |
            TypeSpec::TypeLevelFunc(_, _) |
            TypeSpec::Me => panic!(), // TODO: error
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{parse::TypeSpec, Type, Unit, errors::CResult};

    #[test]
    fn works_as_method_on() -> CResult<()> {
        let mut unit = Unit::new();

        assert!(Type::i(32).works_as_method_on("i32".into()).is_some());
        assert!(!Type::i(32).works_as_method_on("i64".into()).is_some());

        let i32i64 = Type::new_tuple(vec![Type::i(32), Type::i(64)]);
        assert!(i32i64.works_as_method_on(TypeSpec::from("{ i32, i64 }")).is_some());
        assert!(!i32i64.works_as_method_on(TypeSpec::from("{ i32, i64, i8 }")).is_some());

        assert!(i32i64.works_as_method_on(TypeSpec::from("{ i32, T }")).is_some());
        assert!(!i32i64.works_as_method_on(TypeSpec::from("{ i32, T, i8 }")).is_some());

        unit.push_script("Vec<T> = { capacity: i64, data_loc: &T, len: i64 }")
            .unwrap();

        let veci32 = unit
            .comp_data
            .get_spec(&TypeSpec::from("Vec<i32>"), &Vec::new())
            .unwrap();
        assert!(veci32.works_as_method_on(TypeSpec::from("Vec<i32>")).is_some());
        assert!(!veci32.works_as_method_on(TypeSpec::from("Vec<f32>")).is_some());
        assert!(veci32.works_as_method_on(TypeSpec::from("Vec<T>")).is_some());
        assert!(!veci32.works_as_method_on(TypeSpec::from("Vec<&T>")).is_some());

        Ok(())
    }
}
