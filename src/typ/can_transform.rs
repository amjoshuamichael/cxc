use crate::{typ::TypeSpec, *};

use super::Field;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TransformationStep {
    Ref(i32),
    Field(VarName),
    Fields(Box<[VarName]>),
    ArrayToSlice,
}

#[derive(Debug, PartialEq, Eq, Clone, Default)]
pub enum TransformationList {
    Cons(TransformationStep, Box<Self>),

    #[default]
    Nil,
}

impl TransformationList {
    pub fn to_vec(mut self) -> Vec<TransformationStep> {
        let mut steps = Vec::new();

        while let TransformationList::Cons(step, rest) = self {
            steps.push(step);
            self = *rest;
        }

        steps
    }
}

#[derive(Debug)]
pub struct Transformation {
    pub generics: Vec<Type>,
    pub steps: TransformationList,
}

pub fn transformation_steps_dist(list: &TransformationList) -> u32 {
    let mut rest = list;
    let mut sum = 0;

    while let TransformationList::Cons(step, new_rest) = rest {
        rest = new_rest;

        sum += match step {
            TransformationStep::Ref(count) => count.abs() as u32,
            _ => 1,
        };
    }
    
    sum
}

impl Type {
    pub fn can_transform_to(
        &self, 
        spec: TypeSpec, 
    ) -> Option<Transformation> {
        match self.as_type_enum() {
            TypeEnum::Ref(RefType { base }) => {
                let mut base = base.clone();
                let mut reference_count = 1;

                while let Some(dereffed) = base.get_deref() {
                    base = dereffed.clone();
                    reference_count += 1;
                }

                return base.can_transform_to(spec.clone()).map(|equivalence| {
                    Transformation {
                        steps: TransformationList::Cons(
                            TransformationStep::Ref(-reference_count), 
                            Box::new(equivalence.steps),
                        ),
                        generics: equivalence.generics,
                    }
                })
            },
            TypeEnum::Struct(struct_type) => {
                if let TypeSpec::Struct(field_specs) = spec.clone()
                    && struct_type.fields.len() != field_specs.len()
                    && field_specs.iter().all(|(inherited, _, _)| *inherited) {
                    let mut generics = Vec::new();

                    let mut field_names = Vec::new();

                    let mut failure = false;

                    for (_, name, spec) in field_specs {
                        let Ok(field_index) = struct_type.get_field_index(&name)
                            else { failure = true; break };

                        let field = &struct_type.fields[field_index];

                        if !field.inherited || !field.typ.is_equivalent(spec, &mut generics) {
                             failure = true;
                             break;
                        }

                        field_names.push(name);
                    }

                    if !failure {
                        return Some(Transformation {
                            generics,
                            steps: TransformationList::Cons(
                                TransformationStep::Fields(field_names.into_boxed_slice()),
                                Box::new(TransformationList::Nil),
                            ),
                        })
                    }
                }

                for Field { inherited, name, typ } in &struct_type.fields {
                    if !inherited { continue }
                    
                    if let Some(transformation) = typ.can_transform_to(spec.clone()) {
                        return Some(Transformation {
                            steps: TransformationList::Cons(
                                TransformationStep::Field(name.clone()),
                                Box::new(transformation.steps),
                            ),
                            generics: transformation.generics,
                        });
                    }
                }
            },
            TypeEnum::Array(ArrayType { base, count: _ }) => {
                let slice = Type::new_struct(vec![
                    Field { inherited: true, name: "ptr".into(), typ: base.get_ref() },
                    Field { inherited: true, name: "len".into(), typ: Type::u(64) },
                ]);

                if let Some(transformation) = slice.can_transform_to(spec.clone()) {
                    return Some(Transformation {
                        steps: TransformationList::Cons(
                            TransformationStep::ArrayToSlice,
                            Box::new(transformation.steps),
                        ),
                        generics: transformation.generics,
                    });
                }
            },
            _ => { }
        }

        if let TypeSpec::Ref(mut spec) = spec {
            let mut reference_count = 1;

            while let TypeSpec::Ref(base) = *spec {
                reference_count += 1;
                spec = base;
            }

            return self.can_transform_to(*spec).map(|equivalence| 
                Transformation {
                    steps: TransformationList::Cons(
                        TransformationStep::Ref(reference_count), 
                        Box::new(equivalence.steps),
                    ),
                    generics: equivalence.generics,
                }
            );
        }
        
        let mut generics = Vec::new();

        self.is_equivalent(spec, &mut generics).then(|| Transformation {
            steps: TransformationList::Nil,
            generics,
        })
    }

    pub fn route_to(&self, field: VarName) -> Option<(TransformationList, Type)> {
        match self.as_type_enum() {
            TypeEnum::Ref(RefType { base }) => {
                let mut base = base.clone();
                let mut reference_count = 1;

                while let Some(dereffed) = base.get_deref() {
                    base = dereffed.clone();
                    reference_count += 1;
                }

                return base.route_to(field).map(|(list, typ)|
                    (
                        TransformationList::Cons(
                            TransformationStep::Ref(-reference_count), 
                            Box::new(list),
                        ),
                        typ
                    )
                )
            },
            TypeEnum::Struct(StructType { fields, .. }) => {
                for Field { name, typ, .. } in fields {
                    if name == &field {
                        return Some((TransformationList::Nil, typ.clone()));
                    }
                }

                for Field { inherited, typ, name } in fields {
                    if !inherited { continue }

                    if let Some((list, typ)) = typ.route_to(field.clone()) {
                        return Some((
                            TransformationList::Cons(
                                TransformationStep::Field(name.clone()),
                                Box::new(list),
                            ),
                            typ.clone()
                        ));
                    }
                }

                None
            },
            TypeEnum::Array(ArrayType { base, .. }) => {
                let field_typ = match &*field {
                    "ptr" => base.get_ref(),
                    "len" => Type::u(64),
                    _ => return None,
                };

                Some((
                    TransformationList::Cons(
                        TransformationStep::Field(field),
                        Box::new(
                            TransformationList::Cons(
                                TransformationStep::ArrayToSlice, 
                                Box::new(TransformationList::Nil)
                            )
                        ),
                    ),
                    field_typ
                ))
            }
            _ => None,
        }
    }

    fn is_equivalent(
        &self, 
        spec: TypeSpec, 
        generics: &mut Vec<Type>,
    ) -> bool {
        let type_enum = self.as_type_enum();

        match spec {
            TypeSpec::Int(size) => 
                type_enum == &TypeEnum::Int(IntType::new(size, true)),
            TypeSpec::UInt(size) => 
                type_enum == &TypeEnum::Int(IntType::new(size, false)),
            TypeSpec::Float(float_type) => 
                type_enum == &TypeEnum::Float(float_type),
            TypeSpec::Named(name) => self.name() == &name,
            TypeSpec::Generic(name, set_generics) => {
                if self.name() != &name {
                    return false;
                }

                for (generic, generic_spec) 
                    in self.generics().iter().zip(set_generics.into_iter()) {
                    if !generic.is_equivalent(generic_spec, generics) {
                        return false;
                    }
                }

                return true;
            },
            TypeSpec::GenParam(index) => {
                let index = index as usize;

                if let Some(found_generic) = generics.get(index)
                    && found_generic != &Type::unknown()
                    && found_generic != self {
                    false
                } else {
                    if generics.len() <= index as usize {
                        generics.resize(index + 1, Type::unknown());
                    }

                    generics[index] = self.clone();
                    true
                }
            },
            TypeSpec::Bool => type_enum == &TypeEnum::Bool,
            TypeSpec::Ref(spec) => {
                let TypeEnum::Ref(RefType { base }) = self.as_type_enum() 
                    else { return false };
                base.is_equivalent(*spec, generics)
            },
            TypeSpec::Struct(field_specs) => {
                let TypeEnum::Struct(StructType { fields, .. }) = type_enum 
                    else { return false };

                if fields.len() != field_specs.len() {
                    return false;
                }

                for (
                    Field { name: field_name, typ: field, .. }, 
                    (_, field_spec_name, field_spec)
                ) in fields.iter().zip(field_specs.into_iter()) {

                    if field_name != &field_spec_name {
                        return false;
                    }

                    if !field.is_equivalent(field_spec, generics) {
                        return false;
                    }
                }

                true
            },
            TypeSpec::Tuple(field_specs) => {
                let TypeEnum::Struct(StructType { fields, .. }) = type_enum 
                    else { return false };

                if fields.len() != field_specs.len() {
                    return false;
                }

                for (
                    Field { name: field_name, typ: field, .. },
                    (field_spec_index, field_spec),
                ) in fields.iter().zip(field_specs.into_iter().enumerate()) {
                    let VarName::TupleIndex(index) = field_name else { return false };

                    if *index != field_spec_index {
                        return false;
                    }

                    if !field.is_equivalent(field_spec, generics) {
                        return false;
                    }
                }

                return true;
            },
            TypeSpec::Sum(_) => todo!(),
            TypeSpec::Function(arg_specs, ret_spec) => {
                let TypeEnum::Func(FuncType { ret, args, .. }) = type_enum 
                    else { return false };

                if args.len() != arg_specs.len() {
                    return false;
                }

                if !ret.is_equivalent(*ret_spec, generics) {
                    return false;
                }

                for (arg, arg_spec) in args.iter().zip(arg_specs.into_iter()) {
                    if !arg.is_equivalent(arg_spec, generics) {
                        return false;
                    }
                }

                true
            },
            TypeSpec::Array(base_spec, count_spec) => {
                let TypeEnum::Array(ArrayType { base, count }) = type_enum 
                    else { return false };

                *count == count_spec && base.is_equivalent(*base_spec, generics)
            },
            TypeSpec::Destructor(_, _) => panic!(),
            TypeSpec::Void => type_enum == &TypeEnum::Void,
            TypeSpec::GetGeneric(_, _) |
            TypeSpec::Deref(_) |
            TypeSpec::StructMember(_, _) |
            TypeSpec::SumMember(_, _) |
            TypeSpec::FuncReturnType(_) |
            TypeSpec::FuncArgType(..) |
            TypeSpec::ArrayElem(_) |
            TypeSpec::TypeLevelFunc(_, _) |
            TypeSpec::Unknown |
            TypeSpec::Me => panic!(), // TODO: error
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{parse::TypeSpec, Type, Unit, errors::CResult, typ::can_transform::{Transformation, TransformationStep}, VarName};

    #[test]
    fn method_transformations() -> CResult<()> {
        let mut unit = Unit::new();

        unit.push_script("Vec<T> = { ptr: &T, capacity: i64, len: i64 }")
            .unwrap();

        let to_typ = |code| unit
            .comp_data
            .get_spec(&TypeSpec::from(code), &Vec::new())
            .unwrap();

        let comp = |code, spec| 
            to_typ(code)
            .can_transform_to(TypeSpec::from(spec))
            .map(|Transformation { generics, steps }| (generics, steps.to_vec()));

        let refstp = |ref_count| TransformationStep::Ref(ref_count);
        let fldstp = |field_name: &str| TransformationStep::Field(field_name.into());
        let flsstp = |field_names: &[&str]| TransformationStep::Fields(
            field_names
                .into_iter()
                .copied()
                .map(VarName::from)
                .collect::<Vec<_>>()
                .into_boxed_slice()
        );

        assert_eq!(comp("i32", "i32"), Some((vec![], vec![])));
        assert_eq!(comp("i32", "i64"), None);

        assert_eq!(comp("{i32, i64}", "{i32, i64}"), Some((vec![], vec![])));
        assert_eq!(comp("{i32, i64}", "{i32, i64, i8}"), None);

        assert_eq!(comp("{i32, i64}", "{i32, T}"), Some((vec![Type::i(64)], vec![])));
        assert_eq!(comp("{i32, i64}", "{i32, T, i8}"), None);

        assert_eq!(
            comp("{i32, i64}", "{T, U}"), 
            Some((vec![Type::i(32), Type::i(64)], vec![]))
        );
        assert_eq!(comp("{i32, i64}", "{T, U, V}"), None);

        assert_eq!(
            comp("{i32, i64}", "{U, T}"), 
            Some((vec![Type::i(64), Type::i(32)], vec![]))
        );
        assert_eq!(comp("{i32, i64}", "{V, U, T}"), None);

        assert_eq!(comp("{i64, i64}", "{T, T}"), Some((vec![Type::i(64)], vec![])));
        assert_eq!(comp("{i32, i64}", "{T, T}"), None);

        assert_eq!(comp("Vec<i32>", "Vec<i32>"), Some((vec![], vec![])));
        assert_eq!(comp("Vec<i32>", "Vec<f32>"), None);
        assert_eq!(comp("Vec<i32>", "Vec<T>"), Some((vec![Type::i(32)], vec![])));
        assert_eq!(comp("Vec<i32>", "Vec<&T>"), None);


        assert_eq!(comp("Vec<i32>", "&Vec<i32>"), Some((vec![], vec![refstp(1)])));
        assert_eq!(comp("Vec<i32>", "&Vec<f32>"), None);
        assert_eq!(comp("Vec<i32>", "&Vec<T>"), Some((vec![Type::i(32)], vec![refstp(1)])));
        assert_eq!(comp("Vec<i32>", "&Vec<&T>"), None);

        assert_eq!(comp("&Vec<i32>", "Vec<i32>"), Some((vec![], vec![refstp(-1)])));
        assert_eq!(comp("&Vec<i32>", "Vec<f32>"), None);
        assert_eq!(comp("&Vec<i32>", "Vec<T>"), Some((vec![Type::i(32)], vec![refstp(-1)])));
        assert_eq!(comp("&Vec<i32>", "Vec<&T>"), None);

        assert_eq!(
            comp("{+x: {i32, i64}}", "{i32, i64}"), 
            Some((vec![], vec![fldstp("x")]))
        );
        assert_eq!(
            comp("&{+x: {i32, i64}}", "{i32, i64}"), 
            Some((vec![], vec![refstp(-1), fldstp("x")]))
        );
        assert_eq!(
            comp("{+x: {i32, i64}}", "&{i32, i64}"), 
            Some((vec![], vec![fldstp("x"), refstp(1)]))
        );
        assert_eq!(comp("{+x: {i32, i32}}", "{i32, i64}"), None);

        assert_eq!(
            comp("{+y: {+x: {i32, i64}}}", "{i32, i64}"), 
            Some((vec![], vec![fldstp("y"), fldstp("x")]))
        );
        assert_eq!(
            comp("{+y: {+x: {i32, i64}}}", "&{T, i64}"), 
            Some((vec![Type::i(32)], vec![fldstp("y"), fldstp("x"), refstp(1)]))
        );
        assert_eq!(
            comp("{+y: {+x: {i32, i64}}}", "&{i32, i64}"), 
            Some((vec![], vec![fldstp("y"), fldstp("x"), refstp(1)]))
        );
        assert_eq!(
            comp("{+y: {+x: {i32, i64}}}", "&{T, i64}"), 
            Some((vec![Type::i(32)], vec![fldstp("y"), fldstp("x"), refstp(1)]))
        );

        assert_eq!(
            comp("{+x: i32, +y: i64}", "{+x: i32, +y: i64}"), 
            Some((vec![], vec![]))
        );
        assert_eq!(
            comp("{+x: i32, +y: i64, z: u32}", "{+x: i32, +y: i64}"), 
            Some((vec![], vec![flsstp(&["x", "y"])]))
        );
        assert_eq!(comp("{+x: i32, y: i64, z: u32}", "{+x: i32, +y: i64}"), None);
        
        Ok(())
    }

    #[test]
    fn field_transformations() {
        let mut unit = Unit::new();

        unit.push_script("RcBox<T> = { strong: i64, weak: i64, +value: T }").unwrap();
        unit.push_script("Rc<T> = { +inner: &RcBox<T> }").unwrap();

        let _comp = |spec: &str, field: &str| 
            unit.comp_data.get_spec(&TypeSpec::from(spec), &())
            .unwrap()
            .route_to(VarName::from(field))
            .map(|(steps, typ)| (typ, steps.to_vec()));
    }
}
