use crate::{
    parse::TypeSpec, ArrayType, CompData, FuncType, Type, TypeEnum, TypeName, TypeRelation,
    UniqueFuncInfo, VarName,
};

pub trait GenericTable {
    fn get_at_index(&self, index: u8) -> Option<Type>;
    fn get_self(&self) -> Option<Type>;
}

impl GenericTable for Vec<Type> {
    fn get_at_index(&self, index: u8) -> Option<Type> { self.get(index as usize).cloned() }
    fn get_self(&self) -> Option<Type> { None }
}

impl GenericTable for () {
    fn get_at_index(&self, _: u8) -> Option<Type> { None }
    fn get_self(&self) -> Option<Type> { None }
}

impl<T: GenericTable> GenericTable for (T, Type) {
    fn get_at_index(&self, index: u8) -> Option<Type> { self.0.get_at_index(index) }
    fn get_self(&self) -> Option<Type> { Some(self.1.clone()) }
}

impl<T: GenericTable> GenericTable for (T, TypeRelation) {
    fn get_at_index(&self, index: u8) -> Option<Type> { self.0.get_at_index(index) }
    fn get_self(&self) -> Option<Type> { self.1.inner_type() }
}

impl<T: GenericTable> GenericTable for (T, Option<Type>) {
    fn get_at_index(&self, index: u8) -> Option<Type> { self.0.get_at_index(index) }
    fn get_self(&self) -> Option<Type> { self.1.clone() }
}

impl GenericTable for UniqueFuncInfo {
    fn get_at_index(&self, index: u8) -> Option<Type> {
        self.own_generics.get(index as usize).cloned()
    }
    fn get_self(&self) -> Option<Type> { self.relation.inner_type() }
}

impl CompData {
    pub fn get_spec(&self, spec: &TypeSpec, generics: &impl GenericTable) -> Option<Type> {
        dbg!(&spec);
        let typ = match spec {
            TypeSpec::Named(name) => self.get_by_name(name)?,
            TypeSpec::TypeLevelFunc(name, args) => {
                let func = self.type_level_funcs.get(name)?;
                let args = args
                    .iter()
                    .map(|arg| self.get_spec(arg, generics))
                    .collect::<Option<Vec<_>>>()?;
                func(args.clone(), self)
                    .with_from_function(name.clone())
                    .with_parameters(&args)
            },
            TypeSpec::Int(size) => Type::i(*size),
            TypeSpec::UInt(size) => Type::u(*size),
            TypeSpec::Float(size) => Type::f(*size),
            TypeSpec::Bool => Type::bool(),
            TypeSpec::Ref(base) => self.get_spec(base, generics)?.get_ref(),
            TypeSpec::Deref(base) => self.get_spec(base, generics)?.get_deref().unwrap(),
            TypeSpec::StructMember(struct_type, field_name) => {
                let struct_type = self.get_spec(struct_type, generics)?.complete_deref();
                let TypeEnum::Struct(struct_type) = struct_type.as_type_enum() else { panic!() };

                struct_type.get_field_type(field_name)?.clone()
            },
            TypeSpec::SumMember(sum_type, type_name) => {
                let sum_type = self.get_spec(sum_type, generics)?;
                let TypeEnum::Sum(sum_type_inner) = sum_type.as_type_enum() else { panic!() };

                sum_type_inner
                    .get_variant_type(&sum_type, &type_name)
                    .unwrap()
            },
            TypeSpec::Struct(fields) => {
                let mut typed_fields: Vec<(VarName, Type)> = Vec::new();

                for field in fields {
                    let field_type = self.get_spec(&field.1, generics)?;
                    typed_fields.push((field.0.clone(), field_type));
                }

                Type::new_struct(typed_fields)
            },
            TypeSpec::Sum(variants) => {
                let mut typed_variants: Vec<(TypeName, Type)> = Vec::new();

                for (name, sub_type_spec) in variants {
                    let sub_type = self.get_spec(&sub_type_spec, generics)?;
                    typed_variants.push((name.clone(), sub_type));
                }

                Type::new_sum(typed_variants)
            },
            TypeSpec::Tuple(types) => {
                let mut typed_fields: Vec<(VarName, Type)> = Vec::new();

                for (i, typ) in types.iter().enumerate() {
                    let field_type = self.get_spec(typ, generics)?;
                    typed_fields.push((VarName::from(i.to_string()), field_type));
                }

                Type::new_struct(typed_fields)
            },
            TypeSpec::Function(args, ret_type) => {
                let args = args
                    .iter()
                    .map(|arg| self.get_spec(arg, generics))
                    .collect::<Option<Vec<Type>>>()?;
                let ret_type = self.get_spec(ret_type, generics)?;
                ret_type.func_with_args(args)
            },
            TypeSpec::FuncReturnType(func_type) => {
                let func_type = self.get_spec(func_type, generics)?;
                let TypeEnum::Func(FuncType { ret_type, .. }) = func_type.as_type_enum() else { panic!() };
                ret_type.clone()
            },
            TypeSpec::Generic(name, generic_aliases) => {
                let generics = generic_aliases
                    .iter()
                    .map(|ga| self.get_spec(ga, generics))
                    .collect::<Option<Vec<_>>>()?;

                let cached_type = self.types.get(name);

                if cached_type.is_some() {
                    cached_type?.clone()
                } else {
                    self.get_spec(self.aliases.get(name)?, &generics)?
                        .with_name(name.clone())
                        .with_generics(&generics)
                }
            },
            TypeSpec::GetGeneric(on, index) => {
                let on = self.get_spec(on, generics)?;
                on.generics().get(*index as usize)?.clone()
            },
            TypeSpec::GenParam(index) => generics.get_at_index(*index)?,
            TypeSpec::Array(base, count) => self.get_spec(base, generics)?.get_array(*count),
            TypeSpec::ArrayElem(array) => {
                let array = self.get_spec(array, generics)?;
                let TypeEnum::Array(ArrayType { base, .. }) = array.as_type_enum() else { panic!() };
                base.clone()
            },
            TypeSpec::Union(left, right) => {
                let left = self.get_spec(left, generics)?;
                let right = self.get_spec(right, generics)?;

                let TypeEnum::Struct(l_struct_type) = left.as_type_enum() 
                    else { panic!() };
                let TypeEnum::Struct(r_struct_type) = right.as_type_enum() 
                    else { panic!() };

                let new_fields = l_struct_type
                    .fields
                    .iter()
                    .chain(r_struct_type.fields.iter())
                    .cloned()
                    .collect::<Vec<_>>();

                Type::new_struct(new_fields)
            },
            TypeSpec::Void => Type::void(),
            TypeSpec::Type(typ) => typ.clone(),
            TypeSpec::Me => {
                if let Some(se) = generics.get_self() {
                    se
                } else {
                    panic!()
                }
            },
        };

        Some(typ)
    }
}
