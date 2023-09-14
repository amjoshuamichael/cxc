use crate::{
    errors::{TErr, TResult},
    parse::TypeSpec,
    ArrayType, CompData, FuncType, Type, TypeEnum, TypeRelation, FuncQuery,
    VarName, typ::{Field, can_transform::TransformationList},
};
use std::fmt::Debug;

pub trait GenericTable: Debug {
    fn get_at_index(&self, _: u8) -> Option<Type> { None }
    fn get_self(&self) -> Option<Type> { None }

    fn all_generics(&self) -> Vec<Type> {
        let mut all = Vec::new();

        for n in 0.. {
            if let Some(generic) = self.get_at_index(n) {
                all.push(generic);
            } else {
                break;
            }
        }

        all
    }
}

impl GenericTable for Vec<Type> {
    fn get_at_index(&self, index: u8) -> Option<Type> { 
        self.get(index as usize).cloned()
    }
}

impl GenericTable for () { }

impl<T: GenericTable> GenericTable for (T, Type) {
    fn get_at_index(&self, index: u8) -> Option<Type> { self.0.get_at_index(index) }
    fn get_self(&self) -> Option<Type> { Some(self.1.clone()) }
}

impl GenericTable for FuncQuery {
    fn get_at_index(&self, index: u8) -> Option<Type> { 
        self.generics.get(index as usize).cloned()
    }
    fn get_self(&self) -> Option<Type> { self.relation.inner_type().cloned() }
}

impl GenericTable for (&Vec<Type>, &TypeRelation) {
    fn get_at_index(&self, index: u8) -> Option<Type> { 
        self.0.get(index as usize).cloned()
    }
    fn get_self(&self) -> Option<Type> { self.1.inner_type().cloned() }
}

impl<T: GenericTable> GenericTable for (T, Option<Type>) {
    fn get_at_index(&self, index: u8) -> Option<Type> { self.0.get_at_index(index) }
    fn get_self(&self) -> Option<Type> { self.1.clone() }
}

#[derive(Debug)]
pub struct GetSpecReport {
    pub typ: Type,
    pub transformation: Option<TransformationList>,
}

impl CompData {
    pub fn get_spec(&self, spec: &TypeSpec, generics: &impl GenericTable) -> TResult<Type> {
        self.get_spec_report(&spec, generics).map(|re| re.typ)
    }

    pub fn get_spec_report(
        &self,
        spec: &TypeSpec,
        generics: &impl GenericTable,
    ) -> TResult<GetSpecReport> {
        let typ = match spec {
            TypeSpec::Named(name) => self.get_by_name(name)?,
            TypeSpec::TypeLevelFunc(name, args) => {
                let func = self
                    .type_level_funcs
                    .get(name)
                    .ok_or(TErr::UnknownFunc(name.clone()))?;
                let args = args
                    .iter()
                    .map(|arg| self.get_spec(arg, generics))
                    .collect::<TResult<Vec<_>>>()?;
                // TODO: type level function errors
                func(args.clone(), self)
            },
            TypeSpec::Int(size) => Type::i(*size),
            TypeSpec::UInt(size) => Type::u(*size),
            TypeSpec::Float(size) => Type::f(*size),
            TypeSpec::Bool => Type::bool(),
            TypeSpec::Ref(base) => self.get_spec(base, generics)?.get_ref(),
            TypeSpec::Deref(base) => self.get_spec(base, generics)?.get_auto_deref(&*self)?,
            TypeSpec::StructMember(struct_type, field_name) => {
                let struct_type = self.get_spec(struct_type, generics)?;
                if let Some((trans, typ)) = struct_type.route_to(field_name.clone()) {
                    return Ok(GetSpecReport {
                        typ,
                        transformation: Some(trans),
                    });
                } else {
                    return Err(TErr::FieldNotFound(
                        struct_type.clone_type_enum(), 
                        field_name.clone(),
                    ));
                }
            },
            TypeSpec::SumMember(_sum_type, _type_name) => {
                todo!()
            },
            TypeSpec::Struct(fields) => {
                Type::new_struct(
                    fields
                        .iter()
                        .map(|&(inherited, ref name, ref typ)| Ok(Field {
                            name: name.clone(),
                            typ: self.get_spec(&typ, generics)?,
                            inherited,
                        }))
                        .collect::<Result<_, _>>()?
                )
            },
            TypeSpec::Sum(_variants) => {
                todo!()
            },
            TypeSpec::Tuple(types) => {
                Type::new_struct(
                    types
                        .iter()
                        .enumerate()
                        .map(|(f, &ref typ)| Ok(Field {
                            name: VarName::TupleIndex(f),
                            typ: self.get_spec(&typ, generics)?,
                            inherited: false,
                        }))
                        .collect::<Result<_, _>>()?
                )
            },
            TypeSpec::Function(args, ret_type) => {
                let args = args
                    .iter()
                    .map(|arg| self.get_spec(arg, generics))
                    .collect::<TResult<Vec<Type>>>()?;
                let ret_type = self.get_spec(ret_type, generics)?;
                ret_type.func_with_args(args)
            },
            TypeSpec::FuncReturnType(func_type) => {
                let func_type = self.get_spec(func_type, generics)?;

                let TypeEnum::Func(FuncType { ret: ret_type, .. }) = func_type.as_type_enum()
                    else { Err(TErr::NotAFunction(func_type))? };

                ret_type.clone()
            },
            TypeSpec::FuncArgType(func_type, index) => {
                let func_type = self.get_spec(func_type, generics)?;

                let TypeEnum::Func(FuncType { args, .. }) = func_type.as_type_enum()
                    else { Err(TErr::NotAFunction(func_type))? };

                args[*index].clone()
            },
            TypeSpec::Generic(name, generic_aliases) => {
                let generics = generic_aliases
                    .iter()
                    .map(|ga| self.get_spec(ga, generics))
                    .collect::<TResult<Vec<_>>>()?;

                self.get_spec(self.get_typedef_of(name)?, &generics)?
                    .with_name(name.clone())
                    .with_generics(&generics)
            },
            TypeSpec::GetGeneric(on, index) => {
                let on = self.get_spec(on, generics)?;
                on.generics()
                    .get(*index as usize)
                    .ok_or(TErr::CantGetGeneric(on.clone(), on.generics().clone(), *index))?
                    .clone()
            },
            TypeSpec::GenParam(index) => {
                generics
                .get_at_index(*index)
                .ok_or(TErr::TooFewGenerics(spec.clone(), generics.all_generics(), *index))?
            }
            TypeSpec::Array(base, count) => self.get_spec(base, generics)?.get_array(*count),
            TypeSpec::ArrayElem(array) => {
                let array = self.get_spec(array, generics)?;
                let TypeEnum::Array(ArrayType { base, .. }) = array.as_type_enum() 
                    else { return Err(TErr::NotAnArray(array)) };
                base.clone()
            },
            TypeSpec::Void => Type::void(),
            TypeSpec::Unknown => Type::unknown(),
            TypeSpec::Me => {
                if let Some(me) = generics.get_self() {
                    me
                } else {
                    panic!()
                }
            },
        };

        Ok(GetSpecReport { typ, transformation: None })
    }
}
