use crate::{
    errors::{TErr, TResult},
    parse::{TypeSpec, FuncCode, VarDecl, FieldSpec},
    ArrayType, CompData, FuncType, Type, TypeEnum, TypeRelation, FuncQuery,
    VarName, typ::{Field, can_transform::TransformationList, ABI, DestructorType},
};
use std::{fmt::Debug, sync::Arc};

use super::{BorrowedCachedTypeRetrieval, CachedTypeRetrieval};

pub trait GenericTable: Debug {
    fn get_at_index(&self, _: u8) -> Option<Type> { None }
    fn get_self(&self) -> Option<&Type> { None }
    fn cached_type_retrieval<'a>(&'a self, spec: &'a TypeSpec) -> 
        BorrowedCachedTypeRetrieval<'a> {
        BorrowedCachedTypeRetrieval {
            spec,
            generics: &*self.all_generics(),
            relation: self.get_self(),
        }
    }

    fn all_generics(&self) -> &[Type];
}

impl GenericTable for Vec<Type> {
    fn get_at_index(&self, index: u8) -> Option<Type> { 
        self.get(index as usize).cloned()
    }
    fn all_generics(&self) -> &[Type] { &**self }
}

impl<'a> GenericTable for &'a Vec<Type> {
    fn get_at_index(&self, index: u8) -> Option<Type> { 
        self.get(index as usize).cloned()
    }
    fn all_generics(&self) -> &[Type] { &**self }
}

impl GenericTable for () { 
    fn all_generics(&self) -> &[Type] { &[] }
}

impl<T: GenericTable> GenericTable for (T, Type) {
    fn get_at_index(&self, index: u8) -> Option<Type> { self.0.get_at_index(index) }
    fn get_self(&self) -> Option<&Type> { Some(&self.1) }
    fn all_generics(&self) -> &[Type] { self.0.all_generics() }
}

impl GenericTable for FuncQuery {
    fn get_at_index(&self, index: u8) -> Option<Type> { 
        self.generics.get(index as usize).cloned()
    }
    fn get_self(&self) -> Option<&Type> { self.relation.inner_type() }
    fn all_generics(&self) -> &[Type] { &*self.generics }
}

impl GenericTable for (&Vec<Type>, &TypeRelation) {
    fn get_at_index(&self, index: u8) -> Option<Type> { 
        self.0.get(index as usize).cloned()
    }
    fn get_self(&self) -> Option<&Type> { self.1.inner_type() }
    fn all_generics(&self) -> &[Type] { &*self.0 }
}

impl<T: GenericTable> GenericTable for (T, Option<Type>) {
    fn get_at_index(&self, index: u8) -> Option<Type> { self.0.get_at_index(index) }
    fn get_self(&self) -> Option<&Type> { self.1.as_ref() }
    fn all_generics(&self) -> &[Type] { self.0.all_generics() }
}

#[derive(Debug)]
pub struct GetSpecReport {
    pub typ: Type,
    pub transformation: Option<TransformationList>,
}

impl CompData {
    pub fn get_spec(&self, spec: &TypeSpec, generics: &impl GenericTable) -> TResult<Type> {
        let retriever = BorrowedCachedTypeRetrieval { 
            spec,
            generics: generics.all_generics(),
            relation: generics.get_self(),
        };

        let realized_read = self.caches.realized_type_specs.read().unwrap();
        if let Some(retrieved) = realized_read.get(&retriever as &dyn CachedTypeRetrieval) {
            retrieved.clone()
        } else {
            std::mem::drop(realized_read);
            let new_type = self.get_spec_report(&spec, generics).map(|re| re.typ);
            let mut realized_cache = self.caches.realized_type_specs.write().unwrap();
            realized_cache.insert(retriever.to_owned(), new_type.clone());
            new_type
        }
    }

    pub fn ty(&self, type_spec_code: &str) -> Type {
        let type_spec = TypeSpec::from(type_spec_code);
        self.get_spec(&type_spec, &()).unwrap()
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
            TypeSpec::Deref(base) => {
                let base_type = self.get_spec(base, generics)?;
                base_type.get_deref().ok_or_else(|| TErr::CantDeref(base_type.clone()))?
            }
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
            TypeSpec::Struct(fields) | TypeSpec::Union(fields) => {
                let realized = fields
                    .iter()
                    .map(|FieldSpec { inherited, ref name, ref type_spec }| Ok(Field {
                        name: name.clone(),
                        typ: self.get_spec(&type_spec, generics)?,
                        inherited: *inherited,
                    }))
                    .collect::<Result<_, _>>()?;
                
                if matches!(spec, TypeSpec::Struct(_)) {
                    Type::new_struct(realized)
                } else {
                    Type::new_union(realized)
                }
            },
            TypeSpec::Enum(variants) => {
                Type::new_enum(variants.clone())
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
                ret_type.func_with_args(args, ABI::C)
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
                .ok_or(TErr::TooFewGenerics(spec.clone(), generics.all_generics().to_vec(), *index))?
            }
            TypeSpec::Array(base, count) => self.get_spec(base, generics)?.get_array(*count),
            TypeSpec::ArrayElem(array) => {
                let array = self.get_spec(array, generics)?;
                let TypeEnum::Array(ArrayType { base, .. }) = array.as_type_enum() 
                    else { return Err(TErr::NotAnArray(array)) };
                base.clone()
            },
            TypeSpec::RemoveWrappers(spec) => {
                let with_wrappers = self.get_spec(spec, generics)?;
                with_wrappers.remove_wrappers().clone()
            }
            TypeSpec::Destructor(base_spec, code) => {
                let base = self.get_spec(base_spec, generics)?;

                let query = FuncQuery {
                    name: VarName::None,
                    relation: TypeRelation::Unrelated,
                    generics: generics.all_generics().to_vec(),
                };

                let func_code = FuncCode {
                    args: vec![VarDecl {
                        name: VarName::from("self"),
                        type_spec: (**base_spec).clone(),
                    }],
                    code: code.clone(),
                    ..FuncCode::empty()
                };

                let (hlr, _) = crate::hlr::hlr(query, self, &func_code).unwrap();

                Type::new(TypeEnum::Destructor(DestructorType {
                    base,
                    destructor: Arc::new(hlr),
                }))
            }
            TypeSpec::Void => Type::void(),
            TypeSpec::Unknown => Type::unknown(),
            TypeSpec::Me => {
                if let Some(me) = generics.get_self() {
                    me.clone()
                } else {
                    panic!()
                }
            },
        };

        Ok(GetSpecReport { typ, transformation: None })
    }
}
