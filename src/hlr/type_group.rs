use super::prelude::*;
use crate::parse::*;
use indexmap::IndexMap;
use std::collections::HashMap;
#[derive(Debug, Default, Clone)]
pub struct TypeGroup {
    types: HashMap<String, Type>,
    generic_alias: HashMap<String, TypeAlias>,
}

impl TypeGroup {
    pub fn add(&mut self, name: String, t: Type) { self.types.insert(name, t); }

    pub fn add_generic_alias(&mut self, name: String, a: TypeAlias) {
        self.generic_alias.insert(name, a);
    }

    pub fn get_by_name(&self, name: &String) -> Option<Type> {
        self.types.get(name).cloned()
    }

    pub fn get_spec(&self, alias: &TypeAlias) -> Option<Type> {
        let empty_vec = Vec::new();
        self.get_gen_spec(alias, &empty_vec)
    }

    fn fill_gen_param_holes(
        &self,
        alias: &TypeAlias,
        generics: &Vec<TypeAlias>,
    ) -> Option<TypeAlias> {
        // TODO: fill in type aliases like &T or T[]

        match alias {
            TypeAlias::GenParam(index) => {
                return Some(generics[*index as usize].clone())
            },
            _ => {},
        }

        return Some(alias.clone());
    }

    pub fn get_gen_spec(
        &self,
        alias: &TypeAlias,
        generics: &Vec<TypeAlias>,
    ) -> Option<Type> {
        let typ = match alias {
            TypeAlias::Named(name) => self.get_by_name(&name)?,
            TypeAlias::Int(size) => Type::int_of_size(*size),
            TypeAlias::Float(size) => Type::float_of_size(*size),
            TypeAlias::Ref(base) => self.get_gen_spec(base, generics)?.get_ref(),
            TypeAlias::Struct(fields, methods) => {
                let mut typed_fields: IndexMap<String, Type> = IndexMap::new();

                for field in fields {
                    let field_type = self.get_gen_spec(field.1, generics)?;
                    typed_fields.insert(field.0.clone(), field_type);
                }

                Type::new_struct(typed_fields, methods.clone())
            },
            TypeAlias::Generic(name, generic_aliases) => {
                let generics = generic_aliases
                    .iter()
                    .map(|ga| self.fill_gen_param_holes(ga, generics).unwrap())
                    .collect();
                self.get_gen_spec(self.generic_alias.get(name)?, &generics)?
            },
            TypeAlias::GenParam(index) => {
                self.get_spec(&generics[*index as usize])?
            },
            TypeAlias::Array(base, count) => {
                self.get_gen_spec(base, generics)?.get_array(*count)
            },
        };

        Some(typ)
    }

    pub fn add_types(&mut self, rhs: &TypeGroup) {
        for t in &rhs.types {
            if self.get_by_name(&t.0).is_none() {
                self.add(t.0.clone(), t.1.clone())
            }
        }
    }
}
