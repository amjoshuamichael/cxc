use crate::lex::{TypeName, VarName};
use crate::parse::*;
use crate::Type;
use indexmap::IndexMap;
use std::collections::HashMap;

mod rust_type_name_conversion;

#[derive(Debug, Default, Clone)]
pub struct TypeGroup {
    types: HashMap<TypeName, Type>,
    aliases: HashMap<TypeName, TypeAlias>,
}

impl TypeGroup {
    pub fn add(&mut self, name: TypeName, a: TypeAlias) {
        self.aliases.insert(name, a);
    }

    pub fn contains(&self, key: &TypeName) -> bool { self.aliases.contains_key(key) }

    pub fn get_by_name(&self, name: &TypeName) -> Option<Type> {
        {
            let cached_type = self.types.get(name);
            if cached_type.is_some() {
                return cached_type.cloned();
            }
        }

        let alias = self.aliases.get(name)?;
        let realized_type = self.get_spec(alias, &vec![])?;
        Some(realized_type)
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

    pub fn get_spec(
        &self,
        alias: &TypeAlias,
        generics: &Vec<TypeAlias>,
    ) -> Option<Type> {
        let typ = match alias {
            TypeAlias::Named(name) => self.get_by_name(&name)?,
            TypeAlias::Int(size) => Type::i(*size),
            TypeAlias::Float(size) => Type::f(*size),
            TypeAlias::Ref(base) => self.get_spec(base, generics)?.get_ref(),
            TypeAlias::Struct(fields, methods) => {
                let mut typed_fields: IndexMap<VarName, Type> = IndexMap::new();

                for field in fields {
                    let field_type = self.get_spec(field.1, generics)?;
                    typed_fields.insert(field.0.clone(), field_type);
                }

                Type::new_struct(typed_fields, methods.clone())
            },
            TypeAlias::Generic(name, generic_aliases) => {
                let generics = generic_aliases
                    .iter()
                    .map(|ga| self.fill_gen_param_holes(ga, generics).unwrap())
                    .collect();
                self.get_spec(self.aliases.get(name)?, &generics)?
            },
            TypeAlias::GenParam(index) => {
                self.get_spec(&generics[*index as usize], &vec![])?
            },
            TypeAlias::Array(base, count) => {
                self.get_spec(base, generics)?.get_array(*count)
            },
        };

        Some(typ)
    }

    pub fn add_types(&mut self, rhs: &TypeGroup) {
        for t in &rhs.aliases {
            if !self.contains(&t.0) {
                self.add(t.0.clone(), t.1.clone())
            }
        }
    }
}
