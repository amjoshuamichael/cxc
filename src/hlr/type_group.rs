use super::prelude::*;
use crate::parse::*;
use indexmap::IndexMap;
use std::collections::HashMap;
use std::sync::Arc;

#[derive(Debug, Default, Clone)]
pub struct TypeGroup(pub HashMap<String, Type>);

impl TypeGroup {
    pub fn add(&mut self, name: String, t: Type) {
        self.0.insert(name, t);
    }

    pub fn get_by_name(&self, name: &String) -> Option<Type> {
        self.0.get(name).cloned()
    }

    pub fn get_spec(&self, alias: &TypeAlias) -> Option<Type> {
        dbg!(alias);
        let typ = match alias {
            TypeAlias::Named(name) => self.get_by_name(&name)?,
            TypeAlias::Int(size) => Type::int_of_size(*size),
            TypeAlias::Float(size) => Type::float_of_size(*size),
            TypeAlias::Ref(base) => self.get_spec(base)?.get_ref(),
            TypeAlias::Struct(fields) => {
                let mut typed_fields: IndexMap<String, Type> = IndexMap::new();

                for field in fields {
                    let field_type = self.get_spec(field.1)?;
                    typed_fields.insert(field.0.clone(), field_type);
                }

                Type::new_struct(typed_fields)
            },
        };

        Some(typ)
    }

    pub fn add_types(&mut self, rhs: &TypeGroup) {
        for t in &rhs.0 {
            if self.get_by_name(&t.0).is_none() {
                self.add(t.0.clone(), t.1.clone())
            }
        }
    }
}
