use crate::lex::{TypeName, VarName};
use crate::parse::*;
use crate::unit::CompData;
use crate::Type;

mod rust_type_name_conversion;

impl<'a> CompData<'a> {
    pub fn add_type_alias(&mut self, name: TypeName, a: TypeAlias) {
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
        Some(realized_type.with_name(name.clone()))
    }

    pub fn get_spec(&self, alias: &TypeAlias, generics: &Vec<Type>) -> Option<Type> {
        let typ = match alias {
            TypeAlias::Named(name) => self.get_by_name(name)?,
            TypeAlias::Int(size) => Type::i(*size),
            TypeAlias::Float(size) => Type::f(*size),
            TypeAlias::Bool => Type::bool(),
            TypeAlias::Ref(base) => self.get_spec(base, generics)?.get_ref(),
            TypeAlias::Struct(fields, methods) => {
                let mut typed_fields: Vec<(VarName, Type)> = Vec::new();

                for field in fields {
                    let field_type = self.get_spec(&field.1, generics)?;
                    typed_fields.push((field.0.clone(), field_type));
                }

                Type::new_struct(typed_fields, methods.to_vec())
            },
            TypeAlias::Function(args, ret_type) => {
                let args = args
                    .iter()
                    .map(|arg| self.get_spec(arg, generics))
                    .collect::<Option<Vec<Type>>>()?;
                let ret_type = self.get_spec(ret_type, generics)?;
                ret_type.func_with_args(args)
            },
            TypeAlias::Generic(name, generic_aliases) => {
                let generics = generic_aliases
                    .iter()
                    .map(|ga| self.get_spec(ga, generics).unwrap())
                    .collect();
                self.get_spec(self.aliases.get(name)?, &generics)?
                    .with_name(name.clone())
            },
            TypeAlias::GenParam(index) => generics[*index as usize].clone(),
            TypeAlias::Array(base, count) => {
                self.get_spec(base, generics)?.get_array(*count)
            },
        };

        Some(typ)
    }
}
