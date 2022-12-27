use crate::lex::{TypeName, VarName};
use crate::unit::CompData;
use crate::Type;
use crate::{parse::*, TypeEnum};

mod rust_type_name_conversion;

impl<'a> CompData<'a> {
    pub fn add_type_alias(&mut self, name: TypeName, a: TypeSpec) {
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

    pub fn get_spec(&self, alias: &TypeSpec, generics: &Vec<Type>) -> Option<Type> {
        let typ = match alias {
            TypeSpec::Named(name) => self.get_by_name(name)?,
            TypeSpec::Int(size) => Type::i(*size),
            TypeSpec::Float(size) => Type::f(*size),
            TypeSpec::Bool => Type::bool(),
            TypeSpec::Ref(base) => self.get_spec(base, generics)?.get_ref(),
            TypeSpec::Struct(fields) => {
                let mut typed_fields: Vec<(VarName, Type)> = Vec::new();

                for field in fields {
                    let field_type = self.get_spec(&field.1, generics)?;
                    typed_fields.push((field.0.clone(), field_type));
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
            TypeSpec::Generic(name, generic_aliases) => {
                let generics = generic_aliases
                    .iter()
                    .map(|ga| self.get_spec(ga, generics).unwrap())
                    .collect();
                let cached_type = self.types.get(name);
                if cached_type.is_some() {
                    cached_type.unwrap().clone()
                } else {
                    self.get_spec(self.aliases.get(name)?, &generics)?
                        .with_name(name.clone())
                }
            },
            TypeSpec::GenParam(index) => generics[*index as usize].clone(),
            TypeSpec::Array(base, count) => self.get_spec(base, generics)?.get_array(*count),
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
        };

        Some(typ)
    }
}
