use crate::{parse::TypeAlias, unit::CompData, Type};
use std::convert::From;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeOrAlias {
    Type(Type),
    Alias(TypeAlias),
}

impl TypeOrAlias {
    pub fn into_type(&self, comp_data: &CompData) -> Option<Type> {
        use TypeOrAlias::*;

        match self {
            Type(typ) => Some(typ.clone()),
            Alias(alias) => comp_data.get_spec(alias, &Vec::new()),
        }
    }

    pub fn into_type_with_generics(
        &self,
        comp_data: &CompData,
        generics: &Vec<Type>,
    ) -> Option<Type> {
        use TypeOrAlias::*;

        match self {
            Type(typ) => Some(typ.clone()),
            Alias(alias) => comp_data.get_spec(alias, generics),
        }
    }
}

impl From<Type> for TypeOrAlias {
    fn from(typ: Type) -> Self { Self::Type(typ) }
}

impl From<TypeAlias> for TypeOrAlias {
    fn from(alias: TypeAlias) -> Self { Self::Alias(alias) }
}
