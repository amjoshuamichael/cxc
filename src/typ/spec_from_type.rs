use crate::{Type, parse::TypeSpec, TypeEnum, StructType, IntType, RefType, FuncType, ArrayType, TypeName};

use super::Field;

pub fn type_to_type_spec(typ: Type) -> TypeSpec {
    if typ.name() != &TypeName::Anonymous {
        if typ.generics().len() == 0 {
            return TypeSpec::Named(typ.name().clone());
        } else {
            return TypeSpec::Generic(
                typ.name().clone(),
                typ.generics().iter().cloned().map(type_to_type_spec).collect(),
            );
        }
    }

    match typ.as_type_enum() {
        TypeEnum::Int(IntType { signed: true, size }) => TypeSpec::Int(size.to_num() as u32),
        TypeEnum::Int(IntType { signed: false, size }) => TypeSpec::UInt(size.to_num() as u32),
        TypeEnum::Float(float_type) => TypeSpec::Float(*float_type),
        TypeEnum::Struct(StructType { fields, .. }) => TypeSpec::Struct(
            fields.iter().map(|Field { name, typ, inherited }| (
                *inherited,
                name.clone(),
                type_to_type_spec(typ.clone()),
            )).collect()
        ),
        TypeEnum::Ref(RefType { base }) => TypeSpec::Ref(
            Box::new(type_to_type_spec(base.clone()))
        ),
        TypeEnum::Func(FuncType { args, ret }) => TypeSpec::Function(
            args.iter().cloned().map(type_to_type_spec).collect(),
            Box::new(type_to_type_spec(ret.clone())),
        ),
        TypeEnum::Array(ArrayType { base, count }) => TypeSpec::Array(
            Box::new(type_to_type_spec(base.clone())), 
            *count,
        ),
        TypeEnum::Bool => TypeSpec::Bool,
        TypeEnum::Void => TypeSpec::Void,
        TypeEnum::Unknown => panic!(),
    }
}
