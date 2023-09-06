use crate::{Type, parse::TypeSpec, TypeEnum, StructType, IntType, RefType, FuncType, ArrayType, TypeName};

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
        TypeEnum::Int(IntType { signed: true, size }) => TypeSpec::Int(*size),
        TypeEnum::Int(IntType { signed: false, size }) => TypeSpec::UInt(*size),
        TypeEnum::Float(float_type) => TypeSpec::Float(*float_type),
        TypeEnum::Struct(StructType { fields, .. }) => TypeSpec::Struct(
            fields.iter().map(|(name, typ)| (
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
