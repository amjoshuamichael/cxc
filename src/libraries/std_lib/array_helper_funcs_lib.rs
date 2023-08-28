use crate::libraries::Library;
use crate::parse::{InitOpts, VarDecl};
use crate::RefType;
use crate::{
    lex::VarName,
    parse::{Expr, FuncCode, TypeSpecRelation, TypeSpec},
    typ::ArrayType,
    unit::CompData,
    Type, TypeEnum,
};

pub struct ArrayHelperLib;

impl Library for ArrayHelperLib {
    fn add_to_unit(&self, unit: &mut crate::Unit) {
        unit.add_static_deriver("len".into(), derive_array_len);
        unit.add_method_deriver("len".into(), derive_array_len_m); // TODO: remove
        unit.add_method_deriver("deref".into(), derive_array_deref);
    }
}

fn derive_array_deref(_: &CompData, typ: Type) -> Option<FuncCode> {
    let TypeEnum::Ref(RefType { base: ref_base, .. }) =
        typ.as_type_enum() else { return None };
    let TypeEnum::Array(ArrayType { base: array_base, count, .. }) =
        ref_base.as_type_enum() else { return None };

    Some(FuncCode {
        name: VarName::from("deref"),
        ret_type: TypeSpec::Generic("Slice".into(), vec![array_base.clone().into()]),
        args: vec![VarDecl {
            name: "self".into(),
            type_spec: typ.clone().into(),
        }],
        generic_count: 0,
        code: Expr::Struct(
            vec![
                ("ptr".into(), Expr::Ident("self".into())),
                (
                    "len".into(), 
                    Expr::TypedValue(
                        TypeSpec::UInt(64), 
                        Box::new(Expr::Number(*count as u64)),
                    ),
                ),
            ], 
            InitOpts::NoFill
        ).wrap_in_block(),
        relation: TypeSpecRelation::MethodOf(typ.into()),
    })
}

fn derive_array_len(_: &CompData, typ: Type) -> Option<FuncCode> {
    let derefed_typ = typ.clone().complete_deref();
    let TypeEnum::Array(ArrayType { count, .. }) =
        derefed_typ.as_type_enum() else { return None };

    Some(FuncCode {
        name: VarName::from("len"),
        ret_type: Type::i(64).into(),
        args: vec![],
        generic_count: 0,
        code: Expr::TypedValue(
            TypeSpec::Int(64), 
            Box::new(Expr::Number(*count as u64))
        ).wrap_in_block(),
        relation: TypeSpecRelation::Static(typ.into()),
    })
}

fn derive_array_len_m(_: &CompData, typ: Type) -> Option<FuncCode> {
    let derefed_typ = typ.clone().complete_deref();
    let TypeEnum::Array(ArrayType { count, .. }) =
        derefed_typ.as_type_enum() else { return None };

    Some(FuncCode {
        name: VarName::from("len"),
        ret_type: Type::i(64).into(),
        args: vec![VarDecl {
            name: "self".into(),
            type_spec: typ.clone().into(),
        }],
        generic_count: 0,
        code: Expr::TypedValue(
            TypeSpec::Int(64), 
            Box::new(Expr::Number(*count as u64))
        ).wrap_in_block(),
        relation: TypeSpecRelation::Static(typ.into()),
    })
}
