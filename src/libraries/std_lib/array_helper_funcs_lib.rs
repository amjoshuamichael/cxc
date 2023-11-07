use crate::libraries::Library;
use crate::parse::{InitOpts, VarDecl};
use crate::RefType;
use crate::typ::ABI;
use crate::typ::spec_from_type::type_to_type_spec;
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
    }
}

fn derive_array_len(_: &CompData, typ: Type) -> Option<FuncCode> {
    let derefed_typ = typ.clone().complete_deref();
    let TypeEnum::Array(ArrayType { count, .. }) =
        derefed_typ.as_type_enum() else { return None };

    Some(FuncCode {
        name: VarName::from("len"),
        ret_type: TypeSpec::Int(64),
        args: vec![],
        generic_count: 0,
        code: Expr::TypedValue(
            TypeSpec::Int(64), 
            Box::new(Expr::Number(*count as u64))
        ).wrap(),
        relation: TypeSpecRelation::Static(type_to_type_spec(typ)),
        is_external: false,
        abi: ABI::C,
    })
}
