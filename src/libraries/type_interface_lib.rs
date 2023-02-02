use crate::lex::VarName;
use crate::parse::Expr;
use crate::parse::FuncCode;
use crate::Type;
use crate::TypeEnum;

use crate::parse::TypeSpec;
use crate::parse::TypeSpecRelation;
use crate::parse::VarDecl;
use crate::typ::StructType;
use crate::unit::CompData;

use super::Library;

pub struct TypeInterfaceLib;

impl Library for TypeInterfaceLib {
    fn add_to_unit(&self, unit: &mut crate::Unit) {
        unit.add_method_deriver(VarName::from("type_field_count"), field_count);
        unit.add_static_deriver(VarName::from("size_in_bytes"), size_in_bytes);
    }
}

fn field_count(_: &CompData, typ: Type) -> Option<FuncCode> {
    let derefed_typ = typ.clone().complete_deref();
    let TypeEnum::Struct(StructType { fields, .. }) = 
        derefed_typ.as_type_enum() else { return None };
    let field_count = fields.len();

    Some(FuncCode {
        name: VarName::from("type_field_count"),
        ret_type: Type::i(32).into(),
        args: vec![VarDecl {
            name: VarName::temp(),
            type_spec: typ.clone().into(),
        }],
        generic_count: 0,
        code: Expr::Number(field_count as u64).wrap_in_block(),
        relation: TypeSpecRelation::MethodOf(TypeSpec::Type(typ.into())),
    })
}

fn size_in_bytes(_: &CompData, typ: Type) -> Option<FuncCode> {
    Some(FuncCode {
        name: VarName::from("size_in_bytes"),
        ret_type: Type::i(32).into(),
        args: Vec::new(),
        generic_count: 0,
        code: Expr::Number(typ.size() as u64).wrap_in_block(),
        relation: TypeSpecRelation::Static(TypeSpec::Type(typ.into())),
    })
}
