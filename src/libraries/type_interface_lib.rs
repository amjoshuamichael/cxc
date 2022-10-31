use crate::lex::VarName;
use crate::parse::Expr;
use crate::parse::FuncCode;
use crate::parse::TypeAlias;
use crate::Type;
use crate::TypeEnum;

use crate::parse::VarDecl;
use crate::typ::StructType;
use crate::unit::CompData;

use super::Library;

pub struct TypeInterfaceLib;

impl Library for TypeInterfaceLib {
    fn add_to_unit(&self, unit: &mut crate::Unit) {
        unit.add_deriver(VarName::from("type_field_count"), field_count);
    }
}

fn field_count(_: &CompData, typ: Type) -> Option<FuncCode> {
    let TypeEnum::Struct(StructType { fields, .. }) = 
        typ.clone().complete_deref().as_type_enum() else { return None };
    let field_count = fields.len();

    Some(FuncCode {
        name: VarName::from("type_field_count"),
        ret_type: TypeAlias::Int(32),
        args: vec![VarDecl {
            name: VarName::temp(),
            typ: Some(typ.clone().into()),
        }],
        generic_count: 0,
        code: Expr::Return(box Expr::Number(field_count as u128)),
        method_of: Some(typ.into()),
    })
}
