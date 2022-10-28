
use crate::lex::TypeName;
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

fn field_count(comp_data: &CompData, name: TypeName) -> Option<FuncCode> {
    let alias = TypeAlias::Named(name.clone());
    let typ = comp_data.get_spec(&alias, &Vec::new())?;
    let TypeEnum::Struct(StructType { fields, .. }) = typ.as_type_enum() 
        else { return None };
    let field_count = fields.len();

    Some(FuncCode {
        name: VarName::from("type_is_primitive"),
        ret_type: TypeAlias::Int(32),
        args: vec![VarDecl {
            name: VarName::temp(),
            typ: Some(alias),
        }],
        generic_count: 0,
        code: Expr::Return(box Expr::Number(field_count as u128)),
        method_of: Some(name),
        dependencies: Vec::new(),
    })
}

fn is_primitive(comp_data: &CompData, typ: TypeName) -> Option<FuncCode> {
    let is_prim = !matches!(typ, TypeName::Other(_));

    Some(FuncCode {
        name: VarName::from("type_is_primitive"),
        ret_type: TypeAlias::Int(32),
        args: vec![VarDecl {
            name: VarName::temp(),
            typ: Some(TypeAlias::Named(typ.clone())),
        }],
        generic_count: 0,
        code: Expr::Return(box Expr::Bool(is_prim)),
        method_of: Some(typ),
        dependencies: Vec::new(),
    })
}
