use crate::{
    lex::VarName,
    parse::{Expr, FuncCode, TypeAlias, VarDecl},
    typ::ArrayType,
    unit::CompData,
    Type, TypeEnum,
};

use super::Library;

pub struct StdLib;
mod to_string;
use to_string::ToStringLib;

impl Library for StdLib {
    fn add_to_unit(&self, unit: &mut crate::Unit) {
        unit.add_rust_func("to_i64", [to_i64]);
        unit.add_deriver("len".into(), derive_array_len)
            .push_script(include_str!("vec.cxc"));
        unit.push_script(include_str!("string.cxc"));
        unit.add_lib(ToStringLib);
    }
}

fn derive_array_len(_: &CompData, typ: Type) -> Option<FuncCode> {
    let TypeEnum::Array(ArrayType { count, .. }) =
        typ.clone().complete_deref().as_type_enum() else { return None };

    Some(FuncCode {
        name: VarName::from("len"),
        ret_type: TypeAlias::Int(32),
        args: vec![VarDecl {
            name: VarName::temp(),
            typ: Some(typ.clone().into()),
        }],
        generic_count: 0,
        code: Expr::Return(box Expr::Number(*count as u128)),
        method_of: Some(typ.into()),
    })
}

fn to_i64(input: i32) -> i64 { input as i64 }
