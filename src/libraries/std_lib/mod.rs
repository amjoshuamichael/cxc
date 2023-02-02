use crate::{
    lex::VarName,
    parse::{Expr, FuncCode, TypeSpecRelation, VarDecl},
    typ::ArrayType,
    unit::CompData,
    Type, TypeEnum,
};

use super::{Library, TypeInterfaceLib};

pub struct StdLib;

mod bit_array;
mod default;
mod print_lib;
mod string;
mod to_string;
mod value_lib;
use bit_array::BitArrayLib;
use default::DefaultLib;
use print_lib::PrintLib;
use string::StringLib;
use to_string::ToStringLib;
use value_lib::ValueLib;

impl Library for StdLib {
    fn add_to_unit(&self, unit: &mut crate::Unit) {
        unit.add_rust_func("to_i64", [to_i64]);
        unit.add_static_deriver("len".into(), derive_array_len);

        unit.push_script(include_str!("vec.cxc")).unwrap();
        unit.push_script(include_str!("rc.cxc")).unwrap();
        unit.push_script(include_str!("lucid.cxc")).unwrap();
        unit.push_script(include_str!("option.cxc")).unwrap();

        unit.add_lib(StringLib);
        unit.add_lib(ToStringLib);
        unit.add_lib(DefaultLib);
        unit.add_lib(ValueLib);
        unit.add_lib(TypeInterfaceLib);
        unit.add_lib(PrintLib);
        unit.add_lib(BitArrayLib);

        unit.add_external_default::<bool>(Type::bool());
    }
}

fn derive_array_len(_: &CompData, typ: Type) -> Option<FuncCode> {
    let derefed_typ = typ.clone().complete_deref();
    let TypeEnum::Array(ArrayType { count, .. }) =
        derefed_typ.as_type_enum() else { return None };

    Some(FuncCode {
        name: VarName::from("len"),
        ret_type: Type::i(32).into(),
        args: vec![VarDecl {
            name: VarName::temp(),
            type_spec: typ.clone().into(),
        }],
        generic_count: 0,
        code: Expr::Number(*count as u64),
        relation: TypeSpecRelation::Static(typ.into()),
    })
}

fn to_i64(input: i32) -> i64 { input as i64 }
