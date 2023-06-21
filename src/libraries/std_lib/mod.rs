use crate::{
    lex::VarName,
    parse::{Expr, FuncCode, TypeSpecRelation, TypeSpec},
    typ::ArrayType,
    unit::CompData,
    Type, TypeEnum,
};

use super::{Library, TypeInterfaceLib};

pub struct StdLib;

mod atomic_lib;
pub mod bit_array;
mod default;
pub mod hash;
mod print_lib;
mod string;
mod to_string;
mod type_helpers;
mod unit_lib;
mod drop_lib;
mod value_lib;
use atomic_lib::AtomicLib;
use bit_array::BitArrayLib;
use default::DefaultLib;
use print_lib::PrintLib;
use string::StringLib;
use to_string::ToStringLib;
use type_helpers::TypeHelperLib;
use unit_lib::UnitLib;
use value_lib::ValueLib;
use drop_lib::DropLib;

impl Library for StdLib {
    fn add_to_unit(&self, unit: &mut crate::Unit) {
        // TODO: this is a hack
        unit.push_script("str = u8").unwrap();

        unit.add_static_deriver("len".into(), derive_array_len);

        unit.push_script(include_str!("drop.cxc")).unwrap();
        unit.add_lib(DropLib);

        unit.push_script(include_str!("vec.cxc")).unwrap();
        unit.push_script(include_str!("hashmap.cxc")).unwrap();
        unit.push_script(include_str!("rc.cxc")).unwrap();
        unit.push_script(include_str!("lucid.cxc")).unwrap();
        unit.push_script(include_str!("option.cxc")).unwrap();
        unit.push_script(include_str!("../array_ref.cxc")).unwrap();

        unit.add_lib(StringLib);
        unit.add_lib(ToStringLib);
        unit.add_lib(DefaultLib);
        unit.add_lib(TypeInterfaceLib);
        unit.add_lib(PrintLib);
        unit.add_lib(BitArrayLib);
        unit.add_lib(TypeHelperLib);

        unit.add_lib(AtomicLib);
        unit.push_script(include_str!("arc.cxc")).unwrap();

        unit.add_lib(UnitLib);
        unit.add_lib(ValueLib);

        unit.add_external_default::<bool>();
    }
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
