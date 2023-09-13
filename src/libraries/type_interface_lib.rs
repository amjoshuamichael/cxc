use crate::lex::VarName;
use crate::parse::Expr;
use crate::parse::FuncCode;
use crate::Type;

use crate::parse::TypeSpec;
use crate::parse::TypeSpecRelation;
use crate::typ::spec_from_type::type_to_type_spec;
use crate::unit::CompData;

use super::Library;

pub struct TypeInterfaceLib;

impl Library for TypeInterfaceLib {
    fn add_to_unit(&self, unit: &mut crate::Unit) {
        unit.add_static_deriver(VarName::from("size_in_bytes"), size_in_bytes);

        unit.add_type_level_func("EraseDataU8".into(), erase_data_u8);
    }
}

fn erase_data_u8(input: Vec<Type>, _: &CompData) -> Type {
    Type::u(8).get_array(input[0].size() as u32)
}

fn size_in_bytes(_: &CompData, typ: Type) -> Option<FuncCode> {
    Some(FuncCode {
        name: VarName::from("size_in_bytes"),
        ret_type: TypeSpec::Int(32),
        args: Vec::new(),
        generic_count: 0,
        code: Expr::Number(typ.size() as u64).wrap_in_block(),
        relation: TypeSpecRelation::Static(type_to_type_spec(typ)),
        is_external: false,
    })
}
