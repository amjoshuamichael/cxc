use crate::{
    lex::VarName,
    libraries::Library,
    parse::{Expr, FuncCode, InitOpts, TypeSpec, TypeSpecRelation},
    unit::CompData,
    ExternalFuncAdd, Type, TypeEnum, TypeRelation, typ::Field,
};

pub(super) struct DefaultLib;

impl Library for DefaultLib {
    fn add_to_unit(&self, unit: &mut crate::Unit) {
        unit.add_static_deriver("default".into(), derive_default);

        // TODO: write this in actual cxc code
        unit.add_external_default::<i8>();
        unit.add_external_default::<i16>();
        unit.add_external_default::<i32>();
        unit.add_external_default::<i64>();
        unit.add_external_default::<u8>();
        unit.add_external_default::<u16>();
        unit.add_external_default::<u32>();
        unit.add_external_default::<u64>();
        unit.add_external_default::<f32>();
        unit.add_external_default::<f64>();
        unit.add_external_default::<bool>();
    }
}

fn derive_default(_: &CompData, typ: Type) -> Option<FuncCode> {
    match typ.as_type_enum() {
        TypeEnum::Struct(struct_type) => {
            let mut fields = Vec::<(VarName, Expr)>::new();

            for Field { name: field_name, typ: field_type, .. } in &struct_type.fields {
                let other_default_call = Expr::Call {
                    func: Box::new(Expr::StaticMethodPath(
                        field_type.clone().into(),
                        "default".into(),
                    )),
                    generics: Vec::new(),
                    args: Vec::new(),
                    is_method: false,
                };

                fields.push((field_name.clone(), other_default_call));
            }

            let struct_lit = Expr::TypedValue(
                typ.clone().into(),
                Box::new(Expr::Struct(fields, InitOpts::NoFill)),
            );

            let relation = TypeSpecRelation::Static(TypeSpec::Type(typ.clone()));

            Some(FuncCode {
                name: VarName::from("default"),
                ret_type: typ.into(),
                args: Vec::new(),
                generic_count: 0,
                code: struct_lit.wrap_in_block(),
                relation,
                is_external: false,
            })
        },
        _ => None,
    }
}
