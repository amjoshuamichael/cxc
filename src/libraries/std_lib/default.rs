use crate::{
    lex::VarName,
    libraries::Library,
    parse::{Expr, FuncCode, InitOpts, TypeSpecRelation},
    unit::CompData, Type, TypeEnum, typ::{Field, spec_from_type::type_to_type_spec, ABI},
};

pub(super) struct DefaultLib;

impl Library for DefaultLib {
    fn add_to_unit(&self, unit: &mut crate::Unit) {
        unit.add_static_deriver("default".into(), derive_default);

        unit.push_script(include_str!("default.cxc"));
    }
}

fn derive_default(_: &CompData, typ: Type) -> Option<FuncCode> {
    let type_spec = type_to_type_spec(typ.clone());
    match typ.as_type_enum() {
        TypeEnum::Struct(struct_type) => {
            let mut fields = Vec::<(VarName, Expr)>::new();

            for Field { name: field_name, typ: field_type, .. } in &struct_type.fields {
                let other_default_call = Expr::Call {
                    func: Box::new(Expr::StaticMethodPath(
                        type_to_type_spec(field_type.clone()),
                        "default".into(),
                    )),
                    generics: Vec::new(),
                    args: Vec::new(),
                    is_method: false,
                };

                fields.push((field_name.clone(), other_default_call));
            }

            let struct_lit = Expr::TypedValue(
                type_spec.clone(),
                Box::new(Expr::Struct(fields, InitOpts::NoFill)),
            );

            let relation = TypeSpecRelation::Static(type_spec.clone());

            Some(FuncCode {
                name: VarName::from("default"),
                ret_type: type_spec.clone(),
                args: Vec::new(),
                generic_count: 0,
                code: struct_lit.wrap(),
                relation,
                is_external: false,
                abi: ABI::C,
            })
        },
        _ => None,
    }
}
