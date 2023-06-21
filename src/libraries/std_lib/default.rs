use crate::{
    lex::VarName,
    libraries::Library,
    parse::{Expr, FuncCode, InitOpts, TypeSpec, TypeSpecRelation},
    unit::CompData,
    ExternalFuncAdd, Type, TypeEnum, TypeRelation,
};

pub(super) struct DefaultLib;

impl Library for DefaultLib {
    fn add_to_unit(&self, unit: &mut crate::Unit) {
        unit.add_static_deriver("default".into(), derive_default);

        unit.add_rust_func_explicit(
            "default",
            i32::default as *const usize,
            ExternalFuncAdd {
                ret_type: Type::i(32),
                relation: TypeRelation::Static(Type::i(32)),
                ..ExternalFuncAdd::empty()
            },
        );

        unit.add_rust_func_explicit(
            "default",
            f32::default as *const usize,
            ExternalFuncAdd {
                ret_type: Type::f(32),
                relation: TypeRelation::Static(Type::f(32)),
                ..ExternalFuncAdd::empty()
            },
        );
    }
}

fn derive_default(_: &CompData, typ: Type) -> Option<FuncCode> {
    match typ.as_type_enum() {
        TypeEnum::Struct(struct_type) => {
            let mut fields = Vec::<(VarName, Expr)>::new();

            for (field_name, field_type) in &struct_type.fields {
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
            })
        },
        _ => None,
    }
}
