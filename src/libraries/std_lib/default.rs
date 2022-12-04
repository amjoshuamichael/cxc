use crate::{
    lex::VarName,
    libraries::Library,
    parse::{Expr, FuncCode, TypeAlias, TypeOrAliasRelation},
    typ::TypeOrAlias,
    unit::CompData,
    Type, TypeEnum, TypeRelation,
};

pub(super) struct DefaultLib;

impl Library for DefaultLib {
    fn add_to_unit(&self, unit: &mut crate::Unit) {
        unit.add_static_deriver("default".into(), derive_default);

        unit.add_rust_func_explicit(
            "default",
            i32::default as *const usize,
            Type::i(32).func_with_args(Vec::new()),
            TypeRelation::Static(Type::i(32)),
            Vec::new(),
        );

        unit.add_rust_func_explicit(
            "default",
            f32::default as *const usize,
            Type::f(32).func_with_args(Vec::new()),
            TypeRelation::Static(Type::f(32)),
            Vec::new(),
        );
    }
}

fn derive_default(comp_data: &CompData, typ: Type) -> Option<FuncCode> {
    match typ.as_type_enum() {
        TypeEnum::Struct(struct_type) => {
            let mut fields = Vec::<(VarName, Expr)>::new();

            for (field_name, field_type) in &struct_type.fields {
                let other_default_call = Expr::Call(
                    box Expr::StaticMethodPath(
                        field_type.clone().into(),
                        "default".into(),
                    ),
                    Vec::new(),
                    Vec::new(),
                    false,
                );

                fields.push((field_name.clone(), other_default_call));
            }

            let struct_lit = Expr::Struct(typ.clone().into(), fields, false);

            let relation =
                TypeOrAliasRelation::Static(TypeOrAlias::Type(typ.clone()));

            Some(FuncCode {
                name: VarName::from("default"),
                ret_type: typ.into(),
                args: Vec::new(),
                generic_count: 0,
                code: struct_lit,
                relation,
            })
        },
        _ => None,
    }
}
