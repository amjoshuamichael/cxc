use crate::lex::VarName;
use crate::parse::{Expr, Opcode, TypeAlias, VarDecl};
use crate::typ::StructType;

use crate::TypeEnum;
use crate::{parse::FuncCode, unit::CompData, Type};

use crate::libraries::Library;

pub(super) struct ToStringLib;

impl Library for ToStringLib {
    fn add_to_unit(&self, unit: &mut crate::Unit) {
        unit.add_deriver("to_string".into(), derive_to_string);

        let string_type = unit.comp_data.get_by_name(&"String".into()).unwrap();

        unit.add_rust_func_explicit(
            "to_string",
            to_string::<i32> as *const usize,
            string_type
                .clone()
                .func_with_args(vec![Type::i(32).get_ref()]),
            Some(Type::i(32).get_ref()),
            Vec::new(),
        );

        unit.add_rust_func_explicit(
            "to_string",
            to_string::<f32> as *const usize,
            string_type
                .clone()
                .func_with_args(vec![Type::f(32).get_ref()]),
            Some(Type::f(32).get_ref()),
            Vec::new(),
        );
    }
}

pub fn derive_to_string(comp_data: &CompData, typ: Type) -> Option<FuncCode> {
    let string_type = comp_data.get_by_name(&"String".into()).unwrap();
    let prefix = typ
        .name()
        .clone()
        .map(|n| n.to_string() + " {")
        .unwrap_or(String::from("{"));

    let to_string = VarName::from("to_string");
    let input_var = VarName::from("input");

    let expr = match typ.clone().get_deref().unwrap().as_type_enum() {
        TypeEnum::Struct(StructType { fields, .. }) => {
            let output_var = VarName::from("output");
            let output_var_expr = Expr::Ident(output_var.clone());
            let output_var_ref =
                Expr::UnarOp(Opcode::Ref(1), box output_var_expr.clone());

            let mut statements = Vec::new();

            let make_var = Expr::MakeVar(
                VarDecl {
                    name: output_var.clone(),
                    typ: Some(string_type.into()),
                },
                box Expr::Strin(prefix),
            );
            statements.push(make_var);

            let push_string = VarName::from("push_string");

            for (field_index, field) in fields.iter().enumerate() {
                let field_prefix = if field_index > 0 {
                    String::from(", ") + &*field.0.to_string() + " = "
                } else {
                    field.0.to_string() + " = "
                };

                let field_prefix_expr = Expr::Strin(field_prefix);

                let push_prefix_call = Expr::Call(
                    push_string.clone(),
                    Vec::new(),
                    vec![field_prefix_expr, output_var_ref.clone()],
                    true,
                );
                statements.push(push_prefix_call);

                let field = Expr::Member(
                    box Expr::Ident(input_var.clone()),
                    field.0.clone(),
                );

                let field_ref = Expr::UnarOp(Opcode::Ref(1), box field);

                let field_to_string =
                    Expr::Call(to_string.clone(), Vec::new(), vec![field_ref], true);
                let push_string_call = Expr::Call(
                    push_string.clone(),
                    Vec::new(),
                    vec![field_to_string.clone(), output_var_ref.clone()],
                    true,
                );
                statements.push(push_string_call);
            }

            let push_closer_call = Expr::Call(
                push_string.clone(),
                Vec::new(),
                vec![Expr::Strin("}".into()), output_var_ref.clone()],
                true,
            );
            statements.push(push_closer_call);

            let ret = Expr::Return(box output_var_expr.clone());
            statements.push(ret);
            Expr::Block(statements)
        },
        _ => return None,
    };

    Some(FuncCode {
        name: to_string.clone(),
        ret_type: TypeAlias::Named("String".into()),
        args: vec![VarDecl {
            name: input_var,
            typ: Some(typ.clone().into()),
        }],
        generic_count: 0,
        code: expr,
        method_of: Some(typ.into()),
    })
}

fn to_string<T: ToString>(val: &T) -> String { val.to_string() }
