use crate::lex::VarName;
use crate::parse::{Expr, Opcode, TypeRelation, TypeSpec, TypeSpecRelation, VarDecl};
use crate::typ::{ArrayType, StructType};

use crate::{parse::FuncCode, unit::CompData, Type};
use crate::{ExternalFuncAdd, TypeEnum};

use crate::libraries::Library;

pub(super) struct ToStringLib;

impl Library for ToStringLib {
    fn add_to_unit(&self, unit: &mut crate::Unit) {
        unit.add_method_deriver("to_string".into(), derive_to_string);

        let string_type = unit.comp_data.get_by_name(&"String".into()).unwrap();
        unit.add_rust_func_explicit(
            "to_string",
            to_string::<i32> as *const usize,
            ExternalFuncAdd {
                ret_type: string_type.clone(),
                arg_types: vec![Type::i(32).get_ref()],
                relation: TypeRelation::MethodOf(Type::i(32).get_ref()),
                ..ExternalFuncAdd::empty()
            },
        );
        unit.add_rust_func_explicit(
            "to_string",
            to_string::<f32> as *const usize,
            ExternalFuncAdd {
                ret_type: string_type.clone(),
                arg_types: vec![Type::f(32).get_ref()],
                relation: TypeRelation::MethodOf(Type::f(32).get_ref()),
                ..ExternalFuncAdd::empty()
            },
        );
        unit.add_rust_func_explicit(
            "to_string",
            string_to_string as *const usize,
            ExternalFuncAdd {
                ret_type: string_type.clone(),
                arg_types: vec![string_type.get_ref()],
                relation: TypeRelation::MethodOf(string_type.get_ref()),
                ..ExternalFuncAdd::empty()
            },
        );
    }
}

pub fn derive_to_string(comp_data: &CompData, typ: Type) -> Option<FuncCode> {
    let string_type = comp_data.get_by_name(&"String".into()).unwrap();

    let to_string = box Expr::Ident(VarName::from("to_string"));
    let input_var = box Expr::Ident(VarName::from("input"));
    let push_string = box Expr::Ident(VarName::from("push_string"));

    let expr = match typ.clone().get_deref().unwrap().as_type_enum() {
        TypeEnum::Struct(StructType { fields, .. }) => {
            let prefix = typ
                .clone()
                .complete_deref()
                .name()
                .clone()
                .map(|n| n.to_string() + " {")
                .unwrap_or(String::from("{"));

            let output_var = VarName::from("output");
            let output_var_expr = Expr::Ident(output_var.clone());
            let output_var_ref = output_var_expr.get_ref();

            let mut statements = Vec::new();
            let make_var = Expr::MakeVar(
                VarDecl {
                    name: output_var.clone(),
                    type_spec: Some(string_type.into()),
                },
                box Expr::Strin(prefix),
            );
            statements.push(make_var);

            for (field_index, (field_name, _)) in fields.iter().enumerate() {
                let field_prefix = if field_index > 0 {
                    String::from(", ") + &*field_name.to_string() + " = "
                } else {
                    field_name.to_string() + " = "
                };

                let field_prefix_expr = Expr::Strin(field_prefix);

                let push_prefix_call = Expr::Call(
                    push_string.clone(),
                    Vec::new(),
                    vec![field_prefix_expr.get_ref(), output_var_ref.clone()],
                    true,
                );
                statements.push(push_prefix_call);

                let field = Expr::Member(input_var.clone(), field_name.clone());

                let field_to_string =
                    Expr::Call(to_string.clone(), Vec::new(), vec![field.get_ref()], true);
                let push_string_call = Expr::Call(
                    push_string.clone(),
                    Vec::new(),
                    vec![field_to_string.get_ref(), output_var_ref.clone()],
                    true,
                );
                statements.push(push_string_call);
            }

            let push_closer_call = Expr::Call(
                push_string.clone(),
                Vec::new(),
                vec![Expr::Strin("}".into()).get_ref(), output_var_ref.clone()],
                true,
            );
            statements.push(push_closer_call);

            let ret = Expr::Return(box output_var_expr.clone());
            statements.push(ret);
            Expr::Block(statements)
        },
        TypeEnum::Array(ArrayType { count, .. }) => {
            let output_var = VarName::from("output");
            let output_var_expr = Expr::Ident(output_var.clone());
            let output_var_ref = Expr::UnarOp(Opcode::Ref(1), box output_var_expr.clone());

            let mut statements = Vec::new();
            let make_var = Expr::MakeVar(
                VarDecl {
                    name: output_var.clone(),
                    type_spec: Some(string_type.into()),
                },
                box Expr::Strin("[".into()),
            );
            statements.push(make_var);

            let push_string = box Expr::Ident(VarName::from("push_string"));

            for index in 0..*count {
                let comma = if index > 0 {
                    String::from(", ")
                } else {
                    String::new()
                };

                let comma_expr = Expr::Strin(comma);

                let push_comma_call = Expr::Call(
                    push_string.clone(),
                    Vec::new(),
                    vec![comma_expr, output_var_ref.clone()],
                    true,
                );
                statements.push(push_comma_call);

                let index_to_string = Expr::Call(
                    to_string.clone(),
                    Vec::new(),
                    vec![Expr::UnarOp(
                        Opcode::Ref(1),
                        box Expr::Index(
                            box Expr::UnarOp(Opcode::Deref(1), input_var.clone()),
                            box Expr::Number(index as u128),
                        ),
                    )],
                    true,
                );

                let push_string_call = Expr::Call(
                    push_string.clone(),
                    Vec::new(),
                    vec![index_to_string, output_var_ref.clone()],
                    true,
                );
                statements.push(push_string_call);
            }

            let push_closer_call = Expr::Call(
                push_string.clone(),
                Vec::new(),
                vec![Expr::Strin("]".into()), output_var_ref.clone()],
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
        name: VarName::from("to_string"),
        ret_type: TypeSpec::Named("String".into()).into(),
        args: vec![VarDecl {
            name: VarName::from("input"),
            type_spec: Some(typ.clone().into()),
        }],
        generic_count: 0,
        code: expr,
        relation: TypeSpecRelation::MethodOf(TypeSpec::Type(typ.into())),
    })
}

fn to_string<T: ToString + std::fmt::Debug>(val: &T) -> String { val.to_string() }
fn string_to_string(val: &String) -> String { format!(r#""{}""#, val) }
