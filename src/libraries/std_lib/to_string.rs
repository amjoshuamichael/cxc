use std::sync::Arc;

use crate::lex::VarName;
use crate::parse::{Expr, Opcode, TypeRelation, TypeSpec, TypeSpecRelation, VarDecl};
use crate::typ::{ArrayType, StructType};

use crate::{parse::FuncCode, unit::CompData, Type};
use crate::{ExternalFuncAdd, TypeEnum, Unit, XcReflect};

use crate::libraries::Library;

pub(super) struct ToStringLib;

impl Library for ToStringLib {
    fn add_to_unit(&self, unit: &mut Unit) {
        unit.add_method_deriver("to_string".into(), derive_to_string);

        let string_type = unit.comp_data.get_by_name(&"String".into()).unwrap();

        unit.add_external_to_string::<i8>();
        unit.add_external_to_string::<i16>();
        unit.add_external_to_string::<i32>();
        unit.add_external_to_string::<i64>();
        unit.add_external_to_string::<i128>();
        unit.add_external_to_string::<u8>();
        unit.add_external_to_string::<u16>();
        unit.add_external_to_string::<u32>();
        unit.add_external_to_string::<u64>();
        unit.add_external_to_string::<u128>();
        unit.add_external_to_string::<f32>();
        unit.add_external_to_string::<f64>();
        unit.add_external_to_string::<bool>();

        unit.add_rust_func_explicit(
            "to_string",
            //String::clone as *const usize,
            clone_string as *const usize,
            ExternalFuncAdd {
                ret_type: string_type.clone(),
                arg_types: vec![string_type.get_ref()],
                relation: TypeRelation::MethodOf(string_type.get_ref()),
                ..ExternalFuncAdd::empty()
            },
        );

        unit.push_script(include_str!("to_string.cxc")).unwrap();
    }
}

fn clone_string(s: &String) -> String {
    s.clone()
}

pub fn derive_to_string(comp_data: &CompData, typ: Type) -> Option<FuncCode> {
    if typ.is_unknown() {
        panic!();
    }

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
                .to_string_zero_if_anonymous()
                + " {";

            let output_var = VarName::from("output");
            let output_var_expr = Expr::Ident(output_var.clone());
            let output_var_ref = output_var_expr.get_ref();

            let mut statements = Vec::new();
            let make_var = Expr::SetVar(
                VarDecl {
                    name: output_var,
                    type_spec: string_type.into(),
                },
                box Expr::Strin(Arc::from(&*prefix)),
            );
            statements.push(make_var);

            for (field_index, (field_name, _)) in fields.iter().enumerate() {
                let field_prefix = if field_index > 0 {
                    String::from(", ") + &*field_name.to_string() + " = "
                } else {
                    field_name.to_string() + " = "
                };

                let field_prefix_expr = Expr::Strin(Arc::from(&*field_prefix));

                let push_prefix_call = Expr::Call {
                    func: push_string.clone(),
                    generics: Vec::new(),
                    args: vec![output_var_ref.clone(), field_prefix_expr.get_ref()],
                    is_method: true,
                };
                statements.push(push_prefix_call);

                let field = Expr::Member(input_var.clone(), field_name.clone());

                let field_to_string = Expr::Call {
                    func: to_string.clone(),
                    generics: Vec::new(),
                    args: vec![field.get_ref()],
                    is_method: true,
                };
                let push_string_call = Expr::Call {
                    func: push_string.clone(),
                    generics: Vec::new(),
                    args: vec![output_var_ref.clone(), field_to_string.get_ref()],
                    is_method: true,
                };
                statements.push(push_string_call);
            }

            let push_closer_call = Expr::Call {
                func: push_string,
                generics: Vec::new(),
                args: vec![output_var_ref, Expr::Strin("}".into()).get_ref()],
                is_method: true,
            };
            statements.push(push_closer_call);

            let ret = Expr::Return(box output_var_expr.clone());
            statements.push(ret);
            Expr::Block(statements)
        },
        TypeEnum::Array(ArrayType { count, .. }) => {
            let output_var = VarName::from("output");
            let output_var_expr = Expr::Ident(output_var.clone());
            let output_var_ref = Expr::UnarOp(Opcode::Ref, box output_var_expr.clone());

            let mut statements = Vec::new();
            let make_var = Expr::SetVar(
                VarDecl {
                    name: output_var,
                    type_spec: string_type.into(),
                },
                box Expr::Strin("[".into()),
            );
            statements.push(make_var);

            let push_string = box Expr::Ident(VarName::from("push_string"));

            for index in 0..*count {
                let comma = if index > 0 {
                    Arc::from(", ")
                } else {
                    Arc::from("")
                };

                let comma_expr = Expr::Strin(comma);

                let push_comma_call = Expr::Call {
                    func: push_string.clone(),
                    generics: Vec::new(),
                    args: vec![output_var_ref.clone(), comma_expr],
                    is_method: true,
                };
                statements.push(push_comma_call);

                let index_to_string = Expr::Call {
                    func: to_string.clone(),
                    generics: Vec::new(),
                    args: vec![Expr::UnarOp(
                        Opcode::Ref,
                        box Expr::Index(
                            box Expr::UnarOp(Opcode::Deref, input_var.clone()),
                            box Expr::Number(index as u64),
                        ),
                    )],
                    is_method: true,
                };

                let push_string_call = Expr::Call {
                    func: push_string.clone(),
                    generics: Vec::new(),
                    args: vec![output_var_ref.clone(), index_to_string],
                    is_method: true,
                };
                statements.push(push_string_call);
            }

            let push_closer_call = Expr::Call {
                func: push_string,
                generics: Vec::new(),
                args: vec![output_var_ref, Expr::Strin("]".into())],
                is_method: true,
            };
            statements.push(push_closer_call);

            let ret = Expr::Return(box output_var_expr.clone());
            statements.push(ret);
            Expr::Block(statements)
        },
        _ => return None,
    };

    Some(FuncCode {
        name: VarName::from("to_string"),
        ret_type: TypeSpec::Named("String".into()),
        args: vec![VarDecl {
            name: VarName::from("input"),
            type_spec: typ.clone().into(),
        }],
        generic_count: 0,
        code: expr,
        relation: TypeSpecRelation::MethodOf(TypeSpec::Type(typ)),
    })
}

fn to_string<T: ToString>(val: &T) -> String { val.to_string() }
