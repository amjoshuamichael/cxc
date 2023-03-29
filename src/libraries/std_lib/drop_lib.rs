use crate::{library::Library, CompData, Type, parse::{FuncCode, Expr, Opcode, TypeSpecRelation, TypeSpec, VarDecl}, TypeEnum, VarName};

pub struct DropLib;

impl Library for DropLib {
    fn add_to_unit(&self, unit: &mut crate::Unit) {
        unit.add_method_deriver("drop".into(), derive_drop);
    }
}

pub fn derive_drop(_: &CompData, typ: Type) -> Option<FuncCode> {
    let drop = box Expr::Ident(VarName::from("drop"));
    let me = box Expr::Ident(VarName::from("self"));

    let expr = match typ.clone().get_deref()?.as_type_enum() {
        _ if typ.is_shallow() => Expr::Block(Vec::new()),
        TypeEnum::Struct(struct_type) => {
            let mut statements = Vec::new();

            for (field_name, field_type) in struct_type.fields.iter() {
                let generics = 
                    field_type.generics().clone().into_iter().map(|x| x.into()).collect();
                let args =
                    vec![Expr::UnarOp(Opcode::Ref, box Expr::Member(me.clone(), field_name.clone()))];

                statements.push(Expr::Call {
                    func: drop.clone(),
                    generics, 
                    args, 
                    is_method: true,
                })
            }

            Expr::Block(statements)
        }
        _ => return None,
    };

    Some(FuncCode {
        name: VarName::from("drop"),
        ret_type: TypeSpec::Void,
        args: vec![VarDecl {
            name: VarName::from("self"),
            type_spec: typ.clone().into(),
        }],
        generic_count: typ.generics().len() as u32,
        code: expr,
        relation: TypeSpecRelation::MethodOf(TypeSpec::Type(typ)),
    })
}
