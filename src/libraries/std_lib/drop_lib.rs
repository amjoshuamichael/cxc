use crate::{library::Library, CompData, Type, parse::{FuncCode, Expr, Opcode, TypeSpecRelation, TypeSpec, VarDecl}, TypeEnum, VarName, typ::{Field, spec_from_type::type_to_type_spec, ABI}};

pub struct DropLib;

impl Library for DropLib {
    fn add_to_unit(&self, unit: &mut crate::Unit) {
        unit.add_method_deriver("drop".into(), derive_drop);
    }
}

pub fn derive_drop(_: &CompData, typ: Type) -> Option<FuncCode> {
    let drop = Expr::ident("drop");
    let me = Expr::ident("self");
    let typ = typ.complete_deref(); 

    let expr = match typ.clone().as_type_enum() {
        _ if typ.is_shallow() => Expr::Block(Vec::new()),
        TypeEnum::Struct(struct_type) => {
            let mut statements = Vec::new();

            for Field { name, typ, .. } in struct_type.fields.iter() {
                let generics = 
                    typ.generics().clone().into_iter().map(|x| type_to_type_spec(x)).collect();
                let args = vec![
                    Expr::UnarOp(
                        Opcode::Ref, 
                        Box::new(Expr::Member(me.clone(), name.clone()))
                    )
                ];

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

    let type_spec = type_to_type_spec(typ.clone()).get_ref();
    Some(FuncCode {
        name: VarName::from("drop"),
        ret_type: TypeSpec::Void,
        relation: TypeSpecRelation::MethodOf(type_spec.clone()),
        args: vec![VarDecl {
            name: VarName::from("self"),
            type_spec,
        }],
        generic_count: 0,
        code: expr,
        is_external: false,
        abi: ABI::C,
    })
}
