use super::*;
use crate::hlr::expr_tree::{ExprID, NodeData::*};
use crate::hlr::prelude::*;
use crate::parse::Opcode::*;
use crate::unit::*;
use core::cell::RefCell;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::types::*;
use inkwell::values::*;
use inkwell::AddressSpace;
use inkwell::FloatPredicate;
use inkwell::IntPredicate;
use std::collections::HashMap;
use std::sync::Arc;

pub struct FunctionCompilationState<'f> {
    pub tree: ExprTree,
    pub variables: HashMap<Arc<str>, PointerValue<'f>>,
    pub function: FunctionValue<'f>,
    pub builder: Builder<'f>,
    pub context: &'f Context,
    pub globals: &'f Globals<'f>,
    pub arg_names: Vec<Arc<str>>,
    pub llvm_ir_uuid: RefCell<u32>,
}

impl<'f> FunctionCompilationState<'f> {
    fn new_uuid<'a>(&self) -> String {
        let current_uuid = *self.llvm_ir_uuid.borrow();
        let output = current_uuid.to_string();
        self.llvm_ir_uuid.replace(current_uuid + 1);
        String::from("t") + &*output
    }

    pub fn delete(self) {}
}

pub fn compile<'comp>(
    fcs: &mut FunctionCompilationState<'comp>,
    expr_id: ExprID,
) -> Option<AnyValueEnum<'comp>> {
    let expr = fcs.tree.get(expr_id);

    if crate::DEBUG {
        println!("compiling: {expr_id:?}, {expr:?}");
    }

    dbg!(&expr);

    let output: Option<AnyValueEnum> = match expr {
        Number { ref value, .. } => {
            // TODO: implement different int types
            Some(
                fcs.context
                    .i32_type()
                    .const_int(value.try_into().unwrap(), false)
                    .into(),
            )
        },
        Float { ref value, .. } => {
            // TODO: implement different int types
            Some(fcs.context.f32_type().const_float(*value).into())
        },
        Ident { ref name, .. } => {
            if fcs.arg_names.contains(name) {
                let param_index = fcs
                    .arg_names
                    .iter()
                    .position(|arg_name| arg_name == name)
                    .unwrap();
                let param = fcs
                    .function
                    .get_nth_param(param_index.try_into().unwrap())
                    .unwrap();
                return Some(param.into());
            }

            let val = fcs.variables.get(&**name).unwrap().clone();
            let loaded = fcs.builder.build_load(val, &*fcs.new_uuid());

            Some(loaded.into())
        },
        MakeVar {
            ref var_type,
            ref name,
            ref rhs,
            ..
        } => {
            let to_store = compile(fcs, *rhs).unwrap();
            let to_store_basic: BasicValueEnum = to_store.try_into().unwrap();

            if fcs.arg_names.contains(name) {
                let param = fcs.function.get_nth_param(0).unwrap();
                fcs.builder
                    .build_store(param.try_into().unwrap(), to_store_basic);
                return Some(to_store);
            }

            if let Some(val) = fcs.variables.get(&**name) {
                if matches!(var_type.clone().as_type_enum(), TypeEnum::Ref(_)) {
                    let tmp = fcs.builder.build_load(*val, &*fcs.new_uuid());
                    fcs.builder
                        .build_store(tmp.into_pointer_value(), to_store_basic);
                    return Some(to_store);
                }
                fcs.builder.build_store(*val, to_store_basic);

                return Some(to_store);
            }

            let var_ptr = {
                let var_ptr = {
                    fcs.builder.build_alloca(
                        expr.ret_type().unwrap().to_basic_type(fcs.context),
                        name,
                    )
                };

                fcs.variables.insert(name.clone(), var_ptr);

                var_ptr
            };

            fcs.builder.build_store(var_ptr, to_store_basic);

            return Some(to_store);
        },
        SetVar {
            ref lhs, ref rhs, ..
        } => {
            let var_ptr = compile_as_ptr(fcs, *lhs);

            let rhs = compile(fcs, *rhs).unwrap();
            let rhs_basic: BasicValueEnum = rhs.try_into().unwrap();
            fcs.builder.build_store(var_ptr, rhs_basic);

            Some(rhs)
        },
        BinOp {
            ref lhs,
            ref op,
            ref rhs,
            ref ret_type,
        } => {
            let mut lhs = compile(fcs, *lhs).unwrap();
            let mut rhs = compile(fcs, *rhs).unwrap();

            match ret_type.as_type_enum() {
                TypeEnum::Int(_) => {
                    let lhs = lhs.try_into().expect("incorrect type for expression");
                    let rhs = rhs.try_into().expect("incorrect type for expression");

                    let result: IntValue = match op {
                        Plus => fcs.builder.build_int_add(lhs, rhs, "sum"),
                        Minus => fcs.builder.build_int_sub(lhs, rhs, "sub"),
                        Multiplier => fcs.builder.build_int_mul(lhs, rhs, "mul"),
                        Divider => {
                            fcs.builder.build_int_unsigned_div(lhs, rhs, "div")
                        },
                        Modulus => {
                            fcs.builder.build_int_unsigned_rem(lhs, rhs, "mod")
                        },
                        Inequal => fcs.builder.build_int_compare(
                            IntPredicate::NE,
                            lhs,
                            rhs,
                            "eq",
                        ),
                        Equal => fcs.builder.build_int_compare(
                            IntPredicate::EQ,
                            lhs,
                            rhs,
                            "eq",
                        ),
                        LessThan => fcs.builder.build_int_compare(
                            IntPredicate::ULT,
                            lhs,
                            rhs,
                            "lt",
                        ),
                        GrtrThan => fcs.builder.build_int_compare(
                            IntPredicate::UGT,
                            lhs,
                            rhs,
                            "gt",
                        ),
                        _ => todo!(),
                    };

                    Some(result.into())
                },
                TypeEnum::Float(_) => {
                    let lhs = lhs.try_into().expect("incorrect type for expression");
                    let rhs = rhs.try_into().expect("incorrect type for expression");

                    if matches!(op, Plus | Minus | Multiplier | Divider | Modulus) {
                        let result: FloatValue = match op {
                            Plus => fcs.builder.build_float_add(lhs, rhs, "sum"),
                            Minus => fcs.builder.build_float_sub(lhs, rhs, "sub"),
                            Multiplier => {
                                fcs.builder.build_float_mul(lhs, rhs, "mul")
                            },
                            Divider => fcs.builder.build_float_div(lhs, rhs, "div"),
                            Modulus => fcs.builder.build_float_rem(lhs, rhs, "mod"),
                            _ => todo!(),
                        };

                        return Some(result.into());
                    }

                    let result: IntValue = match op {
                        Inequal => fcs.builder.build_float_compare(
                            FloatPredicate::ONE,
                            lhs,
                            rhs,
                            "eq",
                        ),
                        Equal => fcs.builder.build_float_compare(
                            FloatPredicate::OEQ,
                            lhs,
                            rhs,
                            "eq",
                        ),
                        LessThan => fcs.builder.build_float_compare(
                            FloatPredicate::ULT,
                            lhs,
                            rhs,
                            "lt",
                        ),
                        GrtrThan => fcs.builder.build_float_compare(
                            FloatPredicate::UGT,
                            lhs,
                            rhs,
                            "gt",
                        ),
                        _ => todo!(),
                    };

                    Some(result.into())
                },
                _ => {
                    dbg!(&expr);
                    unimplemented!();
                },
            }
        },
        IfThen { i, t, .. } => {
            let cond = compile(fcs, i).unwrap().into_int_value();

            let then_block = fcs.context.append_basic_block(fcs.function, "then");
            // Everything after the "then" statement. Is appended at the end.
            let after_block = fcs.context.append_basic_block(fcs.function, "after");

            fcs.builder
                .build_conditional_branch(cond, then_block, after_block);

            fcs.builder.position_at_end(then_block);
            compile(fcs, t);
            fcs.builder.build_unconditional_branch(after_block);

            fcs.builder.position_at_end(after_block);

            None
        },
        IfThenElse { i, t, e, .. } => {
            let cond = compile(fcs, i).unwrap().into_int_value();

            let then_block = fcs.context.append_basic_block(fcs.function, "then");
            let else_block = fcs.context.append_basic_block(fcs.function, "else");
            let after_block = fcs.context.append_basic_block(fcs.function, "after");

            fcs.builder
                .build_conditional_branch(cond, then_block, else_block);

            fcs.builder.position_at_end(then_block);
            compile(fcs, t);
            fcs.builder.build_unconditional_branch(after_block);

            fcs.builder.position_at_end(else_block);
            compile(fcs, e);
            fcs.builder.build_unconditional_branch(after_block);

            fcs.builder.position_at_end(after_block);

            None
        },
        Block { stmts, .. } => {
            let mut stmts = stmts.iter().peekable();

            while let Some(e) = stmts.next() {
                if stmts.peek().is_some() {
                    compile(fcs, *e);
                } else {
                    return compile(fcs, *e);
                }
            }

            return None;
        },
        While { w, d } => {
            let prewhile_block =
                fcs.context.append_basic_block(fcs.function, "prewhile");
            let whilecode_block =
                fcs.context.append_basic_block(fcs.function, "whilecode");
            let postwhile_block =
                fcs.context.append_basic_block(fcs.function, "postwhile");

            fcs.builder.build_unconditional_branch(prewhile_block);
            fcs.builder.position_at_end(prewhile_block);

            let cond = compile(fcs, w).unwrap().into_int_value();

            fcs.builder.build_conditional_branch(
                cond,
                whilecode_block,
                postwhile_block,
            );

            fcs.builder.position_at_end(whilecode_block);
            compile(fcs, d);
            fcs.builder.build_unconditional_branch(prewhile_block);

            fcs.builder.position_at_end(postwhile_block);

            None
        },
        UnarOp { op, hs, .. } => match op {
            crate::parse::Opcode::Ref(_) => {
                Some(compile_as_ptr(fcs, hs).as_any_value_enum())
            },
            crate::parse::Opcode::Deref(_) => {
                let var_ptr = compile(fcs, hs).unwrap();
                let val = fcs
                    .builder
                    .build_load(var_ptr.into_pointer_value(), &*fcs.new_uuid());
                Some(val.into())
            },
            _ => todo!(),
        },
        Call { f, a, .. } => {
            let function = compile(fcs, f).unwrap();
            let function: FunctionValue = function.try_into().unwrap();

            let mut arg_vals = Vec::new();

            for arg in a {
                let basic_arg: BasicValueEnum =
                    compile(fcs, arg).unwrap().try_into().unwrap();
                let basic_meta_arg: BasicMetadataValueEnum =
                    basic_arg.try_into().unwrap();
                arg_vals.push(basic_meta_arg);
            }

            Some(
                fcs.builder
                    .build_call(function, &arg_vals[..], "call")
                    .as_any_value_enum(),
            )
        },
        Global { name, .. } => fcs.globals.get_value(name),
        Member {
            ret_type,
            object,
            field,
        } => {
            let ptr = compile_as_ptr(fcs, expr_id);
            let val = fcs.builder.build_load(ptr, "load");

            Some(val.as_any_value_enum())
        },
        StructLit {
            struct_type,
            fields,
            ..
        } => {
            let mut compiled_fields = Vec::new();

            let TypeEnum::Struct(struct_type) = struct_type.as_type_enum() else { panic!() };

            // TODO: sort during funcrep
            for field_index in 0..struct_type.field_count() {
                let field = fields
                    .iter()
                    .find(|(f, _)| struct_type.get_field_index(f) == field_index)
                    .unwrap();

                let compiled_field: BasicValueEnum =
                    compile(fcs, field.1).unwrap().try_into().unwrap();
                compiled_fields.push(compiled_field);
            }

            Some(
                fcs.context
                    .const_struct(&compiled_fields[..], true)
                    .as_any_value_enum(),
            )
        },
        _ => todo!(),
    };

    println!("done compiling: {expr_id:?}");
    if crate::DEBUG {
        println!("done compiling: {expr_id:?}");
    }

    output
}

fn compile_as_ptr<'comp>(
    fcs: &mut FunctionCompilationState<'comp>,
    expr_id: ExprID,
) -> PointerValue<'comp> {
    println!("doing a round of compilation as pointer...");
    match fcs.tree.get(expr_id) {
        Ident { name, .. } => fcs.variables.get(&*name).unwrap().clone(),
        Member {
            ret_type,
            object,
            field,
        } => {
            let typ = fcs.tree.get(object).ret_type().unwrap();
            let object_type = typ.as_type_enum();

            let TypeEnum::Struct(struct_type) = object_type else { panic!() };

            let field_index = struct_type.get_field_index(&field);

            let object = compile_as_ptr(fcs, object);

            fcs.builder
                .build_struct_gep(object, field_index as u32, "access")
                .unwrap()
        },
        _ => todo!(),
    }
}
