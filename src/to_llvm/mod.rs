use super::*;
use crate::hlr::expr_tree::{ExprID, NodeData::*};
use crate::hlr::prelude::*;
use crate::parse::Opcode::*;
use crate::unit::to_basic_type;
use core::cell::RefCell;
use inkwell::builder::Builder;
use inkwell::context::Context;
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
}

pub fn compile<'comp>(
    fcs: &mut FunctionCompilationState<'comp>,
    expr_id: ExprID,
) -> Option<BasicValueEnum<'comp>> {
    let expr = fcs.tree.get(expr_id);

    if crate::DEBUG {
        println!("compiling: {expr_id:?}, {expr:?}");
    }

    let output = match expr {
        Number(ref literal) => {
            // TODO: implement different int types
            Some(
                fcs.context
                    .i32_type()
                    .const_int(literal.try_into().unwrap(), false)
                    .into(),
            )
        },
        Float(ref literal) => {
            // TODO: implement different int types
            Some(fcs.context.f32_type().const_float(*literal).into())
        },
        Ident { ref name, .. } => {
            if fcs.arg_names.contains(name) {
                let param_index = fcs
                    .arg_names
                    .iter()
                    .position(|arg_name| arg_name == name)
                    .unwrap();
                println!("NAME {name}, INDEX {param_index}");
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
        SetVar {
            ref var_type,
            ref name,
            ref rhs,
            ..
        } => {
            let to_store = compile(fcs, *rhs).unwrap();

            if fcs.arg_names.contains(name) {
                let param = fcs.function.get_nth_param(0).unwrap();
                fcs.builder.build_store(param.try_into().unwrap(), to_store);
                return Some(to_store);
            }

            if let Some(val) = fcs.variables.get(&**name) {
                if var_type.ref_count > 0 {
                    let tmp = fcs.builder.build_load(*val, &*fcs.new_uuid());
                    fcs.builder.build_store(tmp.into_pointer_value(), to_store);
                    return Some(to_store);
                }
                fcs.builder.build_store(*val, to_store);

                return Some(to_store);
            }

            let var_ptr = {
                let var_ptr = match expr.gen_ret_type() {
                    PrimInt => {
                        let x =
                            fcs.builder.build_alloca(fcs.context.i32_type(), name);
                        x
                    },
                    PrimFloat => {
                        fcs.builder.build_alloca(fcs.context.f32_type(), name)
                    },
                    PrimRef => fcs.builder.build_alloca(
                        to_basic_type(fcs.context, &var_type.clone()),
                        name,
                    ),
                    _ => todo!(),
                };

                fcs.variables.insert(name.clone(), var_ptr);

                var_ptr
            };

            fcs.builder.build_store(var_ptr, to_store);

            return Some(to_store);
        },
        BinOp {
            ref lhs,
            ref op,
            ref rhs,
            ..
        } => {
            let lhs = compile(fcs, *lhs).unwrap();
            let rhs = compile(fcs, *rhs).unwrap();

            match expr.gen_ret_type() {
                PrimInt => {
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
                PrimFloat => {
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
                _ => todo!(),
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
            crate::parse::Opcode::Ref(_) => match fcs.tree.get(hs) {
                Ident { name, .. } => {
                    println!(
                        "TYPE OF {name}: {:?}",
                        fcs.variables.get(&*name).unwrap()
                    );
                    Some(fcs.variables.get(&*name).unwrap().clone().into())
                },
                _ => todo!(),
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
        _ => todo!(),
    };

    if crate::DEBUG {
        println!("done compiling: {expr_id:?}");
    }

    output
}
