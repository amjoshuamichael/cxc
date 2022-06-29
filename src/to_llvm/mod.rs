use crate::hlr::expr_tree::{ExprID, ExprTree, NodeData::*};
use crate::hlr::prelude::*;
use crate::parse::prelude::Opcode;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::execution_engine::JitFunction;
use inkwell::module::Module;
use inkwell::values::{AnyValueEnum, FunctionValue};
use inkwell::IntPredicate;
use inkwell::OptimizationLevel;
use std::cell::RefCell;

pub mod prelude {
    pub use super::Compiler;
}

#[cfg(test)]
mod tests;

type NumGeneratorFunc = unsafe extern "C" fn() -> i32;
pub fn to_llvm<'a>(hlr: HLR) -> i32 {
    let mut ci = (Context::create(), ProgramInfo::default());
    let mut compiler = Compiler::from_context_and_info(&ci.0, &mut ci.1);

    let i32_t = compiler.context.i32_type();
    let fn_type = i32_t.fn_type(&[], false);
    let function = compiler.module.add_function("fn", fn_type, None);
    let basic_block = compiler.context.append_basic_block(function, "entry");

    compiler.current_function = Some(function);
    compiler.builder.position_at_end(basic_block);

    let result = compiler.compile_expr(&hlr.tree, ExprID::ROOT).unwrap().into_int_value();
    compiler.builder.build_return(Some(&result.clone()));

    let result = unsafe { compiler.execution_engine.get_function("fn").ok() };
    let result: JitFunction<NumGeneratorFunc> = result.unwrap();

    unsafe { result.call() }
}

pub struct Compiler<'ctx> {
    pub execution_engine: ExecutionEngine<'ctx>,
    pub program_info: RefCell<ProgramInfo<'ctx>>,
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub current_function: Option<FunctionValue<'ctx>>,
}

impl<'ctx> Compiler<'ctx> {
    pub fn from_context_and_info(context: &'ctx Context, program_info: &'ctx mut ProgramInfo<'ctx>) -> Self {
        let module = context.create_module("sum");
        let execution_engine = module
            .create_jit_execution_engine(OptimizationLevel::Aggressive)
            .unwrap();

        Self {
            context: &context,
            program_info: RefCell::new(program_info.clone()),
            module,
            builder: context.create_builder(),
            execution_engine,
            current_function: None,
        }
    }

    pub fn compile_expr<'comp>(&'comp self, tree: &ExprTree, expr: ExprID) -> Option<AnyValueEnum<'comp>> {
        let expr = tree.get(expr);

        match expr {
            Number(ref literal) => {
                //TODO: implement different int types
                Some(
                    self.context
                        .i32_type()
                        .const_int(literal.try_into().unwrap(), false)
                        .into(),
                )
            }
            Ident { ref name, .. } => {
                let program_info = self.program_info.borrow();
                let val = program_info.variables.get(&**name).unwrap().clone();
                let loaded = self.builder.build_load(val, "load");

                match expr.gen_ret_type() {
                    PrimInt => Some(loaded.into_int_value().into()),
                    PrimFloat => Some(loaded.into_float_value().into()),
                    _ => todo!(),
                }
            }
            // Var declarations should be caught in the "Equals" branch of BinOp
            VarDecl { .. } => unreachable!(),
            BinOp {
                ref lhs,
                ref op,
                ref rhs,
                ..
            } => {
                use Opcode::*;

                if *op == Assignment {
                    let var_ptr = match tree.get(*lhs) {
                        Ident { name, .. } => self.program_info.borrow().variables.get(&*name).unwrap().clone(),
                        VarDecl { name, .. } => {
                            let var_ptr = match expr.gen_ret_type() {
                                PrimInt => self.builder.build_alloca(self.context.i32_type(), "alloc"),
                                PrimFloat => self.builder.build_alloca(self.context.f32_type(), "alloc"),
                                _ => todo!(),
                            };

                            self.program_info.borrow_mut().variables.insert(name, var_ptr);

                            var_ptr
                        }
                        _ => panic!("Cannot set non-ident"),
                    };

                    let to_store = self.compile_expr(tree, *rhs)?;

                    match tree.get(*rhs).gen_ret_type() {
                        PrimInt => self.builder.build_store(var_ptr, to_store.into_int_value()),
                        PrimFloat => self.builder.build_store(var_ptr, to_store.into_float_value()),
                        _ => todo!(),
                    };

                    return Some(to_store);
                }

                let lhs = self.compile_expr(tree, *lhs).unwrap();
                let rhs = self.compile_expr(tree, *rhs).unwrap();

                match expr.gen_ret_type() {
                    PrimInt => {
                        let lhs: inkwell::values::IntValue = lhs.try_into().expect("incorrect type for expression");
                        let rhs = rhs.try_into().expect("incorrect type for expression");

                        let result = match op {
                            Plus => self.builder.build_int_add(lhs, rhs, "sum"),
                            Minus => self.builder.build_int_sub(lhs, rhs, "sub"),
                            Multiplier => self.builder.build_int_mul(lhs, rhs, "mul"),
                            Divider => self.builder.build_int_unsigned_div(lhs, rhs, "div"),
                            Modulus => self.builder.build_int_unsigned_rem(lhs, rhs, "mod"),
                            Inequal => self.builder.build_int_compare(IntPredicate::NE, lhs, rhs, "eq"),
                            Equal => self.builder.build_int_compare(IntPredicate::EQ, lhs, rhs, "eq"),
                            LessThan => self.builder.build_int_compare(IntPredicate::ULT, lhs, rhs, "lt"),
                            GrtrThan => self.builder.build_int_compare(IntPredicate::UGT, lhs, rhs, "gt"),
                            _ => todo!(),
                        };

                        Some(AnyValueEnum::IntValue(result))
                    }
                    PrimInt => {
                        let lhs = lhs.into_float_value();
                        let rhs = rhs.try_into().expect("incorrect type for expression");

                        let result = match op {
                            Plus => self.builder.build_float_add(lhs, rhs, "sum"),
                            Minus => self.builder.build_float_sub(lhs, rhs, "sub"),
                            Multiplier => self.builder.build_float_mul(lhs, rhs, "mul"),
                            Divider => self.builder.build_float_unsigned_div(lhs, rhs, "div"),
                            Modulus => self.builder.build_float_unsigned_rem(lhs, rhs, "mod"),
                            Inequal => self.builder.build_float_compare(IntPredicate::NE, lhs, rhs, "eq"),
                            Equal => self.builder.build_float_compare(IntPredicate::EQ, lhs, rhs, "eq"),
                            LessThan => self.builder.build_float_compare(IntPredicate::ULT, lhs, rhs, "lt"),
                            GrtrThan => self.builder.build_float_compare(IntPredicate::UGT, lhs, rhs, "gt"),
                            _ => todo!(),
                        };

                        Some(AnyValueEnum::IntValue(result))
                    }
                    _ => todo!(),
                }
            }
            IfThen { i, t, .. } => {
                let current_func = self.current_function.unwrap();

                let cond = self.compile_expr(tree, i).unwrap().into_int_value();

                let then_block = self.context.append_basic_block(current_func, "then");
                // Everything after the "then" statement. Is appended at the end.
                let after_block = self.context.append_basic_block(current_func, "after");

                self.builder.build_conditional_branch(cond, then_block, after_block);

                self.builder.position_at_end(then_block);
                self.compile_expr(tree, t);
                self.builder.build_unconditional_branch(after_block);

                self.builder.position_at_end(after_block);

                None
            }
            IfThenElse { i, t, e, .. } => {
                let current_func = self.current_function.unwrap();

                let cond = self.compile_expr(tree, i).unwrap().into_int_value();

                let then_block = self.context.append_basic_block(current_func, "then");
                let else_block = self.context.append_basic_block(current_func, "else");
                let after_block = self.context.append_basic_block(current_func, "after");

                self.builder.build_conditional_branch(cond, then_block, else_block);

                self.builder.position_at_end(then_block);
                self.compile_expr(tree, t);
                self.builder.build_unconditional_branch(after_block);

                self.builder.position_at_end(else_block);
                self.compile_expr(tree, e);
                self.builder.build_unconditional_branch(after_block);

                self.builder.position_at_end(after_block);

                None
            }
            Block { stmts, .. } => {
                for e in 0..(stmts.len() - 1) {
                    self.compile_expr(tree, stmts[e]);
                }

                self.compile_expr(tree, *stmts.last().unwrap())
            }
            While { w, d } => {
                let current_func = self.current_function.unwrap();

                let prewhile_block = self.context.append_basic_block(current_func, "prewhile");
                let whilecode_block = self.context.append_basic_block(current_func, "whilecode");
                let postwhile_block = self.context.append_basic_block(current_func, "postwhile");

                self.builder.build_unconditional_branch(prewhile_block);
                self.builder.position_at_end(prewhile_block);

                let cond = self.compile_expr(tree, w).unwrap().into_int_value();

                self.builder
                    .build_conditional_branch(cond, whilecode_block, postwhile_block);

                self.builder.position_at_end(whilecode_block);
                self.compile_expr(tree, d);
                self.builder.build_unconditional_branch(prewhile_block);

                self.builder.position_at_end(postwhile_block);

                None
            }
            _ => todo!(),
        }
    }
}
