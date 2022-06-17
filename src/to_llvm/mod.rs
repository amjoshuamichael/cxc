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
    for _e in &hlr.expressions {
        //println!("HLR: {_e:?}");
    }

    let mut ci = (Context::create(), ProgramInfo::default());
    let mut compiler = Compiler::from_context_and_info(&ci.0, &mut ci.1);

    let i32_t = compiler.context.i32_type();
    let fn_type = i32_t.fn_type(&[], false);
    let function = compiler.module.add_function("fn", fn_type, None);
    let basic_block = compiler.context.append_basic_block(function, "entry");

    compiler.current_function = Some(function);
    compiler.builder.position_at_end(basic_block);

    for expr_index in 0..(hlr.expressions.len() - 1) {
        compiler.compile_expr(&hlr.expressions[expr_index]);
    }

    let result = compiler
        .compile_expr(hlr.expressions.last().unwrap())
        .expect("no return")
        .into_int_value();
    compiler.builder.build_return(Some(&result.clone()));

    let result = unsafe { compiler.execution_engine.get_function("fn").ok() };
    let result: JitFunction<NumGeneratorFunc> = result.unwrap();

    println!("Running your function:");

    unsafe { result.call() }
}

pub struct Compiler<'ctx> {
    pub execution_engine: ExecutionEngine<'ctx>,
    pub program_info: RefCell<ProgramInfo<'ctx>>,
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub current_function: Option<FunctionValue<'ctx>>,
    scope: Vec<String>,
}

impl<'ctx> Compiler<'ctx> {
    pub fn from_context_and_info(
        context: &'ctx Context,
        program_info: &'ctx mut ProgramInfo<'ctx>,
    ) -> Self {
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
            scope: Vec::new(),
        }
    }

    fn full_scope_string(&self) -> String {
        let mut output = String::new();

        for s in &self.scope {
            output += &(s.clone() + "::");
        }

        output
    }

    pub fn compile_expr<'comp>(&'comp self, expr: &Expr) -> Option<AnyValueEnum<'comp>> {
        match &expr.kind {
            ExprKind::Literal(literal) => {
                //TODO: implement different int types
                match expr.gen_ret_type() {
                    PrimInt => Some(
                        self.context
                            .i32_type()
                            .const_int(literal.expect_num(), false)
                            .into(),
                    ),
                    _ => todo!(),
                }
            }
            ExprKind::Ident(name) => {
                let program_info = self.program_info.borrow();
                let val = program_info.variables.get(name).unwrap().clone();
                let loaded = self.builder.build_load(val, "load");

                match expr.gen_ret_type() {
                    PrimInt => Some(loaded.into_int_value().into()),
                    _ => todo!(),
                }
            }
            ExprKind::VarDecl(_) => unreachable!(), // Var declarations should be caught in the "Equals" branch of BinOp
            ExprKind::BinOp(lhs, op, rhs) => {
                use Opcode::*;

                if *op == Assignment {
                    let var_ptr = match &lhs.kind {
                        ExprKind::Ident(name) => self
                            .program_info
                            .borrow()
                            .variables
                            .get(name)
                            .unwrap()
                            .clone(),
                        ExprKind::VarDecl(name) => match expr.gen_ret_type() {
                            PrimInt => {
                                let var_ptr =
                                    self.builder.build_alloca(self.context.i32_type(), "alloca");
                                self.program_info
                                    .borrow_mut()
                                    .variables
                                    .insert(name.clone(), var_ptr);

                                var_ptr
                            }
                            _ => todo!(),
                        },
                        _ => panic!("Cannot set non-ident"),
                    };

                    let to_store = self.compile_expr(rhs)?;

                    match rhs.gen_ret_type() {
                        PrimInt => {
                            self.builder.build_store(var_ptr, to_store.into_int_value());
                        }
                        _ => todo!(),
                    }

                    return Some(to_store);
                }

                let lhs = self.compile_expr(lhs).unwrap();
                let rhs = self.compile_expr(rhs).unwrap();

                match expr.gen_ret_type() {
                    PrimInt => {
                        let lhs: inkwell::values::IntValue =
                            lhs.try_into().expect("incorrect type for expression");
                        let rhs = rhs.try_into().expect("incorrect type for expression");

                        let result = match op {
                            Plus => self.builder.build_int_add(lhs, rhs, "sum"),
                            Minus => self.builder.build_int_sub(lhs, rhs, "sub"),
                            Multiplier => self.builder.build_int_mul(lhs, rhs, "mul"),
                            Divider => self.builder.build_int_unsigned_div(lhs, rhs, "div"),
                            Equal => {
                                self.builder
                                    .build_int_compare(IntPredicate::EQ, lhs, rhs, "eq")
                            }
                            LessThan => {
                                self.builder
                                    .build_int_compare(IntPredicate::ULT, lhs, rhs, "lt")
                            }
                            GrtrThan => {
                                self.builder
                                    .build_int_compare(IntPredicate::UGT, lhs, rhs, "gt")
                            }
                            _ => todo!(),
                        };

                        Some(AnyValueEnum::IntValue(result))
                    }
                    _ => todo!(),
                }
            }
            ExprKind::IfThen(c, t) => {
                let current_func = self.current_function.unwrap();

                let cond = self.compile_expr(c).unwrap().into_int_value();

                let then_block = self.context.append_basic_block(current_func, "then");
                // Everything after the "then" statement. Is appended at the end.
                let after_block = self.context.append_basic_block(current_func, "after");

                self.builder
                    .build_conditional_branch(cond, then_block, after_block);

                self.builder.position_at_end(then_block);
                self.compile_expr(t);
                self.builder.build_unconditional_branch(after_block);

                self.builder.position_at_end(after_block);

                None
            }
            ExprKind::IfThenElse(c, t, e) => {
                let current_func = self.current_function.unwrap();

                let cond = self.compile_expr(c).unwrap().into_int_value();

                let then_block = self.context.append_basic_block(current_func, "then");
                let else_block = self.context.append_basic_block(current_func, "else");
                let after_block = self.context.append_basic_block(current_func, "after");

                self.builder
                    .build_conditional_branch(cond, then_block, else_block);

                self.builder.position_at_end(then_block);
                self.compile_expr(t);
                self.builder.build_unconditional_branch(after_block);

                self.builder.position_at_end(else_block);
                self.compile_expr(e);
                self.builder.build_unconditional_branch(after_block);

                self.builder.position_at_end(after_block);

                None
            }
            ExprKind::Block(exprs) => {
                for e in 0..(exprs.len() - 1) {
                    self.compile_expr(&exprs[e]);
                }

                self.compile_expr(&exprs.last().unwrap())
            }
            ExprKind::GotoMarker(name) => {
                let current_func = self.current_function.unwrap();

                let code_after = self.context.append_basic_block(current_func, "goto");

                self.program_info
                    .borrow_mut()
                    .gotos
                    .insert(name.clone(), code_after);

                self.builder.build_unconditional_branch(code_after);
                self.builder.position_at_end(code_after);

                None
            }
            ExprKind::Goto(name) => {
                let program_info = self.program_info.borrow();
                let block = program_info.force_grab_goto(name).clone();
                self.builder.build_unconditional_branch(block);

                None
            }
            ExprKind::While(w, d) => {
                let current_func = self.current_function.unwrap();

                let prewhile_block = self.context.append_basic_block(current_func, "prewhile");
                let whilecode_block = self.context.append_basic_block(current_func, "whilecode");
                let postwhile_block = self.context.append_basic_block(current_func, "postwhile");

                self.builder.build_unconditional_branch(prewhile_block);
                self.builder.position_at_end(prewhile_block);

                let cond = self.compile_expr(w).unwrap().into_int_value();

                self.builder
                    .build_conditional_branch(cond, whilecode_block, postwhile_block);

                self.builder.position_at_end(whilecode_block);
                self.compile_expr(d);
                self.builder.build_unconditional_branch(prewhile_block);

                self.builder.position_at_end(postwhile_block);

                None
            }
        }
    }
}
