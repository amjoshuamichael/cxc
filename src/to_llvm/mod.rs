use crate::hlr::expr_tree::{ExprID, ExprTree, NodeData::*};
use crate::hlr::prelude::*;
use crate::parse::prelude::Opcode;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::execution_engine::JitFunction;
use inkwell::module::Module;
use inkwell::values::*;
use inkwell::{AddressSpace, OptimizationLevel};
use inkwell::{FloatPredicate, IntPredicate};
use std::cell::RefCell;

pub mod prelude {
    pub use super::Compiler;
}

#[cfg(test)]
mod tests;

type NumGeneratorFunc = unsafe extern "C" fn() -> f32;
pub fn to_llvm<'a>(hlr: HLR) -> f32 {
    if crate::DEBUG {
        println!("--------COMPILATION DATA--------");
    }

    let mut ci = (Context::create(), ProgramInfo::default());
    let mut compiler = Compiler::from_context_and_info(&ci.0, &mut ci.1);

    let i32_t = compiler.context.f32_type();
    let fn_type = i32_t.fn_type(&[], false);
    let function = compiler.module.add_function("fn", fn_type, None);
    let basic_block = compiler.context.append_basic_block(function, "entry");

    compiler.current_function = Some(function);
    compiler.builder.position_at_end(basic_block);

    let result = compiler
        .compile_expr(&hlr.tree, ExprID::ROOT)
        .unwrap()
        .into_float_value();
    compiler.builder.build_return(Some(&result.clone()));

    let result = unsafe { compiler.execution_engine.get_function("fn").ok() };
    let result: JitFunction<NumGeneratorFunc> = result.unwrap();

    if crate::DEBUG {
        println!();
        println!("--------OUTPUT LLVM--------");
        compiler.module.print_to_stderr();

        println!();
        println!("--------RESULT--------");
        println!();
    }

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

    pub fn compile_expr<'comp>(&'comp self, tree: &ExprTree, expr: ExprID) -> Option<BasicValueEnum<'comp>> {
        if crate::DEBUG {
            println!("compiling: {expr:?}");
        }

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
            Float(ref literal) => {
                //TODO: implement different int types
                Some(self.context.f32_type().const_float(*literal).into())
            }
            Ident { ref name, .. } => {
                let program_info = self.program_info.borrow();
                let val = program_info.variables.get(&**name).unwrap().clone();
                let loaded = self.builder.build_load(val, "load");

                Some(loaded.into())
            }
            VarDecl { ref name, ref rhs, .. } => {
                let to_store = self.compile_expr(tree, *rhs)?;

                if let Some(val) = self.program_info.borrow().variables.get(&**name) {
                    self.builder.build_store(*val, to_store);

                    return Some(to_store);
                }

                let var_ptr = {
                    let var_ptr = match expr.gen_ret_type() {
                        PrimInt => self.builder.build_alloca(self.context.i32_type(), "alloc"),
                        PrimFloat => self.builder.build_alloca(self.context.f32_type(), "alloc"),
                        PrimRef(_) => self
                            .builder
                            .build_alloca(self.context.f32_type().ptr_type(AddressSpace::Global), "alloc"),
                        _ => todo!(),
                    };

                    self.program_info.borrow_mut().variables.insert(name.clone(), var_ptr);

                    var_ptr
                };

                self.builder.build_store(var_ptr, to_store);

                return Some(to_store);
            }
            BinOp {
                ref lhs,
                ref op,
                ref rhs,
                ..
            } => {
                use Opcode::*;

                let lhs = self.compile_expr(tree, *lhs).unwrap();
                let rhs = self.compile_expr(tree, *rhs).unwrap();

                match expr.gen_ret_type() {
                    PrimInt => {
                        let lhs = lhs.try_into().expect("incorrect type for expression");
                        let rhs = rhs.try_into().expect("incorrect type for expression");

                        let result: IntValue = match op {
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

                        Some(result.into())
                    }
                    PrimFloat => {
                        let lhs = lhs.try_into().expect("incorrect type for expression");
                        let rhs = rhs.try_into().expect("incorrect type for expression");

                        if matches!(op, Plus | Minus | Multiplier | Divider | Modulus) {
                            let result: FloatValue = match op {
                                Plus => self.builder.build_float_add(lhs, rhs, "sum"),
                                Minus => self.builder.build_float_sub(lhs, rhs, "sub"),
                                Multiplier => self.builder.build_float_mul(lhs, rhs, "mul"),
                                Divider => self.builder.build_float_div(lhs, rhs, "div"),
                                Modulus => self.builder.build_float_rem(lhs, rhs, "mod"),
                                _ => todo!(),
                            };

                            return Some(result.into());
                        }

                        let result: IntValue = match op {
                            Inequal => self.builder.build_float_compare(FloatPredicate::ONE, lhs, rhs, "eq"),
                            Equal => self.builder.build_float_compare(FloatPredicate::OEQ, lhs, rhs, "eq"),
                            LessThan => self.builder.build_float_compare(FloatPredicate::ULT, lhs, rhs, "lt"),
                            GrtrThan => self.builder.build_float_compare(FloatPredicate::UGT, lhs, rhs, "gt"),
                            _ => todo!(),
                        };

                        Some(result.into())
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
                let mut stmts = stmts.iter().peekable();

                while let Some(e) = stmts.next() {
                    if stmts.peek().is_some() {
                        self.compile_expr(tree, *e);
                    } else {
                        return self.compile_expr(tree, *e);
                    }
                }

                return None;
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
            UnarOp { op, hs, .. } => {
                let program_info = self.program_info.borrow();

                match op {
                    crate::parse::Opcode::Ref => match tree.get(hs) {
                        Ident { name, .. } => Some(program_info.variables.get(&*name).unwrap().clone().into()),
                        _ => todo!(),
                    },
                    crate::parse::Opcode::Deref => {
                        let var_ptr = self.compile_expr(tree, hs).unwrap();
                        let val = self.builder.build_load(var_ptr.into_pointer_value(), "load");
                        Some(val.into())
                    }
                    _ => todo!(),
                }
            }
            _ => todo!(),
        }
    }
}
