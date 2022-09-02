use crate::lex::VarName;
use crate::hlr::expr_tree::{ExprID, NodeData::*};
use crate::hlr::prelude::*;
use crate::parse::Opcode::*;
use crate::unit::*;
use core::cell::RefCell;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::types::*;
use inkwell::values::*;
use inkwell::FloatPredicate;
use inkwell::IntPredicate;
use std::collections::HashMap;

pub struct FunctionCompilationState<'f> {
    pub tree: ExprTree,
    pub variables: HashMap<VarName, PointerValue<'f>>,
    pub function: FunctionValue<'f>,
    pub builder: Builder<'f>,
    pub context: &'f Context,
    pub globals: &'f Functions<'f>,
    pub arg_names: Vec<VarName>,
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

    let output: Option<AnyValueEnum> = match expr {
        Number { ref value, .. } => Some(
            Type::i(32)
                .to_any_type(&fcs.context)
                .into_int_type()
                .const_int(*value as u64, false)
                .into(),
        ),
        Float { ref value, .. } => Some(
            Type::f(32)
                .to_any_type(&fcs.context)
                .into_float_type()
                .const_float(*value)
                .into(),
        ),
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

            let val = fcs.variables.get(&name).unwrap().clone();
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

            if let Some(val) = fcs.variables.get(name) {
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
                        expr.ret_type().to_basic_type(fcs.context),
                        &*name.to_string(),
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
            ..
        } => {
            let lhs_type = fcs.tree.get(*lhs).ret_type().clone();
            let lhs = compile(fcs, *lhs).unwrap();
            let rhs = compile(fcs, *rhs).unwrap();

            match lhs_type.as_type_enum() {
                TypeEnum::Int(_) => {
                    use IntPredicate::*;

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
                            NE, lhs, rhs, "eq"),
                        Equal => fcs.builder.build_int_compare(EQ, lhs, rhs, "eq",),
                        LessThan => fcs.builder.build_int_compare( ULT, lhs, rhs, "lt",),
                        GrtrThan => fcs.builder.build_int_compare( UGT, lhs, rhs, "gt",),
                       LessOrEqual => fcs.builder.build_int_compare( ULE, lhs, rhs, "gt",), 
                       GreaterOrEqual => fcs.builder.build_int_compare( UGE, lhs, rhs, "gt",),
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
                TypeEnum::Ref(_) => {
                    let lhs = lhs.try_into().expect("incorrect type for expression");
                    let rhs = rhs.try_into().expect("incorrect type for expression");

                    let result = match op {
                        Plus => unsafe {
                            fcs.builder.build_in_bounds_gep(lhs, &[rhs], "ptrmath")
                        },
                        _ => todo!(),
                    };

                    Some(result.into())
                },
                _ => unimplemented!(),
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
            let stmts = stmts.iter().peekable();

            for stmt in stmts {
                compile(fcs, *stmt);
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
        Call { a, data, ret_type, .. } => {
            let data = data.unwrap();

            if let Some(output) = internal_function(fcs, data.clone(), ret_type, a.clone()) {
                output
            } else {
                let function = fcs.globals.get_value(data.clone()).unwrap();
                let mut arg_vals = Vec::new();

                for arg in a {
                    let basic_arg: BasicValueEnum =
                        compile(fcs, arg).unwrap().try_into().unwrap();
                    let basic_meta_arg: BasicMetadataValueEnum =
                        basic_arg.try_into().unwrap();
                    arg_vals.push(basic_meta_arg);
                }

                let output = fcs
                    .builder
                    .build_call(function, &*arg_vals, "call")
                    .as_any_value_enum();

                // For external functions, we need to store the output (even
                // if the output is void) in a temporary variable in order
                // to prevent llvm from optimizing it out.
                // TODO: make this only happen if function returns void
                if fcs.globals.get_type(data).unwrap().is_never() {
                    let x = fcs.builder.build_alloca(fcs.context.i32_type(), "temp");
                    let output_basic: BasicValueEnum = output.try_into().unwrap();
                    fcs.builder.build_store(x, output_basic);
                }

                Some(output)
            }
        },
        Member { .. } => {
            let ptr = compile_as_ptr(fcs, expr_id);
            let val = fcs.builder.build_load(ptr, "load");

            Some(val.as_any_value_enum())
        },
        StructLit {
            var_type: struct_type,
            fields,
            ..
        } => {
            // TODO: set each field individually instead of using const_struct
            let mut compiled_fields = Vec::new();

            let TypeEnum::Struct(struct_type) = struct_type.as_type_enum() else { panic!() };

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
        Return { to_return, .. } => {
            let return_value: BasicValueEnum =
                compile(fcs, to_return).unwrap().try_into().unwrap();

            fcs.builder.build_return(Some(&return_value));
            None
        },
        ArrayLit { var_type, parts } => {
            let mut c_parts = Vec::new();

            for part in parts {
                c_parts.push(compile(fcs, part).unwrap());
            }

            let c_parts = c_parts.iter();

            let TypeEnum::Array(s) = var_type.as_type_enum() else { panic!() };

            let array = match s.base().to_any_type(fcs.context) {
                AnyTypeEnum::IntType(t) => t.const_array(
                    &*c_parts.map(|p| p.into_int_value()).collect::<Vec<_>>(),
                ),
                AnyTypeEnum::FloatType(t) => t.const_array(
                    &*c_parts.map(|p| p.into_float_value()).collect::<Vec<_>>(),
                ),
                AnyTypeEnum::PointerType(t) => t.const_array(
                    &*c_parts.map(|p| p.into_pointer_value()).collect::<Vec<_>>(),
                ),
                AnyTypeEnum::VectorType(t) => t.const_array(
                    &*c_parts.map(|p| p.into_vector_value()).collect::<Vec<_>>(),
                ),
                AnyTypeEnum::StructType(t) => t.const_array(
                    &*c_parts.map(|p| p.into_struct_value()).collect::<Vec<_>>(),
                ),
                AnyTypeEnum::ArrayType(t) => t.const_array(
                    &*c_parts.map(|p| p.into_array_value()).collect::<Vec<_>>(),
                ),
                _ => todo!(),
            };

            Some(array.as_any_value_enum())
        },
        Index { .. } => {
            let ptr = compile_as_ptr(fcs, expr_id);
            let val = fcs.builder.build_load(ptr, "load");

            Some(val.as_any_value_enum())
        },
        _ => todo!(),
    };

    if crate::DEBUG {
        println!("done compiling: {expr_id:?}");
    }

    output
}

fn compile_as_ptr_unless_already_ptr<'comp>(
    fcs: &mut FunctionCompilationState<'comp>,
    expr_id: ExprID,
) -> PointerValue<'comp> {
    match fcs.tree.get(expr_id).ret_type().clone().as_type_enum() {
        TypeEnum::Ref(_) => compile(fcs, expr_id).unwrap().into_pointer_value(),
        _ => compile_as_ptr(fcs, expr_id),
    }
}

fn compile_as_ptr<'comp>(
    fcs: &mut FunctionCompilationState<'comp>,
    expr_id: ExprID,
) -> PointerValue<'comp> {
    match fcs.tree.get(expr_id) {
        Ident { name, .. } => match fcs.variables.get(&name) {
            Some(var) => var.clone(),
            None => {
                let ident_no_ptr: BasicValueEnum =
                    compile(fcs, expr_id).unwrap().try_into().unwrap();

                let ident_type =
                    fcs.tree.get(expr_id).ret_type().to_basic_type(fcs.context);
                let new_temp = fcs.builder.build_alloca(ident_type, "temp");
                fcs.builder.build_store(new_temp, ident_no_ptr);
                new_temp
            },
        },
        Member { object, field, .. } => {
            let typ = fcs.tree.get(object).ret_type();

            let TypeEnum::Struct(struct_type) = 
                typ.complete_deref().as_type_enum() 
                    else { panic!() };

            let field_index = struct_type.get_field_index(&field);

            let object = compile_as_ptr_unless_already_ptr(fcs, object);

            fcs.builder
                .build_struct_gep(object, field_index as u32, "access")
                .unwrap()
        },
        Index {
            object,
            index,
            ret_type,
        } => {
            let object = compile_as_ptr(fcs, object);
            let index = compile(fcs, index).unwrap().into_int_value();

            let gepped_array = unsafe {
                fcs.builder.build_gep(
                    object,
                    &[fcs.context.i32_type().const_int(0, false), index],
                    "gep",
                )
            };

            fcs.builder
                .build_cast(
                    InstructionOpcode::BitCast,
                    gepped_array,
                    ret_type.get_ref().to_basic_type(fcs.context),
                    "cast",
                )
                .into_pointer_value()
        },
        _ => todo!(),
    }
}

fn internal_function<'comp>(
    fcs: &mut FunctionCompilationState<'comp>,
    data: UniqueFuncInfo,
    _ret_type: Type,
    args: Vec<ExprID>,
) -> Option<Option<AnyValueEnum<'comp>>> {
    let output = match &*data.og_name().to_string() {
        "alloc" => {
            let alloc_typ = data.arg_types()[1].to_basic_type(&fcs.context);
            let alloc_count = compile(fcs, args[0]).unwrap().try_into().unwrap();

            Some(
                fcs.builder
                    .build_array_malloc(alloc_typ, alloc_count, "malloc")
                    .unwrap()
                    .as_any_value_enum(),
            )
        },
        "free" => {
            let ptr = compile(fcs, args[0]).unwrap().try_into().unwrap();

            fcs.builder.build_free(ptr);

            None
        },
        "memmove" => {
            let src: PointerValue = compile(fcs, args[0]).unwrap().try_into().unwrap();

            let dest: PointerValue = compile(fcs, args[1]).unwrap().try_into().unwrap();

            let size = compile(fcs, args[2]).unwrap().try_into().unwrap();

            fcs.builder.build_memmove(dest, 1, src, 1, size).unwrap();

            None
        },
        "size_of" => {
            let typ = data.arg_types()[0].to_basic_type(&fcs.context);
            let size = typ.size_of().unwrap().as_any_value_enum();

            Some(size)
        }
        _ => {
            return None
        },
    };

    Some(output)
}

pub fn get_alignment<'a, 'ctx>(basic: &'a BasicTypeEnum<'ctx>) -> IntValue<'ctx> {
    match basic {
        BasicTypeEnum::ArrayType(t) => t.get_alignment(),
        BasicTypeEnum::FloatType(t) => t.get_alignment(),
        BasicTypeEnum::IntType(t) => t.get_alignment(),
        BasicTypeEnum::PointerType(t) => t.get_alignment(),
        BasicTypeEnum::StructType(t) => t.get_alignment(),
        BasicTypeEnum::VectorType(t) => t.get_alignment(),
    }
}
