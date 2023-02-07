use crate::hlr::expr_tree::{ExprID, NodeData::*};
use crate::hlr::hlr_data::DataFlow;
use crate::hlr::prelude::*;
use crate::lex::VarName;
use crate::parse::Opcode::*;
use crate::typ::{Kind, ReturnStyle};
use crate::unit::*;
use crate::{Type, TypeEnum};
use core::cell::RefCell;
use inkwell::attributes::{Attribute, AttributeLoc};
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::*;
use inkwell::values::*;
use inkwell::FloatPredicate;
use inkwell::IntPredicate;
use std::collections::HashMap;
use std::rc::Rc;

pub struct FunctionCompilationState {
    pub tree: ExprTree,
    pub data_flow: DataFlow,
    pub variables: HashMap<VarName, PointerValue<'static>>,
    pub used_functions: HashMap<UniqueFuncInfo, FunctionValue<'static>>,
    pub function: FunctionValue<'static>,
    pub builder: Builder<'static>,
    pub context: &'static Context,
    pub comp_data: Rc<CompData>,
    pub arg_names: Vec<VarName>,
    pub llvm_ir_uuid: RefCell<u32>,
    pub ret_type: Type,
}

impl FunctionCompilationState {
    fn new_uuid<'a>(&self) -> String {
        let current_uuid = *self.llvm_ir_uuid.borrow();
        let output = current_uuid.to_string();
        self.llvm_ir_uuid.replace(current_uuid + 1);
        String::from("t") + &*output
    }
}

pub fn compile_routine(
    fcs: &mut FunctionCompilationState,
    module: &Module<'static>,
) -> Option<AnyValueEnum<'static>> {
    if fcs.ret_type.return_style() == ReturnStyle::Sret {
        // informs llvm that we are returning via the first parameter in the
        // function, which is a pointer.
        let sret_id = Attribute::get_named_enum_kind_id("sret");
        let any_type = fcs.ret_type.to_any_type(&fcs.context);
        let sret_attribute = fcs.context.create_type_attribute(sret_id, any_type);
        fcs.function
            .add_attribute(AttributeLoc::Param(0), sret_attribute);
    }

    build_stack_allocas(
        &mut fcs.variables,
        &fcs.tree,
        &fcs.data_flow,
        &mut fcs.builder,
        &fcs.context,
    );
    get_used_functions(&mut fcs.used_functions, &fcs.tree, module);
    compile(fcs, fcs.tree.root)
}

fn build_stack_allocas(
    variables: &mut HashMap<VarName, PointerValue<'static>>,
    tree: &ExprTree,
    data_flow: &DataFlow,
    builder: &mut Builder<'static>,
    context: &'static Context,
) {
    for expr in tree.iter() {
        match expr.1 {
            MakeVar { ref name, .. } => {
                if variables.contains_key(name) {
                    continue;
                }

                let var_type = &data_flow.get(&name).unwrap().typ;

                let var_ptr: PointerValue<'static> =
                    builder.build_alloca(var_type.to_basic_type(context), &*name.to_string());

                variables.insert(name.clone(), var_ptr);
            },
            _ => {},
        }
    }
}

fn get_used_functions(
    used_functions: &mut HashMap<UniqueFuncInfo, FunctionValue<'static>>,
    tree: &ExprTree,
    module: &Module<'static>,
) {
    for (_, call) in tree.iter() {
        if !matches!(call, NodeData::Call { .. }) {
            continue;
        }

        let unique_info = tree.unique_func_info_of_call(call);
        if let Some(function_value) = module.get_function(&*unique_info.to_string()) {
            used_functions.insert(unique_info, function_value);
        }
    }
}

fn compile(fcs: &FunctionCompilationState, expr_id: ExprID) -> Option<AnyValueEnum<'static>> {
    let expr = fcs.tree.get(expr_id);

    if crate::LLVM_DEBUG {
        println!("compiling: {}", expr.to_string(&fcs.tree));
    }

    let output: Option<AnyValueEnum<'static>> = match expr {
        Number {
            ref value,
            ref size,
        } => Some(
            Type::i(*size)
                .to_any_type(&fcs.context)
                .into_int_type()
                .const_int(*value as u64, false)
                .into(),
        ),
        Float { ref value, .. } => Some(
            Type::f32()
                .to_any_type(&fcs.context)
                .into_float_type()
                .const_float(*value)
                .into(),
        ),
        Bool { ref value } => Some(
            Type::bool()
                .to_any_type(&fcs.context)
                .into_int_type()
                .const_int(if *value { 1 } else { 0 }, false)
                .into(),
        ),
        MakeVar {
            ref name, ref rhs, ..
        } => {
            let to_store = compile(fcs, *rhs).unwrap();
            let to_store_basic: BasicValueEnum = to_store.try_into().unwrap();

            if fcs.arg_names.contains(name) {
                let param = fcs.function.get_nth_param(0).unwrap();
                fcs.builder
                    .build_store(param.try_into().unwrap(), to_store_basic);
                return Some(to_store);
            }

            let val = fcs.variables.get(name).unwrap();

            fcs.builder.build_store(*val, to_store_basic);

            return Some(to_store);
        },
        Ident { ref name, .. } => {
            if let Some(param_index) = fcs.arg_names.iter().position(|arg| arg == name) {
                let param = fcs
                    .function
                    .get_nth_param(param_index.try_into().unwrap())
                    .unwrap();
                return Some(param.into());
            }

            if let Some(global) = fcs.comp_data.globals.get(name) {
                let out = global.1.as_pointer_value();
                return Some(AnyValueEnum::PointerValue(out));
            }

            let val = fcs.variables.get(&name).unwrap().clone();
            let loaded = fcs.builder.build_load(val, &*fcs.new_uuid());

            Some(loaded.into())
        },
        Set {
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
                TypeEnum::Int(_) | TypeEnum::Bool(_) => {
                    use IntPredicate::*;

                    let lhs = lhs.try_into().expect("incorrect type for expression");
                    let rhs = rhs.try_into().expect("incorrect type for expression");

                    let result: IntValue = match op {
                        Plus => fcs.builder.build_int_add(lhs, rhs, "sum"),
                        Minus => fcs.builder.build_int_sub(lhs, rhs, "sub"),
                        Multiplier => fcs.builder.build_int_mul(lhs, rhs, "mul"),
                        Divider => fcs.builder.build_int_unsigned_div(lhs, rhs, "div"),
                        Modulus => fcs.builder.build_int_unsigned_rem(lhs, rhs, "mod"),
                        Inequal => fcs.builder.build_int_compare(NE, lhs, rhs, "eq"),
                        Equal => fcs.builder.build_int_compare(EQ, lhs, rhs, "eq"),
                        LessThan => fcs.builder.build_int_compare(ULT, lhs, rhs, "lt"),
                        GrtrThan => fcs.builder.build_int_compare(UGT, lhs, rhs, "gt"),
                        LessOrEqual => fcs.builder.build_int_compare(ULE, lhs, rhs, "gt"),
                        GreaterOrEqual => fcs.builder.build_int_compare(UGE, lhs, rhs, "gt"),
                        BitShiftL => fcs.builder.build_left_shift(lhs, rhs, "bsl"),
                        BitShiftR => fcs.builder.build_right_shift(lhs, rhs, false, "bsr"),
                        BitAND => fcs.builder.build_and(lhs, rhs, "and"),
                        BitOR => fcs.builder.build_or(lhs, rhs, "or"),
                        BitXOR => fcs.builder.build_xor(lhs, rhs, "xor"),
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
                            Multiplier => fcs.builder.build_float_mul(lhs, rhs, "mul"),
                            Divider => fcs.builder.build_float_div(lhs, rhs, "div"),
                            Modulus => fcs.builder.build_float_rem(lhs, rhs, "mod"),
                            _ => todo!(),
                        };

                        return Some(result.into());
                    }

                    let result: IntValue = match op {
                        Inequal => {
                            fcs.builder
                                .build_float_compare(FloatPredicate::ONE, lhs, rhs, "eq")
                        },
                        Equal => {
                            fcs.builder
                                .build_float_compare(FloatPredicate::OEQ, lhs, rhs, "eq")
                        },
                        LessThan => {
                            fcs.builder
                                .build_float_compare(FloatPredicate::ULT, lhs, rhs, "lt")
                        },
                        GrtrThan => {
                            fcs.builder
                                .build_float_compare(FloatPredicate::UGT, lhs, rhs, "gt")
                        },
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
            let prewhile_block = fcs.context.append_basic_block(fcs.function, "prewhile");
            let whilecode_block = fcs.context.append_basic_block(fcs.function, "whilecode");
            let postwhile_block = fcs.context.append_basic_block(fcs.function, "postwhile");

            fcs.builder.build_unconditional_branch(prewhile_block);
            fcs.builder.position_at_end(prewhile_block);

            let cond = compile(fcs, w).unwrap().into_int_value();

            fcs.builder
                .build_conditional_branch(cond, whilecode_block, postwhile_block);

            fcs.builder.position_at_end(whilecode_block);
            compile(fcs, d);
            fcs.builder.build_unconditional_branch(prewhile_block);

            fcs.builder.position_at_end(postwhile_block);

            None
        },
        UnarOp { op, hs, .. } => match op {
            Ref => Some(compile_as_ptr(fcs, hs).as_any_value_enum()),
            Deref => {
                let var_ptr = compile(fcs, hs).unwrap();
                let val = fcs
                    .builder
                    .build_load(var_ptr.into_pointer_value(), &*fcs.new_uuid());
                Some(val.into())
            },
            Not => {
                let b = compile(fcs, hs).unwrap().into_int_value();
                let val = fcs.builder.build_not(b, "not");
                Some(val.into())
            },
            _ => unreachable!(),
        },
        Call { ref a, .. } => {
            let info = fcs.tree.unique_func_info_of_call(&expr);
            let internal_function_ouptut = internal_function(fcs, &info, a);
            if internal_function_ouptut.is_some() {
                internal_function_ouptut.unwrap()
            } else {
                let function = fcs.used_functions.get(&info).unwrap().clone();
                let mut arg_vals = Vec::new();

                for arg in a {
                    let basic_arg: BasicValueEnum =
                        compile(fcs, *arg).unwrap().try_into().unwrap();
                    let basic_meta_arg: BasicMetadataValueEnum = basic_arg.try_into().unwrap();
                    arg_vals.push(basic_meta_arg);
                }

                let output = fcs
                    .builder
                    .build_call(function, &*arg_vals, "call")
                    .as_any_value_enum();

                Some(output)
            }
        },
        FirstClassCall { ref f, ref a, .. } => {
            let function_ptr = compile(fcs, *f).unwrap().into_pointer_value();
            let function_ptr = CallableValue::try_from(function_ptr).unwrap();

            let mut arg_vals = Vec::new();

            for arg in a {
                let basic_arg: BasicValueEnum = compile(fcs, *arg).unwrap().try_into().unwrap();
                let basic_meta_arg: BasicMetadataValueEnum = basic_arg.try_into().unwrap();
                arg_vals.push(basic_meta_arg);
            }

            let output = fcs
                .builder
                .build_call(function_ptr, &*arg_vals, "call")
                .as_any_value_enum();

            Some(output)
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
            if let Some(to_return) = to_return {
                let ret_val: BasicValueEnum =
                    compile(fcs, to_return).unwrap().try_into().unwrap();

                let type_of_return = fcs.tree.get(to_return).ret_type();
                let raw_type_of_return = type_of_return.raw_return_type();

                if type_of_return == raw_type_of_return {
                    fcs.builder.build_return(Some(&ret_val));
                } else {
                    let casted_value = fcs.builder.build_alloca(
                        raw_type_of_return.to_basic_type(&fcs.context),
                        "castedret",
                    );
                    fcs.builder.build_store(casted_value, ret_val);
                    let loaded_cast = fcs.builder.build_load(casted_value, "loadcast");
                    fcs.builder.build_return(Some(&loaded_cast));
                }
            } else {
                fcs.builder.build_return(None);
            }

            None
        },
        ArrayLit { var_type, parts } => {
            let mut c_parts = Vec::new();

            for part in parts {
                c_parts.push(compile(fcs, part).unwrap());
            }

            let c_parts = c_parts.iter();

            let TypeEnum::Array(s) = var_type.as_type_enum() else { panic!() };

            let array = match s.base.clone().to_any_type(fcs.context) {
                AnyTypeEnum::IntType(t) => {
                    t.const_array(&*c_parts.map(|p| p.into_int_value()).collect::<Vec<_>>())
                },
                AnyTypeEnum::FloatType(t) => {
                    t.const_array(&*c_parts.map(|p| p.into_float_value()).collect::<Vec<_>>())
                },
                AnyTypeEnum::PointerType(t) => {
                    t.const_array(&*c_parts.map(|p| p.into_pointer_value()).collect::<Vec<_>>())
                },
                AnyTypeEnum::VectorType(t) => {
                    t.const_array(&*c_parts.map(|p| p.into_vector_value()).collect::<Vec<_>>())
                },
                AnyTypeEnum::StructType(t) => {
                    t.const_array(&*c_parts.map(|p| p.into_struct_value()).collect::<Vec<_>>())
                },
                AnyTypeEnum::ArrayType(t) => {
                    t.const_array(&*c_parts.map(|p| p.into_array_value()).collect::<Vec<_>>())
                },
                _ => todo!(),
            };

            Some(array.as_any_value_enum())
        },
        Index { .. } => {
            let ptr = compile_as_ptr(fcs, expr_id);
            let val = fcs.builder.build_load(ptr, "load");

            Some(val.as_any_value_enum())
        },
    };

    if crate::LLVM_DEBUG {
        println!("done compiling: {}", fcs.tree.get(expr_id).to_string(&fcs.tree));
    }

    output
}

fn compile_as_ptr_unless_already_ptr(
    fcs: &FunctionCompilationState,
    expr_id: ExprID,
) -> PointerValue<'static> {
    match fcs.tree.get(expr_id).ret_type().clone().as_type_enum() {
        TypeEnum::Ref(_) => compile(fcs, expr_id).unwrap().into_pointer_value(),
        _ => compile_as_ptr(fcs, expr_id),
    }
}

fn compile_as_ptr(fcs: &FunctionCompilationState, expr_id: ExprID) -> PointerValue<'static> {
    match fcs.tree.get(expr_id) {
        Ident { name, .. } => match fcs.variables.get(&name) {
            Some(var) => var.clone(),
            None => {
                let ident_no_ptr: BasicValueEnum =
                    compile(fcs, expr_id).unwrap().try_into().unwrap();

                let ident_type = fcs.tree.get(expr_id).ret_type().to_basic_type(fcs.context);
                let new_temp = fcs.builder.build_alloca(ident_type, "tempptr");
                fcs.builder.build_store(new_temp, ident_no_ptr);
                new_temp
            },
        },
        Member { object, field, .. } => {
            let typ = fcs.tree.get(object).ret_type();

            let complete_deref = typ.complete_deref();
            let TypeEnum::Struct(struct_type) = complete_deref
                .as_type_enum() 
                    else { panic!() };

            let field_index = struct_type.get_field_index(&field);

            let object = compile_as_ptr_unless_already_ptr(fcs, object);

            fcs.builder
                .build_struct_gep(object, field_index as u32, &*format!("{field}access"))
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
                fcs.builder.build_in_bounds_gep(
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
        other_expression => {
            let value = compile(fcs, expr_id).unwrap();

            let ptr = fcs
                .builder
                .build_alloca(other_expression.ret_type().to_basic_type(fcs.context), "temp");

            fcs.builder
                .build_store(ptr, BasicValueEnum::try_from(value).unwrap());

            ptr
        },
    }
}

fn internal_function<'comp>(
    fcs: &FunctionCompilationState,
    info: &UniqueFuncInfo,
    args: &Vec<ExprID>,
) -> Option<Option<AnyValueEnum<'static>>> {
    let output = match &*info.og_name().to_string() {
        "alloc" => {
            let alloc_typ = info.generics()[0].to_basic_type(&fcs.context);
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
        "memcpy" => {
            let src: PointerValue = compile(fcs, args[0]).unwrap().try_into().unwrap();
            let dest: PointerValue = compile(fcs, args[1]).unwrap().try_into().unwrap();
            let size = compile(fcs, args[2]).unwrap().try_into().unwrap();

            fcs.builder.build_memcpy(dest, 1, src, 1, size).unwrap();

            None
        },
        "size_of" => {
            let typ = info.generics()[0].to_basic_type(&fcs.context);
            let size = typ.size_of().unwrap().as_any_value_enum();

            Some(size)
        },
        "write" => {
            let src: BasicValueEnum = compile(fcs, args[0]).unwrap().try_into().unwrap();

            let dest: PointerValue = compile(fcs, args[1]).unwrap().try_into().unwrap();
            let dest_loaded: PointerValue =
                fcs.builder.build_load(dest, "loaddest").try_into().unwrap();

            fcs.builder.build_store(dest_loaded, src);

            None
        },
        _ => return None,
    };

    Some(output)
}
