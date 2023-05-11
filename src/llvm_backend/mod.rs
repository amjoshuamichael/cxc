use crate::hlr::hlr_data::VariableInfo;
use crate::mir::{MLine, MIR, MMemLoc, MOperand, MExpr, MLit, MReg, MAddr, MAddrExpr, MAddrReg};
use crate::typ::{Kind, ReturnStyle};
use crate::{unit::*, Type, FuncType};
use inkwell::attributes::{Attribute, AttributeLoc};
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::{values::*, AddressSpace};
use inkwell::types::*;
use std::collections::BTreeMap;
use operations::compile_bin_op;

use self::inkwell_utils::const_array;
use self::operations::compile_unar_op;

pub struct FunctionCompilationState<'a> {
    pub mir: MIR,
    pub memlocs: BTreeMap<MMemLoc, BasicValueEnum<'static>>,
    pub addresses: BTreeMap<MAddr, PointerValue<'static>>,
    pub blocks: Vec<BasicBlock<'static>>,
    pub used_functions: BTreeMap<UniqueFuncInfo, FunctionValue<'static>>,
    pub function: FunctionValue<'static>,
    pub builder: Builder<'static>,
    pub context: &'static Context,
    pub comp_data: &'a CompData,
}

mod operations;
mod inkwell_utils;

// ONLY adds if nescessary
pub fn add_sret_attribute_to_func(
    function: &mut FunctionValue<'static>,
    context: &'static Context,
    func_type: &FuncType,
) {
    if func_type.ret.return_style() == ReturnStyle::Sret {
        let sret_id = Attribute::get_named_enum_kind_id("sret");
        let sret_attribute = context.create_type_attribute(
            sret_id, 
            func_type.ret.to_any_type(context)
        );
        function.add_attribute(AttributeLoc::Param(0), sret_attribute);
    }

    #[cfg(not(any(target_arch = "arm", target_arch = "aarch64")))]
    for (a, arg) in func_type.args.iter().enumerate() {
        if arg.arg_style() == ArgStyle::Pointer {
            let byval_id = Attribute::get_named_enum_kind_id("byval");
            let byval_attribute = context.create_type_attribute(
                byval_id, 
                arg.to_any_type(context)
            );

            let arg_pos_offset = if func_type.ret.return_style() == ReturnStyle::Sret {
                1
            } else { 
                0 
            };

            function.add_attribute(
                AttributeLoc::Param(a as u32 + arg_pos_offset), 
                byval_attribute
            );
        }
    }
}

// Adds no matter what
pub fn add_sret_attribute_to_call_site(
    callsite: &mut CallSiteValue<'static>,
    context: &'static Context,
    ret: &Type,
) {
    let sret_id = Attribute::get_named_enum_kind_id("sret");
    let sret_attribute = context.create_type_attribute(sret_id, ret.to_any_type(context));
    callsite.add_attribute(AttributeLoc::Param(0), sret_attribute);
}

fn build_stack_allocas(
    fcs: &mut FunctionCompilationState,
) {
    for (name, info) in fcs.mir.variables.iter() {
        if info.arg_index.is_some() {
            continue;
        }

        let var_type = &info.typ;

        let var_ptr: PointerValue<'static> = fcs.builder.build_alloca(
            var_type.to_basic_type(fcs.context), 
            &*name.to_string()
        );

        fcs.addresses.insert(MAddr::Var(name.clone()), var_ptr);
    }
}

fn create_blocks(
    fcs: &mut FunctionCompilationState,
) {
    for index in 0..fcs.mir.block_count {
        let block = fcs.context.append_basic_block(fcs.function, &format!("b{index}"));
        fcs.blocks.push(block);
    }
}

fn get_used_functions(
    fcs: &mut FunctionCompilationState,
    module: &Module<'static>,
) {
    for info in fcs.mir.dependencies.iter() {
        if let Some(function_value) = 
            module.get_function(&info.to_string(&fcs.comp_data.generations)) {

            fcs.used_functions.insert(info.clone(), function_value);
        }
    }
}

pub fn compile_routine(fcs: &mut FunctionCompilationState, module: &Module<'static>) {
    if crate::LLVM_DEBUG {
        println!("Compiling: {}", fcs.mir.info.name);
    }

    build_stack_allocas(fcs);
    create_blocks(fcs);

    // TODO: if we can move this to the make_new_fcs function then we can remove the comp 
    // data field from FunctionCompilationState
    get_used_functions(fcs, module);

    for index in 0..fcs.mir.lines.len() {
        compile_mline(fcs, index);
    }
}

pub fn compile_mline(fcs: &mut FunctionCompilationState, index: usize) {
    #[cfg(feature = "llvm-debug")]
    println!("{:?}", fcs.mir.lines[index]);

    match &fcs.mir.lines[index] {
        MLine::Set { l, r } => {
            let r = compile_expr(fcs, &r, Some(l));
            fcs.memlocs.insert(MMemLoc::Reg(*l), r.unwrap());
        },
        MLine::SetAddr { l, r } => {
            let r = compile_addr_expr(fcs, &r, Some(l));
            fcs.addresses.insert(MAddr::Reg(*l), r);
        },
        MLine::Store { l, val } => {
            let val = compile_operand(fcs, &val);
            let l = get_addr(fcs, l);
            fcs.builder.build_store(l, val);
        },
        MLine::Return(Some(loc)) => {
            let loc = compile_operand(fcs, loc);
            fcs.builder.build_return(Some(&loc));
        },
        MLine::Return(None) => { fcs.builder.build_return(None); },
        MLine::Expr(expr) => {
            compile_expr(fcs, expr, None);
        }
        MLine::Marker(index) => {
            fcs.builder.position_at_end(fcs.blocks[*index as usize]);
        },
        MLine::Goto(index) => {
            fcs.builder.build_unconditional_branch(fcs.blocks[*index as usize]);
        },
        MLine::Branch { if_, yes, no } => {
            let if_ = compile_operand(fcs, if_);
            fcs.builder.build_conditional_branch(
                if_.into_int_value(), 
                fcs.blocks[*yes as usize], 
                fcs.blocks[*no as usize]
            );
        },
        _ => todo!(),
    }
}

pub fn compile_expr(fcs: &FunctionCompilationState, expr: &MExpr, reg: Option<&MReg>) 
    -> Option<BasicValueEnum<'static>> {
    let reg_name = &*reg.map(MReg::to_string).unwrap_or_default(); 
    match expr {
        MExpr::MemLoc(memloc) => { Some(load_memloc(fcs, memloc)) },
        MExpr::Addr(addr) => { 
            let addr = get_addr(fcs, addr);

            let reg_type = &fcs.mir.reg_types[reg.unwrap()];
            let loaded = fcs.builder.build_load(reg_type.to_basic_type(fcs.context), addr, reg_name);
            Some(loaded) 
        },
        MExpr::Ref { on } => {
            Some(get_addr(fcs, on).as_basic_value_enum())
        }
        MExpr::Deref { to, on } => {
            let obj = compile_operand(fcs, on).into_pointer_value();
            let loaded = 
                fcs.builder.build_load(to.to_basic_type(fcs.context), obj, reg_name);
            Some(loaded.as_basic_value_enum())
        }
        MExpr::Array { elem_type, elems } => {
            let elems = elems.into_iter().map(|elem| compile_operand(fcs, elem));

            let array = const_array(elem_type.to_any_type(fcs.context), elems);
            Some(array.as_basic_value_enum())
        }
        MExpr::BinOp { ret_type, op, l, r, } => {
            Some(compile_bin_op(
                fcs, 
                ret_type, 
                *op, 
                compile_operand(fcs, l), 
                compile_operand(fcs, r), 
                reg_name,
            ))
        },
        MExpr::UnarOp { ret_type, op, hs } => {
            Some(compile_unar_op(
                fcs, 
                ret_type,
                *op,
                hs,
                reg_name,
            ))
        }
        MExpr::Call { typ, f, a } => {
            match &*f.name.to_string() {
                "memcpy" => {
                    let src = compile_operand(fcs, &a[0]).into_pointer_value();
                    let dest = compile_operand(fcs, &a[1]).into_pointer_value();
                    let size = compile_operand(fcs, &a[2]).into_int_value();

                    fcs.builder.build_memcpy(dest, 1, src, 1, size).unwrap();

                    None
                },
                "memmove" => {
                    let src = compile_operand(fcs, &a[0]).into_pointer_value();
                    let dest = compile_operand(fcs, &a[1]).into_pointer_value();
                    let size = compile_operand(fcs, &a[2]).into_int_value();

                    fcs.builder.build_memmove(dest, 1, src, 1, size).unwrap();

                    None

                },
                "alloc" => {
                    let alloc_typ = f.generics[0].to_basic_type(fcs.context);
                    let alloc_count = compile_operand(fcs, &a[0]).into_int_value();

                    Some(
                        fcs.builder
                            .build_array_malloc(alloc_typ, alloc_count, "malloc")
                            .unwrap()
                            .as_basic_value_enum(),
                    )
                },
                "free" => {
                    let ptr = compile_operand(fcs, &a[0]).into_pointer_value();
                    let free_type = f.generics[0].get_ref().to_basic_type(fcs.context);
                    let casted_ptr = ptr.const_cast(free_type.try_into().unwrap());

                    fcs.builder.build_free(casted_ptr);

                    None
                },
                "size_of" => {
                    let typ = f.generics[0].to_basic_type(fcs.context);
                    let size = typ.size_of().unwrap().as_basic_value_enum();

                    Some(size)
                },
                "cast" => {
                    let src = compile_operand(fcs, &a[0]);

                    let src_type = f.generics[0].to_basic_type(fcs.context);
                    let src_var = fcs.builder.build_alloca(src_type, "castsrc");

                    let dest_type = f.generics[1].to_basic_type(fcs.context);
                    let dest_var = fcs.builder.build_alloca(dest_type, "castdest");

                    fcs.builder.build_store(src_var, src);

                    fcs.builder
                        .build_memcpy(dest_var, 1, src_var, 1, dest_type.size_of().unwrap())
                        .unwrap();
                    let casted_loaded = fcs.builder.build_load(dest_type, dest_var, "cast-load");

                    Some(casted_loaded.as_basic_value_enum())
                },
                "typeobj" => {
                    let typ = f.generics[0].clone();
                    let typusize: u64 = unsafe { std::mem::transmute(typ) };

                    Some(
                        fcs.context
                             .i64_type()
                             .const_int(typusize, false)
                             .const_to_pointer(
                                 fcs.context
                                     .i64_type()
                                     .ptr_type(AddressSpace::default())
                             )
                             .as_basic_value_enum()
                    )
                }
                _ => {
                    let func = fcs.used_functions.get(f).unwrap();

                    let arg_vals = a.into_iter()
                        .map(|arg| {
                            let basic_arg = compile_operand(fcs, arg);
                            BasicMetadataValueEnum::try_from(basic_arg).unwrap()
                        })
                        .collect::<Vec<_>>();

                    let mut callsite = fcs.builder.build_call(
                        *func, 
                        &*arg_vals,
                        &*reg.map(MReg::to_string).unwrap_or_default(),
                    );

                    if typ.ret.return_style() == ReturnStyle::Sret {
                        add_sret_attribute_to_call_site(&mut callsite, fcs.context, &typ.ret);
                    }

                    callsite.try_as_basic_value().left()
                }
            }
        },
        _ => todo!("{:?}", expr),
    }
}

pub fn compile_addr_expr(
    fcs: &FunctionCompilationState, 
    expr: &MAddrExpr, 
    reg: Option<&MAddrReg>
) -> PointerValue<'static> {
    let reg_name = &*reg.map(MAddrReg::to_string).unwrap_or_default(); 

    match expr {
         MAddrExpr::Member { object_type, object, field_index } => {
            let object = fcs.addresses[object];

            fcs.builder.build_struct_gep(
                object_type.to_basic_type(fcs.context),
                object,
                *field_index,
                reg_name,
            ).unwrap()
        },
        MAddrExpr::Index { array_type, element_type, object, index } => {
            let object = get_addr(fcs, object);
            let index = compile_operand(fcs, index).into_int_value();

            let gepped_array = unsafe {
                fcs.builder.build_in_bounds_gep(
                    array_type.to_basic_type(fcs.context),
                    object,
                    &[fcs.context.i32_type().const_int(0, false), index],
                    reg_name,
                )
            };


            fcs.builder
                .build_cast(
                    InstructionOpcode::BitCast,
                    gepped_array,
                    element_type.get_ref().to_basic_type(fcs.context),
                    "cast",
                )
                .into_pointer_value()
        }
        MAddrExpr::Expr(expr) => compile_expr(fcs, expr, None).unwrap().into_pointer_value(),
        MAddrExpr::Addr(addr) => get_addr(fcs, addr),
        _ => todo!("{:?}", expr),
    }
}

pub fn compile_operand(fcs: &FunctionCompilationState, operand: &MOperand) -> BasicValueEnum<'static> {
    match operand {
        MOperand::MemLoc(memloc) => load_memloc(fcs, memloc),
        MOperand::Lit(lit) => compile_lit(fcs, lit),
    }
}

pub fn compile_lit(fcs: &FunctionCompilationState, lit: &MLit) -> BasicValueEnum<'static> {
    match lit {
        MLit::Int { size, val } => {
            match size {
                8 => fcs.context.i8_type().const_int(*val as u64, false),
                16 => fcs.context.i16_type().const_int(*val as u64, false),
                32 => fcs.context.i32_type().const_int(*val as u64, false),
                64 => fcs.context.i64_type().const_int(*val as u64, false),
                128 => fcs.context.i128_type().const_int(*val as u64, false),
                size => fcs.context.custom_width_int_type(*size as u32).const_int(*val as u64, false),
            }.as_basic_value_enum()
        },
        MLit::Float { size, val } => {
            match size {
                32 => fcs.context.f32_type().const_float(*val as f64),
                64 => fcs.context.f64_type().const_float(*val as f64),
                _ => unreachable!(),
            }.as_basic_value_enum()
        },
        MLit::Bool(val) => {
            fcs.context.bool_type().const_int(*val as u64, false).as_basic_value_enum()
        },
    }
}

pub fn load_memloc(fcs: &FunctionCompilationState, memloc: &MMemLoc) -> BasicValueEnum<'static> {
    match memloc {
        MMemLoc::Reg(_) => fcs.memlocs[memloc],
        MMemLoc::Var(name) => {
            match fcs.mir.variables.get(name) {
                None =>  {
                    let global = &fcs.comp_data.globals[&name];
                    BasicValueEnum::PointerValue(global.1)
                }
                Some(VariableInfo { arg_index: Some(arg_index), .. }) => {
                    fcs.function.get_nth_param(*arg_index).unwrap().into()
                }
                Some(VariableInfo { typ: var_type, .. }) => {
                    fcs.builder.build_load(
                        fcs.mir.variables[name].typ.to_basic_type(fcs.context), 
                        fcs.addresses[&MAddr::Var(name.clone())], 
                        &*name.to_string()
                    )
                }
            }
        }
    }
}

pub fn get_addr(fcs: &FunctionCompilationState, addr: &MAddr) -> PointerValue<'static> {
    fcs.addresses[addr]
}
