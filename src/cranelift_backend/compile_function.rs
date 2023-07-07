use std::{collections::{BTreeMap, BTreeSet}, iter::once};

use cranelift::{codegen::{ir::{StackSlot, FuncRef, InstBuilderBase}, self}, prelude::{Variable, FunctionBuilder, Block, FunctionBuilderContext, Value as CLValue, InstBuilder, IntCC, MemFlags, StackSlotData, StackSlotKind, types as cl_types, FloatCC, Signature, isa::TargetFrontendConfig, Type as CLType}};
use cranelift_module::Module;

use crate::{mir::{MIR, MLine, MExpr, MMemLoc, MOperand, MLit, MAddr, MAddrExpr, MAddrReg, MReg, MCallable}, VarName, Type, parse::Opcode, TypeEnum, UniqueFuncInfo, FuncType, typ::ReturnStyle, hlr::hlr_data::{ArgIndex, VariableInfo}, cranelift_backend::variables_in_mline::{self, VarInMIR}, RefType, IntType};

use super::{to_cl_type::{ToCLType, func_type_to_signature}, variables_in_mline::variables_in, CraneliftBackend, external_function::{ExternalFuncData, build_external_func_call}};


#[derive(PartialEq, Eq, Clone, Copy)]
enum VarLocation {
    Stack,
    Reg,
}

pub fn make_fcs<'a>(
    backend: &'a mut CraneliftBackend,
    func_builder_context: &'a mut FunctionBuilderContext,
    context: &'a mut codegen::Context,
    mir: MIR,
) -> (FunctionCompilationState<'a>, Vec<MLine>) {
    let used_functions = mir.dependencies.iter().filter_map(|info| {
        let (func_id, info) = 
            if let Some((_, func_id)) = backend.cl_function_data.get(info) {
                (*func_id, info.clone())
            } else if matches!(&*info.name, "alloc" | "free") {
                let intrinsic_name = format!("${}", info.name);
                let info = UniqueFuncInfo::from(VarName::from(&*intrinsic_name));

                return None;
                //(backend.cl_function_data[&info].1, info)
            } else {
                return None
            };

        let func_ref = backend.module.declare_func_in_func(func_id, &mut context.func);

        Some((info, func_ref))
    }).collect();

    let mut builder = FunctionBuilder::new(&mut context.func, func_builder_context);

    let entry_block = builder.create_block();
    // get function paramaters into the entry block
    builder.append_block_params_for_function_params(entry_block);
    builder.switch_to_block(entry_block);
    // tell cranelift this block will have no predecessors, because it is the entry
    builder.seal_block(entry_block);

    let mut used_globals = BTreeMap::<VarName, CLValue>::new();
    let mut var_locations = BTreeMap::<VarName, (VarLocation, VariableInfo)>::new();

    for line in &mir.lines {
        for var in &variables_in_mline::variables_in(line) {
            if let Some(info) = mir.variables.get(&var.name) {
                let name = var.name.clone();
                let info = info.clone();

                if var.is_referenced {
                    var_locations.insert(name, (VarLocation::Stack, info));
                } else if !var_locations.contains_key(&var.name) {
                    var_locations.insert(name, (VarLocation::Reg, info));
                }
            }

            if used_globals.contains_key(&var.name) {
                continue;
            }

            let val = if !mir.variables.contains_key(&var.name) {
                if let Some((_, ptr)) = backend.globals.get(&var.name) {
                    builder.ins().iconst(cl_types::I64, *ptr as i64)
                } else {
                    // global is a function
                    let info = UniqueFuncInfo::from(var.name.clone());

                    let (_, func_id) = &backend.cl_function_data[&info];

                    let func_ref = 
                        backend.module.declare_func_in_func(*func_id, builder.func);
                    builder.ins().func_addr(cl_types::I64, func_ref)
                }
            } else {
                continue;
            };

            used_globals.insert(var.name.clone(), val);
        }
    }

     for line in &mir.lines {
        let call_type = match line {
            MLine::Set { r: MExpr::Call { typ, .. }, .. } => typ,
            MLine::Expr(MExpr::Call { typ, .. }) => typ,
            _ => continue,
        };

        let mut call_sig = backend.module.make_signature();
        func_type_to_signature(call_type, &mut call_sig, false);

        backend.func_signatures.insert(call_type.clone(), call_sig);
    }

    let fcs = FunctionCompilationState {
        builder,
        entry_block: BlockData {
            block: entry_block,
            variables_used: Vec::new(),
        },
        other_blocks: Vec::new(),
        addresses: BTreeMap::new(),
        variables: BTreeMap::new(),
        registers: BTreeMap::new(),
        reg_types: mir.reg_types,
        used_functions,
        used_globals,
        var_locations,
        function_signatures: &backend.func_signatures,
        external_functions: &backend.external_functions,
        frontend_config: &backend.frontend_config,
        ret_type: mir.func_type.ret.clone(),
    };

    (fcs, mir.lines)
}

pub fn compile(
    mut fcs: FunctionCompilationState,
    lines: Vec<MLine>,
) {
    make_stack_allocas(&mut fcs);
    make_blocks(&mut fcs, &lines);

    #[cfg(feature = "xc-debug")]
    let mut l = 0;

    for line in lines {
        compile_mline(&mut fcs, line);

        #[cfg(feature = "xc-debug")]
        {
            print!("{:03}, ", l);
            l += 1;
        }
    }

    #[cfg(feature = "xc-debug")]
    println!();
}

pub struct FunctionCompilationState<'a> {
    builder: FunctionBuilder<'a>,
    addresses: BTreeMap<MAddrReg, CLValue>,
    // functions can return a struct with multiple values over 8 bytes. for example, the 
    // struct {i64, i32}. in llvm, we return this struct directly as it is, but in 
    // cranelift, we *actually* just return multiple values from the function. that's why 
    // we use a vec of CLValues here.
    registers: BTreeMap<MReg, Vec<CLValue>>,
    variables: BTreeMap<VarName, Writeable>,
    var_locations: BTreeMap<VarName, (VarLocation, VariableInfo)>,
    entry_block: BlockData,
    other_blocks: Vec<BlockData>,
    reg_types: BTreeMap<MReg, Type>,
    used_functions: BTreeMap<UniqueFuncInfo, FuncRef>,
    used_globals: BTreeMap<VarName, CLValue>,
    function_signatures: &'a BTreeMap<FuncType, Signature>,
    external_functions: &'a BTreeMap<UniqueFuncInfo, ExternalFuncData>,
    frontend_config: &'a TargetFrontendConfig,
    ret_type: Type,
}

impl<'a> FunctionCompilationState<'a> {
    fn block_params(&mut self, index: usize) -> Vec<CLValue> {
        self
            .other_blocks[index]
            .variables_used
            .iter()
            .filter(|var| !var.is_stack_slot())
            .map(|var| var.load(&mut self.builder))
            .flatten()
            .collect()
    }
}

#[derive(Clone, Debug)]
enum Writeable {
    Var(Variable),
    Arg {
        index: usize,
        types: Vec<CLType>,
        entry_block: Block,
    },
    Slot(StackSlot, Vec<CLType>),
}

impl Writeable {
    fn write(&self, builder: &mut FunctionBuilder, vals: Vec<CLValue>) {
        match self {
            Writeable::Var(var) => {
                assert_eq!(vals.len(), 1);
                builder.def_var(*var, vals[0])
            },
            Writeable::Arg { .. } => panic!(),
            Writeable::Slot(slot, _) => {
                let slot = builder.ins().stack_addr(cl_types::I64, *slot, 0);
                write_vals_to_addr(builder, slot, vals);
            },
        }
    }

    fn mark_use(&self, builder: &mut FunctionBuilder) {
        match self {
            Writeable::Var(var) => { builder.use_var(*var); },
            Writeable::Slot(..) | Writeable::Arg { .. } => {},
        }

    }

    fn load(&self, builder: &mut FunctionBuilder) -> Vec<CLValue> {
        let out = match self {
            // we could use any type here instead of I32, the type is inferred by 
            // cranelift when we use load_offset
            Writeable::Var(_) => vec![
                self.load_offset(builder, cl_types::I32, 0)
            ],
            Writeable::Slot(_, types) | Writeable::Arg { types, .. } => {
                let mut values = Vec::new();
                let mut byte_offset = 0;

                for typ in types {
                    let val = self.load_offset(builder, *typ, byte_offset);
                    values.push(val);
                    byte_offset += typ.bytes();
                }

                values
            }
        };

        out
    }

    fn load_offset(
        &self, 
        builder: &mut FunctionBuilder, 
        typ: CLType, 
        offset: u32
    ) -> CLValue {
        match self {
            Writeable::Var(var) => {
                assert_eq!(offset, 0);
                builder.use_var(*var)
            },
            Writeable::Slot(slot, _) => {
                builder.ins().stack_load(typ, *slot, offset as i32)
            },
            Writeable::Arg { index, types, entry_block } => {
                let all_args = builder.block_params(*entry_block);
                let mut byte_offset = 0;

                for (t, typ) in types.into_iter().enumerate() {
                    if byte_offset == offset {
                        return all_args[index + t];
                    }

                    byte_offset += typ.bytes();
                }

                return all_args[*index];
            },
        }
    }

    fn addr(&self, builder: &mut FunctionBuilder, offset: u32) -> CLValue {
        match self {
            Writeable::Var(_) | Writeable::Arg { .. } => panic!(),
            Writeable::Slot(slot, _) => {
                builder.ins().stack_addr(cl_types::I64, *slot, offset as i32)
            }
        }
    }

    fn is_stack_slot(&self) -> bool { matches!(self, Writeable::Slot(..)) }
}

fn make_stack_allocas(fcs: &mut FunctionCompilationState) {
    let mut arg_offset = 0;

    for (id, (name, (location, VariableInfo { typ, arg_index, .. }))) 
        in fcs.var_locations.iter().enumerate() {
        let var = Variable::from_u32(id as u32);
        let var_types = typ.to_cl_type();

        if *location == VarLocation::Reg {
            let writeable = if let ArgIndex::Some(arg_index) = arg_index {
                let raw_arg_types = typ.raw_arg_type().to_cl_type();
                let raw_arg_type_count = raw_arg_types.len();

                let writeable = Writeable::Arg { 
                    index: *arg_index + arg_offset, 
                    types: raw_arg_types,
                    entry_block: fcs.entry_block.block,
                };

                arg_offset += raw_arg_type_count - 1;

                writeable
            } else if *arg_index == ArgIndex::SRet {
                assert_eq!(arg_offset, 0);
                arg_offset += 1;

                Writeable::Arg { 
                    index: 0, 
                    types: vec![cl_types::I64],
                    entry_block: fcs.entry_block.block,
                }
            } else {
                fcs.builder.declare_var(var, one(var_types));

                Writeable::Var(var)
            };

            fcs.variables.insert(name.clone(), writeable);
        } else {
            if let ArgIndex::Some(_arg_index) = arg_index {
                todo!()
            } else {
                let slot = fcs.builder.create_sized_stack_slot(StackSlotData {
                    kind: StackSlotKind::ExplicitSlot,
                    size: typ.size() as u32,
                });

                fcs.variables.insert(name.clone(), Writeable::Slot(slot, typ.to_cl_type()));
            }
        }
    }
}

#[derive(Debug)]
struct BlockData {
    block: Block,
    variables_used: Vec<Writeable>,
}

fn make_blocks(fcs: &mut FunctionCompilationState, lines: &Vec<MLine>) {
    use BTreeSet as Set;

    #[derive(Default, Debug)]
    struct BData {
        variables: Set<VarName>,
        goes_to: Set<usize>,
    }

    let mut entry_block_data = BData::default();
    let mut other_block_data = Vec::<BData>::new();

    let mut on_entry_block = true;
    let mut processing_bdata = BData::default();

    for line in lines.iter().chain(once(&MLine::Marker(0))) {
        if matches!(line, MLine::Marker(_)) {
            if on_entry_block {
                entry_block_data = processing_bdata;

                on_entry_block = false;
            } else {
                other_block_data.push(processing_bdata);
            }

            processing_bdata = BData::default();
        } else if let MLine::Goto(loc) = line {
            processing_bdata.goes_to.insert(*loc as usize);
        } else if let MLine::Branch { yes, no, .. } = line {
            processing_bdata.goes_to.insert(*yes as usize);
            processing_bdata.goes_to.insert(*no as usize);
        } else {
            processing_bdata.variables.extend(
                variables_in(line)
                    .into_iter()
                    .map(|VarInMIR { name, .. }| name)
                    .filter(|name| {
                        if let Some(loc) = fcs.var_locations.get(name) {
                            loc.1.arg_index == ArgIndex::None
                        } else {
                            false
                        }
                    })
            );
        }
    }

    let mut last_round_variable_dependency_total = 0;
    let mut this_round_variable_dependency_total = 1;
    while last_round_variable_dependency_total != this_round_variable_dependency_total {
        last_round_variable_dependency_total = this_round_variable_dependency_total;

        for block_index in 0..other_block_data.len() {
            for dependency in other_block_data[block_index].goes_to.clone() {
                let dep_vars = other_block_data[dependency].variables.clone();

                other_block_data[block_index].variables.extend(dep_vars);
            }
        }

        this_round_variable_dependency_total = 
            other_block_data.iter().map(|bl| bl.variables.len()).sum();
    }

    let mut entry_variables: Vec<_> = entry_block_data.variables.into_iter().collect();
    entry_variables.sort();

    fcs.entry_block = BlockData {
        block: fcs.builder.create_block(),
        variables_used: entry_variables
            .into_iter()
            .filter_map(|name| fcs.variables.get(&name).cloned())
            .collect(),
    };

    let mut other_block_variables: Vec<Vec<_>> = 
        other_block_data.into_iter().map(|bl| bl.variables.into_iter().collect()).collect();

    for variables in &mut other_block_variables {
        variables.sort()
    }

    fcs.other_blocks = other_block_variables
        .into_iter()
        .map(|variables| {
            BlockData {
                block: fcs.builder.create_block(),
                variables_used: variables
                    .into_iter()
                    .filter_map(|name| fcs.variables.get(&name).cloned())
                    .collect(),
            }
        })
        .collect();
}



fn compile_mline(fcs: &mut FunctionCompilationState, line: MLine) {
    match &line {
        MLine::Set { l, r } => {
            let r = compile_expr(fcs, &r, Some(l));
            fcs.registers.insert(*l, r.unwrap());
        },
        MLine::SetAddr { l, r } => {
            let r = compile_addr_expr(fcs, &r);
            fcs.addresses.insert(*l, r);
        },
        MLine::Store { l, val } => {
            let vals = compile_operand(fcs, val);

            match l {
                MAddr::Var(name) => fcs.variables[name].write(&mut fcs.builder, vals),
                MAddr::Reg(reg) => {
                    let addr = get_addr(fcs, &MAddr::Reg(*reg), 0);

                    write_vals_to_addr(&mut fcs.builder, addr, vals);
                }
            }
        },
        MLine::MemCpy { from, to, len } => {
            let from = get_addr(fcs, from, 0);
            let to = get_addr(fcs, to, 0);
            let len = one(compile_operand(fcs, len));

            fcs.builder.call_memcpy(
                *fcs.frontend_config,
                to,
                from,
                len,
            );
        },
        MLine::Return(operand) => {

            if let Some(operand) = operand {
                build_some_return(fcs, operand);
            } else {
                fcs.builder.ins().return_(&[]);
            }
        },
        MLine::Marker(index) => {
            let block = &fcs.other_blocks[*index as usize];
            fcs.builder.switch_to_block(block.block);
            
            for var in &block.variables_used {
                // make sure that cranelift knows we're using these variables, and load 
                // them in the right order, otherwise variables get mixed up
                var.mark_use(&mut fcs.builder);
            }
        },
        MLine::Goto(index) => {
            let params = fcs.block_params(*index as usize);
            fcs.builder.ins().jump(
                fcs.other_blocks[*index as usize].block, 
                &*params,
            );
        },
        MLine::Expr(expr) => {
            compile_expr(fcs, expr, None);
        },
        MLine::Branch { if_, yes, no } => {
            let if_ = one(compile_operand(fcs, if_));

            let yes_params = fcs.block_params(*yes as usize);
            let no_params = fcs.block_params(*no as usize);

            fcs.builder.ins().brif(
                if_, 
                fcs.other_blocks[*yes as usize].block, 
                &*yes_params,
                fcs.other_blocks[*no as usize].block,
                &*no_params,
            );
        },
    }
}

fn write_vals_to_addr(
    builder: &mut FunctionBuilder, 
    mut addr: CLValue, 
    vals: Vec<CLValue>,
) {
    let value_count = vals.len();

    for (v, val) in vals.into_iter().enumerate() {
        builder.ins().store(MemFlags::new(), val, addr, 0);
        
        if v != value_count - 1 {
            let typ = builder.ins().data_flow_graph().value_type(val);
            let offset_val = builder.ins().iconst(cl_types::I64, typ.bytes() as i64);

            addr = builder.ins().iadd(addr, offset_val);
        }
    }
}

fn build_some_return(mut fcs: &mut FunctionCompilationState, operand: &MOperand) {
    let return_style = fcs.ret_type.return_style();

    if return_style == ReturnStyle::Direct {
        let val = compile_operand(fcs, operand);
        fcs.builder.ins().return_(&*val);

        return;
    } 

    let mut get_val_at = match operand {
        MOperand::MemLoc(memloc) => {
            match memloc {
                MMemLoc::Reg(_) => todo!(),
                MMemLoc::Var(var_name) => {
                    let writeable = fcs.variables[&var_name].clone();

                    let fcs = &mut fcs;
                    move |typ: CLType, offset: u32| {
                        writeable.load_offset(&mut fcs.builder, typ, offset)
                    }
                },
            }
        },
        MOperand::Lit(_) => unreachable!(), // would be a direct return
    };

    let ret = match return_style {
        ReturnStyle::ThroughI32 => {
            vec![get_val_at(cl_types::I32, 0)]
        },
        ReturnStyle::ThroughI64 => {
            vec![get_val_at(cl_types::I64, 0)]
        },
        ReturnStyle::MoveIntoDouble => {
            vec![get_val_at(cl_types::F64, 0)]
        },
        ReturnStyle::ThroughI32I32 => {
            vec![get_val_at(cl_types::I32, 0), get_val_at(cl_types::I32, 4)]
        },
        ReturnStyle::ThroughF32F32 => {
            vec![get_val_at(cl_types::F32, 0), get_val_at(cl_types::F32, 4)]
        },
        ReturnStyle::ThroughI64I32 => {
            vec![get_val_at(cl_types::I64, 0), get_val_at(cl_types::I32, 8)]
        },
        ReturnStyle::ThroughI64I64 | ReturnStyle::MoveIntoI64I64 => {
            vec![get_val_at(cl_types::I64, 0), get_val_at(cl_types::I64, 8)]
        },
        _ => unreachable!(),
    };

    fcs.builder.ins().return_(&*ret);
}

fn compile_addr_expr(fcs: &mut FunctionCompilationState, r: &&MAddrExpr) -> CLValue {
    match r {
        MAddrExpr::Expr(expr) => compile_expr(fcs, expr, None).map(one).unwrap(),
        MAddrExpr::Addr(addr) => get_addr(fcs, addr, 0),
        MAddrExpr::Member { object_type, object, field_index } => {
            let TypeEnum::Struct(struct_type) = object_type.as_type_enum() else { panic!() };
            let field_byte_offset = struct_type.field_offset_in_bytes(*field_index as usize);
            get_addr(fcs, object, field_byte_offset as u32)
        },
        MAddrExpr::Index { array_type, object, index, .. } => {
            let TypeEnum::Array(array_type) = array_type.as_type_enum() else { panic!() };

            let index = one(compile_operand(fcs, index));

            let size = array_type.base.size();
            let size = fcs.builder.ins().iconst(cl_types::I64, size as i64);

            let offset = fcs.builder.ins().imul(size, index);
            let addr = get_addr(fcs, object, 0);
            let offset_addr = fcs.builder.ins().iadd(addr, offset);

            offset_addr
        },
    }
}

fn compile_expr(fcs: &mut FunctionCompilationState, expr: &MExpr, reg: Option<&MReg>) -> Option<Vec<CLValue>> {
    match expr {
        MExpr::BinOp { left_type, op, l, r } => {
            let l = one(compile_operand(fcs, l));
            let r = one(compile_operand(fcs, r));

            Some(vec![compile_binop(fcs, left_type, *op, l, r,)])
        },
        MExpr::Addr(addr) => {
            let addr = get_addr(fcs, addr, 0);
            let typ = fcs.reg_types[reg.unwrap()].to_cl_type()[0];
            let val = fcs.builder.ins().load(typ, MemFlags::new(), addr, 0);
            Some(vec![val])
        }
        MExpr::Call { f, a, typ, sret } => {
            let sret: Vec<_> = 
                sret.into_iter().map(|sret| one(load_memloc(fcs, sret))).collect();
            let a = a.into_iter().map(|a| compile_operand(fcs, a)).flatten();
            let all_args: Vec<CLValue> = sret.into_iter().chain(a).collect();

            let call_inst = match f {
                MCallable::Func(info) => {
                    if let Some(func_ref) = fcs.used_functions.get(&info) {
                        fcs.builder.ins().call(*func_ref, &*all_args)
                    } else if let Some(ext_func_data) = fcs.external_functions.get(&info) {
                        return Some(build_external_func_call(
                            &mut fcs.builder, 
                            all_args, 
                            ext_func_data
                        ));
                    } else {
                        return build_intrinsic_function(fcs, info, all_args);
                    }
                },
                MCallable::FirstClass(memloc) => {
                    let val = one(load_memloc(fcs, memloc));
                    let signature = fcs.function_signatures[&typ].clone();
                    let sigref = fcs.builder.func.stencil.import_signature(signature);
                    fcs.builder.ins().call_indirect(sigref, val, &*all_args)
                },
            };

            let ret = fcs.builder.inst_results(call_inst).to_vec();
            if ret.len() == 0 { None } else { Some(ret) }
        }
        MExpr::MemLoc(memloc) => Some(load_memloc(fcs, memloc)),
        MExpr::UnarOp { ret_type, op, hs } => {
            Some(vec![compile_unarop(fcs, ret_type, *op, hs)])
        }
        MExpr::Ref { on } => Some(vec![get_addr(fcs, on, 0)]),
        MExpr::Deref { to, on } => {
            let ptr = one(compile_operand(fcs, on));
            let typ = to.to_cl_type()[0];
            Some(vec![fcs.builder.ins().load(typ, MemFlags::new(), ptr, 0)])
        },
        MExpr::Void => None,
    }
}

fn build_intrinsic_function(
    fcs: &mut FunctionCompilationState, 
    info: &UniqueFuncInfo, 
    a: Vec<CLValue>
) -> Option<Vec<CLValue>> {
    match &*info.name.to_string() {
        "memmove" => {
            fcs.builder.call_memmove(
                fcs.frontend_config.clone(), 
                a[1],
                a[0],
                a[2],
            );
            None
        },
        "alloc" => {
            let alloc_info = UniqueFuncInfo::from(VarName::from("$alloc"));
            let ext_alloc = &fcs.external_functions[&alloc_info];
            let alloc_func = fcs.builder.ins().iconst(cl_types::I64, ext_alloc.ptr as i64);
            let alloc_sigref = fcs.builder.import_signature(ext_alloc.sig.clone());
            let size_multiplier = info.generics[0].size() as i64;
            let size_multiplier = fcs.builder.ins().iconst(cl_types::I64, size_multiplier);
            let proper_size = fcs.builder.ins().imul(a[0], size_multiplier);
            let call_inst = fcs.builder.ins().call_indirect(
                alloc_sigref, 
                alloc_func, 
                &[proper_size]
            );
            let ret = fcs.builder.inst_results(call_inst)[0];
            Some(vec![ret])
        },
        "free" => {
            let free_info = UniqueFuncInfo::from(VarName::from("$free"));
            let ext_free = &fcs.external_functions[&free_info];
            let free_func = fcs.builder.ins().iconst(cl_types::I64, ext_free.ptr as i64);
            let free_sigref = fcs.builder.import_signature(ext_free.sig.clone());

            fcs.builder.ins().call_indirect(free_sigref, free_func, &[a[0]]);
            None
        },
        "size_of" => {
            Some(vec![fcs.builder.ins().iconst(cl_types::I64, info.generics[0].size() as i64)])
        },
        "typeobj" => {
            let typ = info.generics[0].clone();
            let typusize: i64 = unsafe { std::mem::transmute(typ) };

            Some(vec![fcs.builder.ins().iconst(cl_types::I64,typusize)])
        },
        _ => panic!(),
    }
}

fn compile_unarop(
    fcs: &mut FunctionCompilationState, 
    ret_type: &Type, 
    op: Opcode, 
    hs: &MOperand, 
) -> CLValue {
    use Opcode::*;

    let hs = one(compile_operand(fcs, hs));

    match ret_type.as_type_enum() {
        TypeEnum::Bool(_) => {
            match op {
                Not => fcs.builder.ins().bnot(hs),
                _ => unreachable!(),
            }
        }
        _ => todo!("{:?}{:?}", op, ret_type),
    }
}

fn compile_binop(
    fcs: &mut FunctionCompilationState, 
    left_type: &Type, 
    op: Opcode, 
    l: CLValue, 
    r: CLValue,
) -> CLValue {
    use Opcode::*;

    let ins = fcs.builder.ins();

    match left_type.as_type_enum() {
        TypeEnum::Int(_) | TypeEnum::Bool(_) => {
            let signed = match left_type.as_type_enum() {
                TypeEnum::Int(IntType { signed, .. }) => *signed,
                TypeEnum::Bool(_) => false,
                _ => unreachable!()
            };

            match op {
                Plus => ins.iadd(l, r),
                Minus => ins.isub(l, r),
                Multiplier => ins.imul(l, r),
                Divider => ins.udiv(l, r),
                Modulus => ins.iadd(l, r),
                Equal => ins.icmp(IntCC::Equal, l, r),
                Inequal => ins.icmp(IntCC::NotEqual, l, r),
                GrtrThan if signed => ins.icmp(IntCC::SignedGreaterThan, l, r),
                GreaterOrEqual if signed => ins.icmp(IntCC::SignedGreaterThanOrEqual, l, r),
                LessThan if signed => ins.icmp(IntCC::SignedLessThan, l, r),
                LessOrEqual if signed => ins.icmp(IntCC::SignedLessThanOrEqual, l, r),
                GrtrThan if !signed => ins.icmp(IntCC::UnsignedGreaterThan, l, r),
                GreaterOrEqual if !signed => ins.icmp(IntCC::UnsignedGreaterThanOrEqual, l, r),
                LessThan if !signed => ins.icmp(IntCC::UnsignedLessThan, l, r),
                LessOrEqual if !signed => ins.icmp(IntCC::UnsignedLessThanOrEqual, l, r),
                BitShiftL => ins.ishl(l, r),
                BitShiftR => ins.ushr(l, r),
                BitAND => ins.band(l, r),
                BitOR => ins.bor(l, r),
                BitXOR => ins.bxor(l, r),
                _ => todo!(),
            }
        }
        TypeEnum::Float(_) => {
            match op {
                Plus => ins.fadd(l, r),
                Minus => ins.fsub(l, r),
                Multiplier => ins.fmul(l, r),
                Divider => ins.fdiv(l, r),
                Modulus => ins.fadd(l, r),
                Equal => ins.fcmp(FloatCC::Equal, l, r),
                Inequal => ins.fcmp(FloatCC::NotEqual, l, r),
                GrtrThan => ins.fcmp(FloatCC::GreaterThan, l, r),
                GreaterOrEqual => ins.fcmp(FloatCC::GreaterThanOrEqual, l, r),
                LessThan => ins.fcmp(FloatCC::LessThan, l, r),
                LessOrEqual => ins.fcmp(FloatCC::LessThanOrEqual, l, r),
                _ => todo!(),
            }
        },
        TypeEnum::Ref(RefType { base }) => {
            match op {
                Plus => {
                    let size = base.size();
                    let size = ins.iconst(cl_types::I64, size as i64);
                    let proper_add = fcs.builder.ins().imul(r, size);
                    fcs.builder.ins().iadd(l, proper_add)
                }
                _ => todo!(),
            }
        }
        _ => todo!("{left_type:?}"),
    }
}

fn compile_operand(fcs: &mut FunctionCompilationState, operand: &MOperand) -> Vec<CLValue> {
    match operand {
        MOperand::MemLoc(memloc) => load_memloc(fcs, memloc),
        MOperand::Lit(lit) => vec![compile_lit(fcs, lit)],
    }
}

fn compile_lit(fcs: &mut FunctionCompilationState, lit: &MLit) -> CLValue {
    match lit {
        MLit::Int { size, val } => {
            let int_type = Type::i(*size).to_cl_type()[0];
            fcs.builder.ins().iconst(int_type, *val as i64)
        }
        MLit::Float { size, val } => {
            match size {
                32 => fcs.builder.ins().f32const(*val as f32),
                64 => fcs.builder.ins().f64const(*val as f64),
                _ => todo!(),
            }
        },
        MLit::Bool(val) => {
            let bool_type = Type::bool().to_cl_type()[0];
            fcs.builder.ins().iconst(bool_type, *val as i64)
        },
    }
}

fn load_memloc(fcs: &mut FunctionCompilationState, memloc: &MMemLoc) -> Vec<CLValue> {
    match memloc {
        MMemLoc::Var(name) => {
            match fcs.variables.get(&name) {
                Some(var) => var.load(&mut fcs.builder),
                None => {
                    vec![fcs.used_globals[&name]]
                }
            }
        }
        MMemLoc::Reg(reg) => fcs.registers[&reg].clone(),
    }
}

fn get_addr(fcs: &mut FunctionCompilationState, addr: &MAddr, offset: u32) -> CLValue {
    match addr {
        MAddr::Var(name) => fcs.variables[&name].addr(&mut fcs.builder, offset),
        MAddr::Reg(reg) => {
            let addr = fcs.addresses[&reg];

            if offset != 0 {
                let offset = fcs.builder.ins().iconst(cl_types::I64, offset as i64);
                fcs.builder.ins().iadd(addr, offset)
            } else {
                addr
            }
        }
    }
}

fn one<T: std::fmt::Debug>(mut vec: Vec<T>) -> T {
    assert_eq!(vec.len(), 1, "{:?}", vec);
    vec.remove(0)
}
