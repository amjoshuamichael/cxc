use std::{collections::{BTreeMap, BTreeSet, HashMap}, iter::once};

use cranelift::{codegen::{ir::{StackSlot, FuncRef, InstBuilderBase}, self}, prelude::{Variable, FunctionBuilder, Block, FunctionBuilderContext, Value as CLValue, InstBuilder, IntCC, MemFlags, StackSlotData, StackSlotKind, types as cl_types, FloatCC, Signature, isa::TargetFrontendConfig, Type as CLType}};
use cranelift_module::Module;
use slotmap::SecondaryMap;

use crate::{mir::{MIR, MLine, MExpr, MMemLoc, MOperand, MAddr, MAddrExpr, MAddrReg, MReg, MCallable}, VarName, Type, parse::Opcode, TypeEnum, FuncType, typ::{ReturnStyle, ABI}, hlr::hlr_data::{ArgIndex, VariableInfo, VarID}, cranelift_backend::variables_in, RefType, IntType, unit::FuncId};

use super::{to_cl_type::{ToCLType, func_type_to_signature}, CraneliftBackend, external_function::ExternalFuncData, ClFunctionData, alloc_and_free::AllocAndFree};


#[derive(Debug, PartialEq, Eq, Clone, Copy)]
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
    let used_functions = mir.dependencies.iter().filter_map(|(_, func_id)| {
        let (cl_func_id, func_id) = 
            if let Some(ClFunctionData { cl_func_id, .. }) = 
                backend.cl_function_data.get(*func_id) {
                (*cl_func_id, *func_id)
            } else {
                return None
            };

        let func_ref = backend.module.declare_func_in_func(cl_func_id, &mut context.func);

        Some((func_id, func_ref))
    }).collect();

    let mut builder = FunctionBuilder::new(&mut context.func, func_builder_context);

    let entry_block = builder.create_block();
    // get function paramaters into the entry block
    builder.append_block_params_for_function_params(entry_block);
    builder.switch_to_block(entry_block);
    // tell cranelift this block will have no predecessors, because it is the entry
    builder.seal_block(entry_block);

    let mut used_globals = BTreeMap::<VarName, CLValue>::new();
    let mut var_locations = SecondaryMap::<VarID, (VarLocation, VariableInfo)>::new();

    let vars_in_mir = variables_in::variables_in(&mir);

    for var in vars_in_mir.stack_vars {
        var_locations.insert(var, (VarLocation::Stack, mir.variables[var].clone()));
    }

    for var in vars_in_mir.register_vars {
        var_locations.insert(var, (VarLocation::Reg, mir.variables[var].clone()));
    }

    for global in vars_in_mir.globals {
        let ptr = &backend.globals[&global];
        used_globals.insert(global, builder.ins().iconst(cl_types::I64, *ptr as i64));
    }

    for line in &mir.lines {
        let call_type = match line {
            MLine::Set { r: MExpr::Call { typ, .. }, .. } => typ,
            MLine::Expr(MExpr::Call { typ, .. }) => typ,
            _ => continue,
        };

        let mut call_sig = backend.module.make_signature();
        func_type_to_signature(call_type, &mut call_sig);

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
        variables: SecondaryMap::new(),
        registers: BTreeMap::new(),
        reg_types: mir.reg_types,
        used_functions,
        used_globals,
        var_locations,
        function_signatures: &backend.func_signatures,
        external_functions: &backend.external_functions,
        frontend_config: &backend.frontend_config,
        alloc_and_free: &backend.alloc_and_free,
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

    #[cfg(feature = "mir-debug")]
    let mut l = 0;

    for line in lines {
        compile_mline(&mut fcs, line);

        #[cfg(feature = "mir-debug")]
        {
            print!("{:03}, ", l);
            l += 1;
        }
    }

    #[cfg(feature = "mir-debug")]
    println!();
}

pub struct FunctionCompilationState<'a> {
    builder: FunctionBuilder<'a>,
    addresses: BTreeMap<MAddrReg, CLValue>,
    // functions can return a struct with a size of over 8 bytes. for example, the 
    // struct {i64, i32}. in llvm, we return this struct directly as it is, but in 
    // cranelift, we *actually* just return multiple values from the function. that's why 
    // we use a vec of CLValues here.
    registers: BTreeMap<MReg, Vec<CLValue>>,
    variables: SecondaryMap<VarID, Writeable>,
    var_locations: SecondaryMap<VarID, (VarLocation, VariableInfo)>,
    entry_block: BlockData,
    other_blocks: Vec<BlockData>,
    reg_types: BTreeMap<MReg, Type>,
    used_functions: SecondaryMap<FuncId, FuncRef>,
    used_globals: BTreeMap<VarName, CLValue>,
    function_signatures: &'a HashMap<FuncType, Signature>,
    external_functions: &'a SecondaryMap<FuncId, ExternalFuncData>,
    frontend_config: &'a TargetFrontendConfig,
    alloc_and_free: &'a AllocAndFree,
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

                for (t, typ) in types.iter().enumerate() {
                    let val = self.load_offset(builder, *typ, byte_offset);
                    values.push(val);

                    if t < types.len() - 1 {
                        byte_offset += typ.bytes().next_multiple_of(types[t + 1].bytes());
                    }
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

    let mut var_locations: Vec<_> = fcs.var_locations.iter().collect();
    var_locations.sort_by(|a, b| a.1.1.arg_index.cmp(&b.1.1.arg_index));

    for (index, (name, (location, VariableInfo { typ, arg_index, .. }))) 
        in var_locations.into_iter().enumerate() {
        let var = Variable::from_u32(index as u32);
        let var_types = typ.to_cl_type();

        if *location == VarLocation::Reg {
            let writeable = if let ArgIndex::Some(arg_index) = arg_index {
                let raw_arg_types = typ.raw_arg_type(ABI::C).to_cl_type();
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
            assert!(*arg_index == ArgIndex::None);

            let slot = fcs.builder.create_sized_stack_slot(StackSlotData {
                kind: StackSlotKind::ExplicitSlot,
                size: typ.size() as u32,
            });

            fcs.variables.insert(name.clone(), Writeable::Slot(slot, typ.to_cl_type()));
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
        variables: Set<VarID>,
        goes_to: Set<usize>,
    }

    let mut other_block_data = Vec::<BData>::new();
    let mut entry_block_data = BData::default();

    {
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
                    variables_in::variables_in_line(line)
                        .into_iter()
                        .filter(|id| {
                            if let Some(loc) = fcs.var_locations.get(*id) {
                                loc.1.arg_index == ArgIndex::None
                            } else {
                                false
                            }
                        })
                );
            }
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
            .filter_map(|name| fcs.variables.get(name).cloned())
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
                    .filter_map(|name| fcs.variables.get(name).cloned())
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
                MAddr::Var(name) => fcs.variables[*name].write(&mut fcs.builder, vals),
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
        MLine::MemMove { from, to, len } => {
            let from = one(compile_operand(fcs, from));
            let to = one(compile_operand(fcs, to));
            let len = one(compile_operand(fcs, len));

            fcs.builder.call_memmove(
                *fcs.frontend_config,
                to,
                from,
                len,
            );
        },
        MLine::Return(operand) => {
            if let Some(operand) = operand {
                let val = compile_operand(fcs, operand);
                fcs.builder.ins().return_(&*val);
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

    let types = vals
        .iter()
        .map(|val| builder.ins().data_flow_graph().value_type(*val))
        .collect::<Vec<_>>();

    for (v, val) in vals.into_iter().enumerate() {
        builder.ins().store(MemFlags::new(), val, addr, 0);
        
        if v != value_count - 1 {
            let offset = types[v].bytes().next_multiple_of(types[v + 1].bytes());
            let offset_val = builder.ins().iconst(cl_types::I64, offset as i64);

            addr = builder.ins().iadd(addr, offset_val);
        }
    }
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
                MCallable::Func(id) => {
                    if let Some(func_ref) = fcs.used_functions.get(*id) {
                        fcs.builder.ins().call(*func_ref, &*all_args)
                    } else if let Some(ExternalFuncData { ptr, sig, .. }) = 
                        fcs.external_functions.get(*id) {
                        let sigref = fcs.builder.func.import_signature(sig.clone());
                        let func_val = fcs.builder.ins().iconst(cl_types::I64, *ptr as i64);

                        fcs.builder.ins().call_indirect(sigref, func_val, &*all_args)
                    } else {
                        panic!("{id:?}");
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
        },
        MExpr::Alloc { len } => {
            let alloc_func = 
                fcs.builder.ins().iconst(cl_types::I64, fcs.alloc_and_free.alloc_ptr as i64);
            let size = one(compile_operand(fcs, len));
            let alloc_sigref = 
                fcs.builder.import_signature(fcs.alloc_and_free.alloc_sig.clone());
            let call_inst = fcs.builder.ins().call_indirect(
                alloc_sigref,
                alloc_func, 
                &[size],
            );
            Some(fcs.builder.inst_results(call_inst).to_vec())
        },
        MExpr::Free { ptr } => {
            let alloc_func = 
                fcs.builder.ins().iconst(cl_types::I64, fcs.alloc_and_free.free_ptr as i64);
            let ptr = one(compile_operand(fcs, ptr));
            let free_sigref = 
                fcs.builder.import_signature(fcs.alloc_and_free.alloc_sig.clone());
            let call_inst = fcs.builder.ins().call_indirect(
                free_sigref,
                alloc_func, 
                &[ptr],
            );
            let ret = fcs.builder.inst_results(call_inst)[0];
            Some(vec![ret])
        },
        MExpr::MemLoc(memloc) => Some(load_memloc(fcs, memloc)),
        MExpr::UnarOp { ret_type, op, hs } => {
            Some(vec![compile_unarop(fcs, ret_type, *op, hs)])
        }
        MExpr::Ref { on } => Some(vec![get_addr(fcs, on, 0)]),
        MExpr::Deref { to, on } => {
            let ptr = one(load_memloc(fcs, on));
            let typ = one(to.to_cl_type());
            Some(vec![fcs.builder.ins().load(typ, MemFlags::new(), ptr, 0)])
        },
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
        TypeEnum::Bool => {
            match op {
                Not => {
                    let true_ = fcs.builder.ins().iconst(cl_types::I8, 1);
                    fcs.builder.ins().bxor(hs, true_)
                }
                _ => unreachable!(),
            }
        }
        TypeEnum::Int(_) => {
            match op {
                Negate => fcs.builder.ins().ineg(hs),
                _ => unreachable!(),
            }
        }
        _ => todo!("{:?} on {:?}", op, ret_type),
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
        TypeEnum::Int(_) | TypeEnum::Bool => {
            let signed = match left_type.as_type_enum() {
                TypeEnum::Int(IntType { signed, .. }) => *signed,
                TypeEnum::Bool => false,
                _ => unreachable!()
            };

            match op {
                Plus => ins.iadd(l, r),
                Minus => ins.isub(l, r),
                Multiplier => ins.imul(l, r),
                Divider => ins.udiv(l, r),
                Modulus => ins.urem(l, r),
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
                And => ins.band(l, r),
                Or => ins.bor(l, r),
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
        TypeEnum::Ref(RefType { .. }) => {
            match op {
                Inequal => ins.icmp(IntCC::NotEqual, l, r ),
                _ => todo!("{op:?}"),
            }
        }
        _ => todo!("{left_type:?}"),
    }
}

fn compile_operand(fcs: &mut FunctionCompilationState, operand: &MOperand) -> Vec<CLValue> {
    match operand {
        MOperand::Memloc(memloc) => load_memloc(fcs, memloc),
        MOperand::Int { size, val } => {
            let int_type = Type::i(*size).to_cl_type()[0];
            vec![fcs.builder.ins().iconst(int_type, *val as i64)]
        }
        MOperand::Float { size, val } => {
            vec![
                match size {
                    32 => fcs.builder.ins().f32const(*val as f32),
                    64 => fcs.builder.ins().f64const(*val as f64),
                    _ => todo!(),
                }
            ]
        },
        MOperand::Bool(val) => {
            let bool_type = Type::bool().to_cl_type()[0];
            vec![fcs.builder.ins().iconst(bool_type, *val as i64)]
        },
        MOperand::Function(func_id) => {
            let func_ref = fcs.used_functions[*func_id];
            vec![fcs.builder.ins().func_addr(cl_types::I64, func_ref)]
        },
    }
}

fn load_memloc(fcs: &mut FunctionCompilationState, memloc: &MMemLoc) -> Vec<CLValue> {
    match memloc {
        MMemLoc::Var(id) => fcs.variables[*id].load(&mut fcs.builder),
        MMemLoc::Reg(reg) => fcs.registers[&reg].clone(),
        MMemLoc::Global(global) => vec![fcs.used_globals[global]],
    }
}

fn get_addr(fcs: &mut FunctionCompilationState, addr: &MAddr, offset: u32) -> CLValue {
    match addr {
        MAddr::Var(name) => fcs.variables[*name].addr(&mut fcs.builder, offset),
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
