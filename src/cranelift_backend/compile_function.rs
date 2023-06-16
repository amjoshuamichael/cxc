use std::{collections::{BTreeMap, BTreeSet}, iter::once};

use cranelift::{codegen::{Context, ir::{StackSlot, FuncRef, SigRef, Inst}}, prelude::{Variable, FunctionBuilder, Block, FunctionBuilderContext, Value as CLValue, InstBuilder, IntCC, MemFlags, StackSlotData, StackSlotKind, types as cl_types, FloatCC, Signature, isa::TargetFrontendConfig, Type as CLType}};

use crate::{mir::{MIR, MLine, MExpr, MMemLoc, MOperand, MLit, MAddr, MAddrExpr, MAddrReg, MReg, MCallable}, VarName, Type, parse::Opcode, TypeEnum, UniqueFuncInfo, FuncType, typ::ReturnStyle};

use super::{to_cl_type::ToCLType, variables_in_mline::variables_in};

struct FunctionCompilationState<'a> {
    builder: FunctionBuilder<'a>,
    addresses: BTreeMap<MAddrReg, CLValue>,
    registers: BTreeMap<MReg, CLValue>,
    variables: BTreeMap<VarName, Writeable>,
    entry_block: BlockData,
    other_blocks: Vec<BlockData>,
    reg_types: BTreeMap<MReg, Type>,
    // TODO: store a map of FuncIds instead, and create the refs while compiling
    used_functions: BTreeMap<UniqueFuncInfo, FuncRef>,
    used_globals: BTreeMap<VarName, CLValue>,
    function_signatures: &'a BTreeMap<FuncType, Signature>,
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
            .collect()
    }
}

#[derive(Copy, Clone, Debug)]
enum Writeable {
    Var(Variable),
    Slot(StackSlot),
}

impl Writeable {
    fn write(&self, builder: &mut FunctionBuilder, val: CLValue) {
        match self {
            Writeable::Var(var) => {
                builder.def_var(*var, val)
            },
            Writeable::Slot(slot) => {
                let slot = builder.ins().stack_addr(cl_types::I64, *slot, 0);
                builder.ins().store(MemFlags::new(), val, slot, 0);
            },
        }
    }

    fn load(&self, builder: &mut FunctionBuilder) -> CLValue {
        match self {
            Writeable::Var(var) => builder.use_var(*var),
            Writeable::Slot(slot) => {
                builder.ins().stack_load(cl_types::I64, *slot, 0)
            }
        }
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
            Writeable::Slot(slot) => {
                builder.ins().stack_load(typ, *slot, offset as i32)
            }
        }
    }

    fn addr(&self, builder: &mut FunctionBuilder, offset: u32) -> CLValue {
        match self {
            Writeable::Var(_) => panic!(),
            Writeable::Slot(slot) => {
                builder.ins().stack_addr(cl_types::I64, *slot, offset as i32)
            }
        }
    }

    fn is_stack_slot(&self) -> bool { matches!(self, Writeable::Slot(_)) }
}

fn make_stack_allocas(fcs: &mut FunctionCompilationState, mir: &MIR) {
    for (id, (name, info)) in mir.variables.iter().enumerate() {
        let var = Variable::from_u32(id as u32);
        let var_type = info.typ.to_cl_type();

        if info.typ.is_primitive() {
            assert_eq!(var_type.len(), 1);
            let var_type = var_type[0];

            if let Some(arg_index) = info.arg_index {
                let arg_index = arg_index as usize;
                let val = fcs.builder.block_params(fcs.entry_block.block)[arg_index];
                fcs.builder.declare_var(var, var_type);
                fcs.builder.def_var(var, val);
            } else {
                fcs.builder.declare_var(var, var_type);
            }

            fcs.variables.insert(name.clone(), Writeable::Var(var));
        } else {
            if let Some(_arg_index) = info.arg_index {
                todo!()
            } else {
                let slot = fcs.builder.create_sized_stack_slot(StackSlotData {
                    kind: StackSlotKind::ExplicitSlot,
                    size: info.typ.size() as u32,
                });

                fcs.variables.insert(name.clone(), Writeable::Slot(slot));
            }
        }
    }
}

#[derive(Debug)]
struct BlockData {
    block: Block,
    variables_used: Vec<Writeable>,
}

fn make_blocks(fcs: &mut FunctionCompilationState, mir: &MIR) {
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

    for line in mir.lines.iter().chain(once(&MLine::Marker(0))) {
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
            processing_bdata.variables.extend(variables_in(line));
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
            .filter_map(|name| fcs.variables.get(&name).copied())
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
                    .filter_map(|name| fcs.variables.get(&name).copied())
                    .collect(),
            }
        })
        .collect();
}

pub fn compile(
    mir: MIR, 
    entry_block: Block,
    builder: FunctionBuilder,
    used_functions: BTreeMap<UniqueFuncInfo, FuncRef>,
    used_globals: BTreeMap<VarName, CLValue>,
    function_signatures: &BTreeMap<FuncType, Signature>,
    frontend_config: &TargetFrontendConfig,
) {
    let mut fcs = FunctionCompilationState {
        builder,
        entry_block: BlockData {
            block: entry_block,
            variables_used: Vec::new(),
        },
        other_blocks: Vec::new(),
        addresses: BTreeMap::new(),
        variables: BTreeMap::new(),
        registers: BTreeMap::new(),
        reg_types: BTreeMap::new(),
        used_functions,
        used_globals,
        function_signatures,
        frontend_config,
        ret_type: mir.func_type.ret.clone(),
    };

    make_stack_allocas(&mut fcs, &mir);
    make_blocks(&mut fcs, &mir);

    fcs.reg_types = mir.reg_types;

    #[cfg(feature = "xc-debug")]
    let mut l = 0;

    for line in mir.lines {
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

fn compile_mline(fcs: &mut FunctionCompilationState, line: MLine) {
    match &line {
        MLine::Set { l, r } => {
            match r {
                // Arrays need a special case because cranelift does not directly support them
                MExpr::Array { elem_type, elems } => {
                    for (e, elem) in elems.iter().enumerate() {
                        
                    }
                },
                _ => {
                    let r = compile_expr(fcs, &r, Some(l));
                    fcs.registers.insert(*l, r.unwrap());
                }
            }
        },
        MLine::SetAddr { l, r } => {
            let r = compile_addr_expr(fcs, &r, Some(l));
            fcs.addresses.insert(*l, r);
        },
        MLine::Store { l, val } => {
            let val = compile_operand(fcs, val);

            match l {
                MAddr::Var(name) => fcs.variables[name].write(&mut fcs.builder, val),
                MAddr::Reg(reg) => {
                    let addr = get_addr(fcs, &MAddr::Reg(*reg), 0);
                    fcs.builder.ins().store(MemFlags::new(), val, addr, 0);
                }
            }
        },
        MLine::MemCpy { from, to, len } => {
            let from = get_addr(fcs, from, 0);
            let to = get_addr(fcs, to, 0);
            let len = compile_operand(fcs, len);
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
                var.load(&mut fcs.builder);
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
            let if_ = compile_operand(fcs, if_);

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

fn build_some_return(mut fcs: &mut FunctionCompilationState, operand: &MOperand) {
    let return_style = fcs.ret_type.return_style();

    if return_style == ReturnStyle::Direct {
        let val = compile_operand(fcs, operand);
        fcs.builder.ins().return_(&[val]);

        return;
    } 

    let mut get_val_at = match operand {
        MOperand::MemLoc(memloc) => {
            match memloc {
                MMemLoc::Reg(_) => todo!(),
                MMemLoc::Var(var_name) => {
                    let writeable = fcs.variables[&var_name];

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

fn compile_addr_expr(fcs: &mut FunctionCompilationState, r: &&MAddrExpr, l: Option<&MAddrReg>) -> CLValue {
    match r {
        MAddrExpr::Expr(expr) => compile_expr(fcs, expr, None).unwrap(),
        MAddrExpr::Addr(addr) => get_addr(fcs, addr, 0),
        MAddrExpr::Member { object_type, object, field_index } => {
            let TypeEnum::Struct(struct_type) = object_type.as_type_enum() else { panic!() };
            let field_byte_offset = struct_type.field_offset_in_bytes(*field_index as usize);
            get_addr(fcs, object, field_byte_offset as u32)
        },
        MAddrExpr::Index { array_type, element_type, object, index } => todo!(),
    }
}

fn compile_expr(fcs: &mut FunctionCompilationState, expr: &MExpr, reg: Option<&MReg>) -> Option<CLValue> {
    match expr {
        MExpr::BinOp { ret_type, op, l, r } => {
            let l = compile_operand(fcs, l);
            let r = compile_operand(fcs, r);

            Some(compile_binop(fcs, ret_type, *op, l, r,))
        },
        MExpr::Addr(addr) => {
            let addr = get_addr(fcs, addr, 0);
            let typ = fcs.reg_types[reg.unwrap()].to_cl_type()[0];
            let val = fcs.builder.ins().load(typ, MemFlags::new(), addr, 0);
            Some(val)
        }
        MExpr::Call { f, a, typ } => {
            let a: Vec<_> = a.into_iter().map(|a| compile_operand(fcs, a)).collect();

            let call_inst = match f {
                MCallable::Func(info) => {
                    if let Some(func_ref) = fcs.used_functions.get(&info) {
                        fcs.builder.ins().call(*func_ref, &*a)
                    } else {
                        return build_intrinsic_function(fcs, info, a);
                    }
                },
                MCallable::FirstClass(memloc) => {
                    let val = load_memloc(fcs, memloc);
                    let signature = fcs.function_signatures[&typ].clone();
                    let sigref = fcs.builder.func.stencil.import_signature(signature);
                    fcs.builder.ins().call_indirect(sigref, val, &*a)
                },
            };

            if let Some(ret) = fcs.builder.inst_results(call_inst).get(0) {
                Some(*ret)
            } else {
                None
            }
        }
        MExpr::MemLoc(memloc) => Some(load_memloc(fcs, memloc)),
        MExpr::UnarOp { ret_type, op, hs } => todo!(),
        MExpr::Array { elem_type, elems } => todo!(),
        MExpr::Ref { on } => Some(get_addr(fcs, on, 0)),
        MExpr::Deref { to, on } => {
            let ptr = compile_operand(fcs, on);
            let typ = to.to_cl_type()[0];
            Some(fcs.builder.ins().load(typ, MemFlags::new(), ptr, 0))
        },
        MExpr::Void => None,
    }
}

fn build_intrinsic_function(
    fcs: &mut FunctionCompilationState, 
    info: &UniqueFuncInfo, 
    a: Vec<CLValue>
) -> Option<CLValue> {
    match &*info.name.to_string() {
        "memcpy" => {
            fcs.builder.call_memcpy(
                fcs.frontend_config.clone(), 
                a[1],
                a[0],
                a[2],
            );
            None
        },
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
            let func_ref = fcs.used_functions[&alloc_info];
            let size_multiplier = info.generics[0].size() as i64;
            let size_multiplier = fcs.builder.ins().iconst(cl_types::I32, size_multiplier);
            let proper_size = fcs.builder.ins().imul(a[0], size_multiplier);
            let call_inst = fcs.builder.ins().call(func_ref, &[proper_size]);
            let ret = fcs.builder.inst_results(call_inst)[0];
            Some(ret)
        },
        "free" => {
            let free_info = UniqueFuncInfo::from(VarName::from("$free"));
            let func_ref = fcs.used_functions[&free_info];
            let free_size = info.generics[0].size() as i64;
            let free_size = fcs.builder.ins().iconst(cl_types::I32, free_size);
            fcs.builder.ins().call(func_ref, &[a[0], free_size]);
            None
        },
        "size_of" => {
            Some(fcs.builder.ins().iconst(cl_types::I64, info.generics[0].size() as i64))
        },
        "cast" => {
            todo!()
        },
        "typeobj" => {
            let typ = info.generics[0].clone();
            let typusize: i64 = unsafe { std::mem::transmute(typ) };

            Some(fcs.builder.ins().iconst(cl_types::I64,typusize))
        },
        _ => panic!(),
    }
}

fn compile_binop(
    fcs: &mut FunctionCompilationState, 
    ret_type: &Type, 
    op: Opcode, 
    l: CLValue, 
    r: CLValue,
) -> CLValue {
    use Opcode::*;

    let ins = fcs.builder.ins();

    match ret_type.as_type_enum() {
        TypeEnum::Int(_) => {
            match op {
                Plus => ins.iadd(l, r),
                Minus => ins.isub(l, r),
                Multiplier => ins.imul(l, r),
                Divider => ins.udiv(l, r),
                Modulus => ins.iadd(l, r),
                Equal => ins.icmp(IntCC::Equal, l, r),
                Inequal => ins.icmp(IntCC::NotEqual, l, r),
                GrtrThan => ins.icmp(IntCC::SignedGreaterThan, l, r),
                GreaterOrEqual => ins.icmp(IntCC::SignedGreaterThanOrEqual, l, r),
                LessThan => ins.icmp(IntCC::SignedLessThan, l, r),
                LessOrEqual => ins.icmp(IntCC::SignedLessThanOrEqual, l, r),
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
        TypeEnum::Ref(_) => {
            match op {
                Plus => ins.iadd(l, r),
                _ => todo!(),
            }
        }
        _ => todo!("{ret_type:?}"),
    }
}

fn compile_operand(fcs: &mut FunctionCompilationState, operand: &MOperand) -> CLValue {
    match operand {
        MOperand::MemLoc(memloc) => load_memloc(fcs, memloc),
        MOperand::Lit(lit) => compile_lit(fcs, lit),
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

fn load_memloc(fcs: &mut FunctionCompilationState, memloc: &MMemLoc) -> CLValue {
    match memloc {
        MMemLoc::Var(name) => {
            match fcs.variables.get(&name) {
                Some(var) => var.load(&mut fcs.builder),
                None => {
                    fcs.used_globals[&name]
                }
            }
        }
        MMemLoc::Reg(reg) => fcs.registers[&reg],
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
