use std::{slice, collections::{HashMap, BTreeMap}};

use slotmap::{SecondaryMap, SlotMap, Key, KeyData};

use crate::{unit::FuncId, Value, mir::{MLine, MOperand, MAddr, MMemLoc, MReg, MExpr, MIR, MCallable, MAddrExpr, MAddrReg}, hlr::hlr_data::{VarID, ArgIndex, VariableInfo}, Type, parse::Opcode, IntType, FloatType, TypeEnum, typ::{ReturnStyle, IntSize}, FuncType, VarName};

use super::IntepreterState;

#[derive(Default)]
struct Allocator {
    allocations: BTreeMap<*const u8, Allocation>,
    stacks: Vec<Stack>,
    freed: Vec<*const u8>,
}

#[derive(Debug)]
struct Stack {
    data: Box<[u8]>,
    var_locations_and_size: SecondaryMap<VarID, (usize, usize)>,
}

impl Stack {
    fn for_vars(variables: &SlotMap<VarID, VariableInfo>) -> Self {
        let mut var_locations_and_size = SecondaryMap::<VarID, (usize, usize)>::new();
        let mut size = 0;

        for (var_id, VariableInfo { typ, arg_index, .. }) in variables {
            // args and srets are stored in the arg_regs and sret_reg fields of the 
            // interpreter, not in the stack.
            if arg_index != &ArgIndex::None { continue }

            var_locations_and_size.insert(var_id, (size, typ.size()));
            size += typ.size();
        }

        Stack {
            data: vec![0u8; size].into_boxed_slice(),
            var_locations_and_size,
        }
    }
}

#[derive(Debug)]
struct Allocation {
    data: Box<[u8]>,
    source: Option<(FuncId, usize)>,
}

struct Interpreter<'a> {
    registers: BTreeMap<MReg, RegValue>,
    addr_registers: BTreeMap<MAddrReg, AddrRegValue>,
    arg_regs: Vec<RegValue>,
    sret_reg: Option<RegValue>,
    func_id: FuncId,
    current_line: usize,
    force_safety: bool,
    allocator: &'a mut Allocator,
    state: &'a IntepreterState,
    mir: &'a MIR,
}

impl<'a> Interpreter<'a> {
    pub fn print_stack(&self) {
        let last_stack = self.allocator.stacks.last().unwrap();
        let mut values = Vec::<(VarID, Value)>::new();

        for (var_id, (location, size)) in &last_stack.var_locations_and_size {
            let var_type = self.mir.variables[var_id].typ.clone();
            
            values.push((var_id, Value::new_from_slice(
                var_type, 
                &last_stack.data[(*location)..(*location + *size)]
            )));
        }

        for var_id in self.mir.variables.keys() {
            let ArgIndex::Some(a) = self.mir.variables[var_id].arg_index
                else { continue };

            values.push((var_id, Value::new_from_slice(
                self.mir.variables[var_id].typ.clone(), 
                &self.arg_regs[a]
            )));
        }

        for (var_id, value) in values {
            if let Some(serialized_primitive) = value.to_string_no_unit() {
                let var_name = self.mir.variables[var_id].name.clone();
                let var_name_string = if var_name == VarName::None {
                    format!("{var_id:?}")
                } else {
                    var_name.to_string() + &*format!("||{var_id:?}")
                };    

                print!("{var_name_string} = {serialized_primitive}, ");
            }
        }
        println!();
    }
}

type RegValue = Box<[u8]>;
type AddrRegValue = usize;

pub(crate) fn run(func_id: FuncId, state: &IntepreterState, force_safety: bool) -> Value {
    run_with_args(func_id, state, Vec::new(), force_safety)
}

pub(crate) fn run_with_args(
    func_id: FuncId, 
    state: &IntepreterState, 
    args: Vec<Box<[u8]>>, 
    force_safety: bool,
) -> Value {
    // TODO: check arg count & type
    let mir = &state.mirs[func_id];
    let stack = Stack::for_vars(&mir.variables);

    let mut allocator = Allocator {
        allocations: BTreeMap::new(),
        stacks: vec![stack],
        freed: Vec::new(),
    };

    let return_style = mir.func_type.ret.return_style(mir.func_type.abi);
    let sret_allocation = (return_style == ReturnStyle::SRet).then(|| {
        let new_allocation = vec![0u8; mir.func_type.ret.size()].into_boxed_slice();
        let alloc_pointer = new_allocation.as_ptr() as usize;

        allocator.allocations.insert(new_allocation.as_ptr(), Allocation {
            data: new_allocation,
            source: None,
        });

        alloc_pointer
    });

    let mut interpreter = Interpreter {
        registers: BTreeMap::new(),
        addr_registers: BTreeMap::new(),
        arg_regs: args,
        sret_reg: sret_allocation.map(|ptr| ptr.to_ne_bytes().into()),
        current_line: 0,
        func_id,
        force_safety,
        allocator: &mut allocator,
        state,
        mir,
    };

    let output = interpret(&mut interpreter);

    let data = if let Some(sret_allocation) = sret_allocation {
        allocator.allocations.remove(&(sret_allocation as *const u8)).unwrap().data
    } else {
        output.data[0..mir.func_type.ret.size()].into()
    };

    if !allocator.allocations.is_empty() {
        let return_type = &mir.func_type.ret;

        panic!("allocations remain after interpretation!");
    }

    Value {
        typ: mir.func_type.ret.clone(),
        data,
    }
}

fn interpret(interpreter: &mut Interpreter) -> Value {
    let mut processed = 0;

    loop {
        #[cfg(feature = "backend-debug")]
        {
            if let Some(expr_id) = interpreter.mir.dbg_map.get(&interpreter.current_line) {
                let line_as_string = interpreter
                    .mir
                    .from_tree
                    .get(*expr_id)
                    .to_string(&interpreter.mir.from_tree, &interpreter.mir.variables);

                println!("{line_as_string}");

                interpreter.print_stack();
            }

            if interpreter.current_line == 0 {
                interpreter.print_stack();
            }

            println!("{:?}", &interpreter.mir.lines[interpreter.current_line]);
        }



        match &interpreter.mir.lines[interpreter.current_line] {
            MLine::Set { l, r } => {
                let r = iexpr(Some(l), r, interpreter);
                interpreter.registers.insert(*l, r);
            },
            MLine::SetAddr { l, r } => {
                let r = iaddrexpr(r, interpreter);
                interpreter.addr_registers.insert(*l, r);
            },
            MLine::Store { l, val } => {
                let val = ioperand(val, interpreter);
                store_into_addr(l, val, interpreter);
            },
            MLine::MemCpy { from, to, len } => {
                let from = iaddr(from, interpreter); 
                let to = iaddr(to, interpreter); 
                let len = slice_to_usize(&*ioperand(len, interpreter));

                let from_slice = load_addr_with_len(from, interpreter, len);
                let to_slice = addr_to_slice(to, interpreter);
                to_slice[0..len].copy_from_slice(&from_slice[0..len]);
            },
            MLine::MemMove { from, to, len } => {
                let from = slice_to_usize(&*ioperand(from, interpreter)); 
                let to = slice_to_usize(&*ioperand(to, interpreter)); 
                let len = slice_to_usize(&*ioperand(len, interpreter));

                let from_slice = load_addr_with_len(from, interpreter, len);
                let to_slice = addr_to_slice(to, interpreter);
                to_slice[0..len].copy_from_slice(&from_slice[0..len]);
            },
            MLine::Return(operand) => {
                if let Some(operand) = operand {
                    let operand = ioperand(operand, interpreter);
                    let func_type = &interpreter.mir.func_type;

                    return Value {
                        typ: func_type.ret.raw_return_type(func_type.abi).clone(),
                        data: operand,
                    };
                } else {
                    return Value::void();
                }
            },
            MLine::Marker(_) => todo!(),
            MLine::Goto(new_line) => {
                interpreter.current_line = interpreter
                    .mir
                    .lines
                    .iter()
                    .position(|marker| matches!(marker, MLine::Marker(line) if line == new_line))
                    .unwrap();
            },
            MLine::Expr(expr) => {
                iexpr(None, expr, interpreter);
            },
            MLine::Branch { if_, yes, no } => {
                let cond = ioperand(if_, interpreter);

                let new_line = if slice_to_bool(&*cond) { *yes } else { *no };

                interpreter.current_line = interpreter
                    .mir
                    .lines
                    .iter()
                    .position(|marker| matches!(marker, MLine::Marker(line) if *line == new_line))
                    .unwrap();
            },
        }

        processed += 1;

        interpreter.current_line += 1;

        if (processed > 1000000) {
            panic!("some kind of loop occured in the interpreter");
        }
    }
}

macro_rules! ibinop_int {
    ($math_type:ty, $l_data:expr, $r_data:expr, $op:expr) => {{
        let l_bytes = <&[u8; std::mem::size_of::<$math_type>()]>::try_from(&*$l_data).unwrap();
        let r_bytes = <&[u8; std::mem::size_of::<$math_type>()]>::try_from(&*$r_data).unwrap();
        let l_val = <$math_type>::from_ne_bytes(*l_bytes);
        let r_val = <$math_type>::from_ne_bytes(*r_bytes);

        if $op.is_cmp() {
            ibinop_cmp!(l_val, r_val, $op)            
        } else {
            let result = match $op {
                Opcode::Plus => <$math_type>::wrapping_add(l_val, r_val),
                Opcode::Minus => <$math_type>::wrapping_sub(l_val, r_val),
                Opcode::Multiplier => <$math_type>::wrapping_mul(l_val, r_val),
                Opcode::Divider => <$math_type>::wrapping_div(l_val, r_val),
                Opcode::Modulus => <$math_type>::wrapping_rem(l_val, r_val),
                Opcode::BitAND => l_val & r_val,
                Opcode::BitOR => l_val | r_val,
                Opcode::BitXOR => l_val ^ r_val,
                Opcode::BitShiftL => l_val << r_val,
                Opcode::BitShiftR => l_val >> r_val,
                _ => unreachable!(),
            };

            result.to_ne_bytes().into()
        }
    }}
}

macro_rules! iunop {
    ($math_type:ty, $h_data:expr, $op:expr) => {{
        let bytes = <&[u8; std::mem::size_of::<$math_type>()]>::try_from(&*$h_data).unwrap();
        let val = <$math_type>::from_ne_bytes(*bytes);

        let result = match $op {
            Opcode::Minus => -val,
            _ => unreachable!(),
        };

        result.to_ne_bytes().into()
    }}
}

macro_rules! ibinop_float {
    ($math_type:ty, $l_data:expr, $r_data:expr, $op:expr) => {{
        let l_bytes = <&[u8; std::mem::size_of::<$math_type>()]>::try_from(&*$l_data).unwrap();
        let r_bytes = <&[u8; std::mem::size_of::<$math_type>()]>::try_from(&*$r_data).unwrap();
        let l_val = <$math_type>::from_ne_bytes(*l_bytes);
        let r_val = <$math_type>::from_ne_bytes(*r_bytes);

        if $op.is_cmp() {
            ibinop_cmp!(l_val, r_val, $op)            
        } else {
            let result = match $op {
                Opcode::Plus => l_val + r_val,
                Opcode::Minus => l_val - r_val,
                Opcode::Multiplier => l_val * r_val,
                Opcode::Divider => l_val / r_val,
                Opcode::Modulus => l_val % r_val,
                _ => unreachable!(),
            };

            result.to_ne_bytes().into()
        }
    }}
}

macro_rules! ibinop_cmp {
    ($l_val:expr, $r_val:expr, $op:expr) => {{
        let result = match $op {
            Opcode::LessThan => $l_val < $r_val,
            Opcode::GrtrThan => $l_val > $r_val,
            Opcode::LessOrEqual => $l_val <= $r_val,
            Opcode::GreaterOrEqual => $l_val >= $r_val,
            Opcode::Equal => $l_val == $r_val,
            Opcode::Inequal => $l_val != $r_val,
            _ => unreachable!(),
        };

        if result {
            vec![1u8].into_boxed_slice()
        } else {
            vec![0u8].into_boxed_slice()
        }
    }}
}

fn iexpr(reg: Option<&MReg>, expr: &MExpr, interpreter: &mut Interpreter) -> RegValue {
    match expr {
        MExpr::MemLoc(memloc) => imemloc(memloc, interpreter),
        MExpr::Addr(addr) => {
            let load_size = interpreter.mir.reg_types[reg.unwrap()].size();
            let pointer = iaddr(addr, interpreter);
            let val = load_addr_with_len(pointer, interpreter, load_size);

            val
        }
        MExpr::BinOp { left_type, op, l, r } => {
            let l = ioperand(l, interpreter);
            let r = ioperand(r, interpreter);

            use TypeEnum::*;
            use IntSize::*;

            match left_type.as_type_enum() {
                Int(IntType { signed: true, size: _64 }) => ibinop_int!(i64, l, r, op),
                Int(IntType { signed: true, size: _32 }) => ibinop_int!(i32, l, r, op),
                Int(IntType { signed: true, size: _16 }) => ibinop_int!(i16, l, r, op),
                Int(IntType { signed: true, size: _8 }) => ibinop_int!(i8, l, r, op),
                Int(IntType { signed: false, size: _64 }) => ibinop_int!(u64, l, r, op),
                Int(IntType { signed: false, size: _32 }) => ibinop_int!(u32, l, r, op),
                Int(IntType { signed: false, size: _16 }) => ibinop_int!(u16, l, r, op),
                Int(IntType { signed: false, size: _8 }) => ibinop_int!(u8, l, r, op),
                Float(FloatType::F64) => ibinop_float!(f64, l, r, op),
                Float(FloatType::F32) => ibinop_float!(f32, l, r, op),
                Bool => {
                    let l = slice_to_bool(&*l);
                    let r = slice_to_bool(&*r);

                    let result = match op {
                        Opcode::Equal => l == r,
                        Opcode::Inequal => l != r,
                        Opcode::And => l && r,
                        Opcode::Or => l || r,
                        _ => unreachable!(),
                    };

                    bool_to_boxed_slice(result)
                },
                Ref(_) if *op == Opcode::Equal => bool_to_boxed_slice(l == r),
                Ref(_) if *op == Opcode::Inequal => bool_to_boxed_slice(l != r),
                _ => todo!("{left_type:?}"),
            }
        },
        MExpr::UnarOp { ret_type, op, hs } => {
            use TypeEnum::*;

            let hs = ioperand(hs, interpreter);

            match ret_type.as_type_enum() {
                Int(IntType { signed: true, size: _64 }) => iunop!(i64, hs, op),
                Int(IntType { signed: true, size: _32 }) => iunop!(i32, hs, op),
                Int(IntType { signed: true, size: _16 }) => iunop!(i16, hs, op),
                Int(IntType { signed: true, size: _8 }) => iunop!(i8, hs, op),
                Bool if *op == Opcode::Not => bool_to_boxed_slice(!slice_to_bool(&*hs)),
                _ => todo!("{ret_type:?}"),
            }
        },
        MExpr::Call { typ, f, a, sret } => {
            let args: Vec<_> = a.iter().map(|operand| ioperand(operand, interpreter)).collect();
            let sret = sret.as_ref().map(|memloc| imemloc(memloc, interpreter));

            let func_id = match f {
                MCallable::Func(func_id) => {
                    *func_id
                },
                MCallable::FirstClass(func_pointer) => {
                    let memloc = imemloc(func_pointer, interpreter);
                    FuncId::from(KeyData::from_ffi(slice_to_usize(&*memloc) as u64))
                },
            };

            if let Some(mir) = interpreter.state.mirs.get(func_id) {
                let stack = Stack::for_vars(&mir.variables);
                interpreter.allocator.stacks.push(stack);

                let mut new_interpreter = Interpreter {
                    registers: BTreeMap::new(),
                    addr_registers: BTreeMap::new(),
                    arg_regs: args,
                    sret_reg: sret,
                    func_id,
                    force_safety: interpreter.force_safety,
                    current_line: 0,
                    allocator: interpreter.allocator,
                    state: interpreter.state,
                    mir,
                };

                #[cfg(feature = "backend-debug")]
                println!("calling new function");
                let value = interpret(&mut new_interpreter);

                #[cfg(feature = "backend-debug")]
                {
                    println!("extiting call");
                    if let Some(serialized_return) = value.to_string_no_unit() {
                        println!("returning: {serialized_return}");
                    }
                }

                interpreter.allocator.stacks.pop();

                return value.data;
            } else {
                panic!("{typ:?}")
            }
        },
        MExpr::Free { ptr } => {
            let ptr_usize = slice_to_usize(&*ioperand(ptr, interpreter));
            let ptr = ptr_usize as *const u8;

            if let Some(removed) = interpreter.allocator.allocations.remove(&ptr) {
                std::mem::drop(removed);
                interpreter.allocator.freed.push(ptr);
            } else if interpreter.allocator.freed.iter().any(|p| *p == ptr) {
                panic!("attempt to double free memory at {ptr:?}");
            } else {
                panic!("attempt to free undefined memory at {ptr:?}");
            }

            Vec::new().into_boxed_slice()
        },
        MExpr::Ref { on } => {
            let on = iaddr(on, interpreter);
            on.to_ne_bytes().into()
        },
        MExpr::Alloc { len } => {
            let len_data_slice = ioperand(len, interpreter);
            let len_data_arr = <[u8; 8]>::try_from(&*len_data_slice).unwrap();
            let len_usize = usize::from_ne_bytes(len_data_arr);
            let allocation = Allocation {
                data: vec![0u8; len_usize].into_boxed_slice(),
                source: Some((interpreter.func_id, interpreter.current_line)),
            };
            let pointer_data_usize = allocation.data.as_ptr() as usize;
            interpreter.allocator.allocations.insert(allocation.data.as_ptr(), allocation);
            pointer_data_usize.to_ne_bytes().into()
        },
        MExpr::Deref { to, on } => {
            let size = to.size();
            let on = imemloc(on, interpreter);
            let on_data_arr = <[u8; 8]>::try_from(&*on).unwrap();
            let on_usize = usize::from_ne_bytes(on_data_arr);
            load_addr_with_len(on_usize, interpreter, size)
        },
    }
}

fn iaddrexpr(expr: &MAddrExpr, interpreter: &mut Interpreter) -> AddrRegValue {
    match expr {
        MAddrExpr::Expr(expr) => {
            let expr_data_slice = iexpr(None, expr, interpreter);
            let expr_data_array = <[u8; 8]>::try_from(&*expr_data_slice).unwrap();
            usize::from_ne_bytes(expr_data_array)
        },
        MAddrExpr::Addr(addr) => {
            iaddr(addr, interpreter)
        },
        MAddrExpr::Member { object_type, object, field_index } => {
            let object_pointer = iaddr(object, interpreter);
            let TypeEnum::Struct(struct_type) = object_type.as_type_enum() 
                else { unreachable!() };
            object_pointer + struct_type.field_offset_in_bytes(*field_index as usize)
        },
        MAddrExpr::Index { array_type, element_type, object, index } => {
            let object_pointer = iaddr(object, interpreter);
            let index = slice_to_usize(&*ioperand(index, interpreter));
            object_pointer + element_type.size() * index
        },
    }
}

fn ioperand(operand: &MOperand, interpreter: &mut Interpreter) -> RegValue {
    match operand {
        MOperand::Memloc(loc) => imemloc(loc, interpreter),
        MOperand::Int { size, val } => {
            match size {
                64 => (*val as u64).to_ne_bytes().into(),
                32 => (*val as u32).to_ne_bytes().into(),
                16 => (*val as u16).to_ne_bytes().into(),
                8 => (*val as u8).to_ne_bytes().into(),
                _ => unreachable!(),
            }
        },
        MOperand::Float { size, val } => {
            match size {
                64 => (*val as f64).to_ne_bytes().into(),
                32 => (*val as f32).to_ne_bytes().into(),
                _ => unreachable!(),
            }
        },
        MOperand::Function(func_id) => {
            func_id.data().as_ffi().to_ne_bytes().into()
        },
        MOperand::Bool(bool) => bool_to_boxed_slice(*bool),
    }
}

fn imemloc(memloc: &MMemLoc, interpreter: &mut Interpreter) -> RegValue {
    match memloc {
        MMemLoc::Reg(reg) => interpreter.registers[reg].clone(),
        MMemLoc::Var(var) => {
            let arg_index = &interpreter.mir.variables[*var].arg_index;

            if let ArgIndex::Some(index) = arg_index {
                interpreter.arg_regs[*index].clone()
            } else if arg_index == &ArgIndex::SRet {
                interpreter.sret_reg.clone().unwrap()
            } else {
                let stack = interpreter.allocator.stacks.last().unwrap();
                let (location, size) = stack.var_locations_and_size[*var];
                stack.data[location..(location + size)].into()
            }
        }
        MMemLoc::Global(global_var) => {
            let global = &interpreter.state.globals[global_var];
            (global.pointer as usize).to_ne_bytes().into()
        },
    }
}

fn iaddr(addr: &MAddr, interpreter: &Interpreter) -> AddrRegValue {
    match addr {
        MAddr::Reg(addr_reg) => interpreter.addr_registers[addr_reg],
        MAddr::Var(var_id) => {
            let stack = interpreter.allocator.stacks.last().unwrap();
            let (location, size) = stack.var_locations_and_size[*var_id];
            let stack_data_ptr_as_usize = stack.data.as_ptr() as usize;
            stack_data_ptr_as_usize + location
        },
    }
}

fn store_into_addr(addr: &MAddr, val: Box<[u8]>, interpreter: &mut Interpreter) {
    match addr {
        MAddr::Reg(addr_reg) => {
            let pointer = interpreter.addr_registers[addr_reg];
            let slice = addr_to_slice(pointer, interpreter);
            slice[0..(val.len())].copy_from_slice(&*val)
        },
        MAddr::Var(var_id) => {
            let stack = interpreter.allocator.stacks.last_mut().unwrap();
            let (location, size) = stack.var_locations_and_size[*var_id];
            stack.data[location..(location + size)].copy_from_slice(&*val);
        },
    }
}

fn slice_to_usize(slice: &[u8]) -> usize {
    let arr = <[u8; 8]>::try_from(&*slice).unwrap();
    usize::from_ne_bytes(arr)
}

fn slice_to_bool(slice: &[u8]) -> bool {
    match slice{
        [0] => false,
        [1] => true,
        _ => unreachable!(),
    }
}

fn bool_to_boxed_slice(bool: bool) -> Box<[u8]> {
    vec![bool as u8].into_boxed_slice()
}

fn load_addr_with_len(addr: usize, interpreter: &mut Interpreter, len: usize) -> Box<[u8]> {
    addr_to_slice(addr, interpreter)[0..len].into()
}

fn addr_to_slice<'a>(addr: usize, interpreter: &'a mut Interpreter) -> &'a mut [u8] {
    for stack in &mut interpreter.allocator.stacks {
        let beginning = stack.data.as_ptr() as usize;
        let end = beginning + stack.data.len();

        if (beginning <= addr && addr < end) {
            return &mut stack.data[(addr - beginning)..];
        }
    }

    for (a, allocation) in interpreter.allocator.allocations.iter_mut() {
        let beginning = allocation.data.as_ptr() as usize;
        let end = beginning + allocation.data.len();

        if (beginning <= addr && addr < end) {
            return &mut allocation.data[(addr - beginning)..];
        }
    }
    
    if interpreter.allocator.freed.contains(&(addr as *const u8)) {
        panic!("tried to access previously freed memory at 0x{addr:x}!");
    }

    panic!("tried to access undefined memory at 0x{addr:x}!");
}
