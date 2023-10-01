use std::{slice, collections::HashMap};

use slotmap::SecondaryMap;

use crate::{unit::FuncId, Value, mir::{MLine, MOperand, MAddr, MLit, MMemLoc, MReg, MExpr, MIR, MCallable, MAddrExpr}, hlr::hlr_data::{VarID, ArgIndex}, Type, parse::Opcode, IntType, TypeEnum, FloatType};

use super::IntepreterState;

struct Interpreter<'a> {
    allocation_handles: HashMap<*const u8, Box<[u8]>>,
    variables: SecondaryMap<VarID, Box<[u8]>>,
    registers: Vec<Option<Box<[u8]>>>,
    addr_registers: Vec<Option<*mut u8>>,
    current_line: usize,
    markers: Vec<usize>,
    state: &'a IntepreterState,
    mir: &'a MIR,
}

impl<'a> Interpreter<'a> {
    fn for_mir(
        mir: &'a MIR, 
        state: &'a IntepreterState, 
        args: Vec<Box<[u8]>>, 
        sret: Option<Box<[u8]>>,
    ) -> Self {
        let variables = mir
            .variables
            .iter()
            .map(|(var_id, var_info)| {
                let data = if let ArgIndex::Some(index) = var_info.arg_index {
                    args[index].clone()
                } else if var_info.arg_index == ArgIndex::SRet {
                    sret.as_ref().unwrap().clone()
                } else {
                    vec![0u8; var_info.typ.size()].into_boxed_slice()
                };

                (var_id, data)
            })
            .collect();
        Interpreter {
            allocation_handles: HashMap::new(),
            variables,
            current_line: 0,
            registers: vec![None; mir.reg_count as usize],
            addr_registers: vec![None; mir.addr_reg_count as usize],
            markers: vec![0; mir.block_count as usize],
            mir,
            state,
        }
    }
}

pub fn run(func_id: FuncId, state: &IntepreterState) -> Value {
    run_with_args(func_id, state, Vec::new(), None)
}

pub fn run_with_args(
    func_id: FuncId, 
    state: &IntepreterState, 
    args: Vec<Box<[u8]>>, 
    sret: Option<Box<[u8]>>,
) -> Value {
    let mir = &state.mirs[func_id];
    let mut interpreter = Interpreter::for_mir(&mir, state, args, sret);
    run_with_interpreter(mir, &mut interpreter)
}

fn run_with_interpreter(mir: &MIR, interpreter: &mut Interpreter) -> Value {
    for (l, line) in mir.lines.iter().enumerate() {
        match line {
            MLine::Marker(marker_id) => {
                // TODO: make markers more uniform
                interpreter.markers[*marker_id as usize] = l;
            },
            _ => {}
        }
    }

    std::thread::sleep(std::time::Duration::from_millis(100));

    loop {
        match &mir.lines[interpreter.current_line] {
            MLine::Set { l, r } => {
                let val = run_expr(r, interpreter, Some(l));
                interpreter.registers[l.0 as usize] = Some(val);
            },
            MLine::SetAddr { l, r } => {
                let val = run_addr_expr(r, interpreter);
                interpreter.addr_registers[l.0 as usize] = Some(val);
            },
            MLine::Store { l, val } => {
                let operand_data = get_operand(&val, &interpreter);
                let pointer = get_mut_pointer_from_addr(l, interpreter);
                checked_memcpy(operand_data.as_ptr(), pointer, operand_data.len());
            },
            MLine::MemCpy { from, to, len } => {
                let from_ptr = get_const_pointer_from_addr(from, interpreter);
                let to_ptr = get_mut_pointer_from_addr(to, interpreter);
                let len_data = get_operand(len, interpreter);
                let len: usize = get_val_from_slice(&*len_data);
                
                checked_memcpy(from_ptr, to_ptr, len);
            },
            MLine::MemMove { from, to, len } => {
                let from_data = get_operand(from, interpreter);
                let from_ptr = get_val_from_slice::<*const u8>(&*from_data);
                let to_data = get_operand(to, interpreter);
                let to_ptr = get_val_from_slice::<*mut u8>(&*to_data);
                let len_data = get_operand(len, interpreter);
                let len: usize = get_val_from_slice(&*len_data);
                
                checked_memcpy(from_ptr, to_ptr, len);
            },
            MLine::Return(val) => {
                return if let Some(val) = val {
                    let data = get_operand(val, &interpreter);

                    Value {
                        typ: mir.func_type.ret.clone(),
                        data,
                    }
                } else {
                    Value::void()
                }
            },
            MLine::Marker(_) => {}
            MLine::Goto(marker_id) => {
                interpreter.current_line = interpreter.markers[*marker_id as usize]
            },
            MLine::Expr(expr) => {
                run_expr(expr, interpreter, None);
            },
            MLine::Branch { if_, yes, no } => {
                let cond = get_operand(if_, &interpreter);
                assert_eq!(cond.len(), 1);

                let new_line = if cond[0] == 0 { *no } else { *yes };

                interpreter.current_line = interpreter.markers[new_line as usize];
            },
        }

        interpreter.current_line += 1;
    }
}

fn run_expr(expr: &MExpr, interpreter: &mut Interpreter, reg: Option<&MReg>) -> Box<[u8]> {
    let output = match expr {
        MExpr::MemLoc(memloc) => load_memloc(memloc, interpreter).into(),
        MExpr::Addr(addr) => {
            let pointer = get_const_pointer_from_addr(addr, interpreter);
            let reg_type = &interpreter.mir.reg_types[reg.unwrap()];
            checked_load_ptr(pointer, reg_type.size())
        },
        MExpr::BinOp { left_type, op, l, r } => run_binop(left_type, *op, l, r, interpreter),
        MExpr::UnarOp { ret_type, op, hs } => todo!(),
        MExpr::Call { typ, f, a, sret } => {
            let args = a.iter().map(|operand| get_operand(operand, interpreter)).collect();
            let sret = sret
                .as_ref()
                .map(|memloc| load_memloc(memloc, interpreter));
            match f {
                MCallable::Func(func_id) => {
                    if let Some(mir) = interpreter.state.mirs.get(*func_id) {
                        let mut new_interpreter = Interpreter::for_mir(
                            mir, 
                            interpreter.state, 
                            args, 
                            sret.map(<&[u8] as Into<Box<[u8]>>>::into),
                        );
                        let value = run_with_interpreter(mir, &mut new_interpreter);
                        value.data
                    } else if let Some(external_func) = 
                        interpreter.state.external_functions.get(*func_id) {
                        super::call_with_values::call_with_boxes(
                            external_func.pointer, 
                            external_func.typ.clone(), 
                            args,
                            sret.map(get_val_from_slice::<*mut u8>),
                        )
                    } else {
                        panic!()
                    }
                },
                MCallable::FirstClass(_) => todo!(),
            }
        },
        MExpr::Free { ptr } => {
            let ptr_data = get_operand(ptr, interpreter);
            let ptr: *const u8 = get_val_from_slice(&*ptr_data);
            interpreter.allocation_handles.remove(&ptr);
            Vec::new().into_boxed_slice()
        },
        MExpr::Ref { on } => {
            match on {
                MAddr::Reg(reg) => {
                    let pointer = interpreter.addr_registers[reg.0 as usize].unwrap();
                    val_to_slice(pointer)
                },
                MAddr::Var(var_id) => {
                    let pointer = interpreter.variables[*var_id].as_ptr();
                    val_to_slice(pointer)
                },
            }
        },
        MExpr::Alloc { len } => {
            let memloc = get_operand(len, interpreter);
            let alloca_size = get_val_from_slice::<usize>(&*memloc);
            let alloca_space = vec![0u8; alloca_size].into_boxed_slice();           
            let alloca_ptr = alloca_space.as_ptr();
            interpreter.allocation_handles.insert(alloca_ptr, alloca_space);
            val_to_slice(alloca_ptr)
        },
        MExpr::Deref { to, on } => {
            let pointer = load_memloc(on, interpreter);
            let as_ptr: *const u8 = get_val_from_slice(pointer);
            let size = to.size();
            checked_load_ptr(as_ptr, size)
        },
    };
    
    return output;
}

fn run_addr_expr(expr: &MAddrExpr, interpreter: &mut Interpreter) -> *mut u8 {
    match expr {
        MAddrExpr::Expr(expr) => {
            let expr_output = run_expr(expr, interpreter, None);
            get_val_from_slice(&*expr_output)
        },
        MAddrExpr::Addr(addr) => {
            get_mut_pointer_from_addr(addr, interpreter)
        },
        MAddrExpr::Member { object_type, object, field_index } => {
            let pointer = get_mut_pointer_from_addr(object, interpreter);
            let TypeEnum::Struct(struct_type) = object_type.as_type_enum() 
                else { unreachable!() };
            let field_offset = struct_type.field_offset(*field_index as usize);
            unsafe { pointer.add(field_offset) }
        },
        MAddrExpr::Index { array_type, element_type, object, index } => {
            let pointer = get_mut_pointer_from_addr(object, interpreter);
            let index_data = get_operand(index, interpreter);
            let index: usize = get_val_from_slice(&*index_data);
            let field_offset = element_type.size() * index;
            unsafe { pointer.add(field_offset) }
        },
    }
}

macro_rules! irun_binop_typed {
    ($math_type:ty, $l_data:expr, $r_data:expr, $op:expr) => {{
        let l: $math_type = get_val_from_slice(&*$l_data);
        let r: $math_type = get_val_from_slice(&*$r_data);

        let result = match $op {
            Opcode::LessThan => l < r,
            Opcode::GrtrThan => l > r,
            Opcode::LessOrEqual => l <= r,
            Opcode::GreaterOrEqual => l >= r,
            Opcode::Equal => l == r,
            Opcode::Inequal => l != r,
            _ => {
                let result = match $op {
                    Opcode::Plus => <$math_type>::wrapping_add(l, r),
                    Opcode::Minus => <$math_type>::wrapping_sub(l, r),
                    Opcode::Multiplier => <$math_type>::wrapping_mul(l, r),
                    Opcode::Divider => <$math_type>::wrapping_div(l, r),
                    Opcode::Modulus => <$math_type>::wrapping_rem(l, r),
                    Opcode::BitAND => l & r,
                    Opcode::BitOR => l | r,
                    Opcode::BitXOR => l ^ r,
                    Opcode::BitShiftL => l << r,
                    Opcode::BitShiftR => l >> r,
                    _ => todo!(),
                };

                return val_to_slice(result)
            }
        };

        val_to_slice(result)
    }}
}

macro_rules! frun_binop_typed {
    ($math_type:ty, $l_data:expr, $r_data:expr, $op:expr) => {{
        let l: $math_type = get_val_from_slice(&*$l_data);
        let r: $math_type = get_val_from_slice(&*$r_data);

        let result = match $op {
            Opcode::LessThan => l < r,
            Opcode::GrtrThan => l > r,
            Opcode::Inequal => l != r,
            _ => {
                let result = match $op {
                    Opcode::Plus => l + r,
                    Opcode::Minus => l - r,
                    Opcode::Multiplier => l * r,
                    Opcode::Divider => l / r,
                    Opcode::Modulus => l % r,
                    _ => todo!(),
                };

                return val_to_slice(result)
            }
        };

        val_to_slice(result)
    }}
}

fn run_binop(left_type: &Type, op: Opcode, l: &MOperand, r: &MOperand, interpreter: &Interpreter) -> Box<[u8]> {
    let ld = get_operand(l, interpreter);
    let rd = get_operand(r, interpreter);

    use TypeEnum::*;

    match left_type.as_type_enum() {
        Int(IntType { signed: false, size: 8 }) => irun_binop_typed!(u8, ld, rd, op),
        Int(IntType { signed: false, size: 16 }) => irun_binop_typed!(u16, ld, rd, op),
        Int(IntType { signed: false, size: 32 }) => irun_binop_typed!(u32, ld, rd, op),
        Int(IntType { signed: false, size: 64 }) => irun_binop_typed!(u64, ld, rd, op),
        Int(IntType { signed: true, size: 8 }) => irun_binop_typed!(i8, ld, rd, op),
        Int(IntType { signed: true, size: 16 }) => irun_binop_typed!(i16, ld, rd, op),
        Int(IntType { signed: true, size: 32 }) => irun_binop_typed!(i32, ld, rd, op),
        Int(IntType { signed: true, size: 64 }) => irun_binop_typed!(i64, ld, rd, op),
        Int(_) => unreachable!(),
        Float(FloatType::F32) => frun_binop_typed!(f32, ld, rd, op),
        Float(FloatType::F64) => frun_binop_typed!(f64, ld, rd, op),
        Float(_) => unreachable!(),
        Struct(_) => todo!(),
        Ref(_) => todo!(),
        Func(_) => todo!(),
        Array(_) => todo!(),
        Bool => todo!(),
        Void => todo!(),
        Unknown => todo!(),
    }
}

fn get_operand(val: &MOperand, interpreter: &Interpreter) -> Box<[u8]> {
    match val {
        MOperand::Memloc(memloc) => load_memloc(memloc, interpreter).to_vec().into_boxed_slice(),
        MOperand::Lit(lit) => {
            match lit {
                MLit::Int { size, val } => {
                    let mut alloc = vec![0; (size / 8) as usize].into_boxed_slice();
                    move_val_into_slice(&mut *alloc, val);
                    alloc
                }
                MLit::Float { size, val } => {
                    let mut alloc = vec![0; (size / 8) as usize].into_boxed_slice();

                    match size {
                        32 => move_val_into_slice(&mut *alloc, &(*val as f32)),
                        64 => move_val_into_slice(&mut *alloc, &(*val as f64)),
                        _ => todo!(),
                    };
                    
                    alloc
                },
                MLit::Function(_) => todo!(),
                MLit::Bool(false) => vec![0u8, 1].into_boxed_slice(),
                MLit::Bool(true) => vec![1u8, 1].into_boxed_slice(),
            }
        },
    }
}

fn load_memloc<'a>(memloc: &MMemLoc, interpreter: &'a Interpreter) -> &'a [u8] {
    match memloc {
        MMemLoc::Reg(reg) => {
            &**interpreter.registers[reg.0 as usize].as_ref().unwrap()
        },
        MMemLoc::Var(var_id) => {
            &*interpreter.variables[*var_id]
        },
        MMemLoc::Global(_) => todo!(),
    }
}

fn get_const_pointer_from_addr(addr: &MAddr, interpreter: &Interpreter) -> *const u8 {
    match addr {
        MAddr::Reg(reg) => {
            interpreter.addr_registers[reg.0 as usize].unwrap()
        },
        MAddr::Var(var_id) => {
            interpreter.variables[*var_id].as_ptr()
        },
    }
}

fn get_mut_pointer_from_addr(addr: &MAddr, interpreter: &mut Interpreter) -> *mut u8 {
    match addr {
        MAddr::Reg(reg) => {
            interpreter.addr_registers[reg.0 as usize].unwrap()
        },
        MAddr::Var(var_id) => {
            interpreter.variables[*var_id].as_mut_ptr()
        },
    }
}

fn checked_memcpy(from: *const u8, to: *mut u8, len: usize) {
    unsafe {
        for n in 0..len {
            *to.add(n) = *from.add(n);
        }
    }
}

fn checked_load_ptr(load: *const u8, size: usize) -> Box<[u8]> {
    unsafe {
        let pointing_to = slice::from_raw_parts(load, size);

        let region = region::query(load).unwrap();
        assert!(size <= region.as_range().end - load as usize);

        Box::from(pointing_to)
    }
}

pub fn val_to_slice<T: Copy>(val: T) -> Box<[u8]> {
    let mut data = vec![0; std::mem::size_of::<T>()];
    move_val_into_slice(&mut *data, &val);
    data.into_boxed_slice()
}

pub fn move_val_into_slice<T: Copy>(slice: &mut [u8], val: &T) {
    let val_as_ptr = val as *const T as *const u8; 
    checked_memcpy(val_as_ptr, slice.as_mut_ptr(), slice.len());
}

pub fn get_val_from_slice<T: Copy>(slice: &[u8]) -> T {
    unsafe { *(slice.as_ptr() as *const T) }
}
