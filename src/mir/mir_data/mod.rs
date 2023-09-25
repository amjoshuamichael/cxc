use std::{collections::{BTreeMap, HashMap}, fmt::{Formatter, Display}, fmt};

use slotmap::SlotMap;

use crate::{hlr::hlr_data::{VariableInfo, ArgIndex, VarID, GotoLabelID}, FuncType, Type, VarName, parse::Opcode, unit::FuncId, FuncQuery};

#[derive(Debug)]
pub struct MIR {
    pub lines: Vec<MLine>,
    pub variables: SlotMap<VarID, VariableInfo>,
    pub dependencies: HashMap<FuncQuery, FuncId>,
    pub func_type: FuncType,
    pub block_count: u32,
    pub block_labels: HashMap<GotoLabelID, u32>,
    pub reg_count: u32,
    pub addr_reg_count: u32,
    pub reg_types: BTreeMap<MReg, Type>,
}

impl MIR {
    pub fn new_reg(&mut self, typ: Type) -> MReg {
        let current_reg = self.reg_count;
        self.reg_count += 1;
        let reg = MReg(current_reg);

        self.reg_types.insert(reg, typ);

        reg
    }

    pub fn new_addr_reg(&mut self) -> MAddrReg {
        let current_reg = self.addr_reg_count;
        self.addr_reg_count += 1;
        MAddrReg(current_reg)
    }

    pub fn new_variable(&mut self, typ: Type) -> VarID {
        self.variables.insert(VariableInfo { 
            typ, 
            arg_index: ArgIndex::None, 
            name: VarName::None,
            do_not_drop: false,
        })
    }
}

pub enum MLine {
    Set {
        l: MReg,
        r: MExpr,
    },
    SetAddr {
        l: MAddrReg,
        r: MAddrExpr,
    },
    Store {
        l: MAddr,
        val: MOperand,
    },
    MemCpy {
        from: MAddr,
        to: MAddr,
        len: MOperand,
    },
    MemMove {
        from: MOperand,
        to: MOperand,
        len: MOperand,
    },
    Return(Option<MOperand>),
    Marker(u32),
    Goto(u32),
    Expr(MExpr),
    Branch {
        if_: MOperand,
        yes: u32,
        no: u32,
    },
}

impl fmt::Debug for MLine {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use MLine::*;

        match self {
            Set { l, r } => write!(f, "rset {:?} <- {:?}", l, r),
            SetAddr { l, r } => write!(f, "aset {:?} <- {:?}", l, r),
            Store { l, val } => write!(f, "stor {:?} <- {:?}", l, val),
            Return(val) => write!(f, "; {:?}", val),
            Marker(id) => write!(f, "mark {}", id),
            Goto(id) => write!(f, "goto {}", id),
            Expr(expr) => write!(f, "{:?}", expr),
            Branch { if_, yes, no } => write!(f, "if {:?} goto {} else {}", if_, yes, no),
            MemCpy { from, to, len } => write!(f, "mcpy {from:?} to {to:?}; {len:?} bytes"),
            MemMove { from, to, len } => write!(f, "mmve {from:?} to {to:?}; {len:?} bytes"),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct MReg(u32);

impl Display for MReg {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result { write!(f, "_{}", self.0) }
}
impl fmt::Debug for MReg {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result { write!(f, "%{self}") }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct MAddrReg(u32);

impl Display for MAddrReg {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result { write!(f, "-{}", self.0) }
}
impl fmt::Debug for MAddrReg {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result { write!(f, "%{self}") }
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub enum MMemLoc {
    Reg(MReg),
    Var(VarID),
    Global(VarName),
}

impl fmt::Debug for MMemLoc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use MMemLoc::*;

        match self {
            Reg(reg) => write!(f, "{:?}", reg),
            Var(var) => write!(f, "{:?}", var),
            Global(var) => write!(f, "{}", var),
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum MAddr {
    Reg(MAddrReg),
    Var(VarID),
}

impl fmt::Debug for MAddr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use MAddr::*;

        match self {
            Reg(reg) => write!(f, "{:?}", reg),
            Var(var) => write!(f, "@{:?}", var),
        }
    }
}

pub enum MLit {
    Int { size: u32, val: u64 },
    Float { size: u32, val: f64 },
    Function(FuncId),
    Bool(bool),
}

impl fmt::Debug for MLit {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use MLit::*;

        match self {
            Int { size, val } => write!(f, "{val}i{size}"),
            Float { size, val } => write!(f, "{val}f{size}"),
            Function(id) => write!(f, "{id:?}"),
            Bool(val) => write!(f, "{val}"),
        }
    }
}

#[derive(Debug)]
pub enum MOperand {
    Memloc(MMemLoc),
    Lit(MLit),
}

#[derive(Debug)]
pub enum MExpr {
    MemLoc(MMemLoc),
    Addr(MAddr),
    BinOp { left_type: Type, op: Opcode, l: MOperand, r: MOperand, },
    UnarOp { ret_type: Type, op: Opcode, hs: MOperand },
    Call { typ: FuncType, f: MCallable, a: Vec<MOperand>, sret: Option<MMemLoc> },
    Free { ptr: MOperand, },
    Ref { on: MAddr },
    Alloc { len: MOperand, },
    Deref { to: Type, on: MMemLoc },
    Void,
}

#[derive(Debug)]
pub enum MCallable {
    Func(FuncId),
    FirstClass(MMemLoc),
}

#[derive(Debug)]
pub enum MAddrExpr {
    Expr(MExpr),
    Addr(MAddr),
    Member { object_type: Type, object: MAddr, field_index: u32 },
    Index { array_type: Type, element_type: Type, object: MAddr, index: MOperand },
}
