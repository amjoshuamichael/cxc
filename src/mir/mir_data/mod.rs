use std::{collections::{BTreeSet, BTreeMap}, fmt::{Formatter, Display}, fmt};

use crate::{hlr::hlr_data::{Variables, VariableInfo, ArgIndex}, UniqueFuncInfo, FuncType, Type, VarName, parse::Opcode, };

#[derive(Debug)]
pub struct MIR {
    pub lines: Vec<MLine>,
    pub variables: Variables,
    pub dependencies: BTreeSet<UniqueFuncInfo>,
    pub info: UniqueFuncInfo,
    pub func_type: FuncType,
    pub block_count: u32,
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

    pub fn new_variable(&mut self, base_name: &str, typ: Type) -> VarName {
        let mut base_name = String::from(base_name);
        let mut var_name = VarName::from(&*base_name);

        while self.variables.contains_key(&var_name) {
            base_name += "_";
            var_name = VarName::from(&*base_name);
        }

        self.variables.insert(var_name, VariableInfo { 
            typ, 
            arg_index: ArgIndex::None, 
            ..Default::default()
        });

        VarName::from(&*base_name)
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
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct MReg(u32);

impl ToString for MReg {
    fn to_string(&self) -> String { format!("{self}") }
}
impl Display for MReg {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result { write!(f, "_{}", self.0) }
}
impl fmt::Debug for MReg {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result { write!(f, "%{self}") }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct MAddrReg(u32);

impl ToString for MAddrReg {
    fn to_string(&self) -> String { format!("{self}") }
}
impl Display for MAddrReg {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result { write!(f, "-{}", self.0) }
}
impl fmt::Debug for MAddrReg {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result { write!(f, "%{self}") }
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub enum MMemLoc {
    Reg(MReg),
    Var(VarName),
}

impl fmt::Debug for MMemLoc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use MMemLoc::*;

        match self {
            Reg(reg) => write!(f, "{:?}", reg),
            Var(var) => write!(f, "{}", var),
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum MAddr {
    Reg(MAddrReg),
    Var(VarName),
}

impl fmt::Debug for MAddr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use MAddr::*;

        match self {
            Reg(reg) => write!(f, "{:?}", reg),
            Var(var) => write!(f, "@{}", var),
        }
    }
}

pub enum MLit {
    Int { size: u32, val: u64 },
    Float { size: u32, val: f64 },
    Bool(bool),
}

impl fmt::Debug for MLit {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use MLit::*;

        match self {
            Int { size, val } => write!(f, "{}i{}", val, size),
            Float { size, val } => write!(f, "{}f{}", val, size),
            Bool(val) => write!(f, "{}", val),
        }
    }
}

#[derive(Debug)]
pub enum MOperand {
    MemLoc(MMemLoc),
    Lit(MLit),
}

#[derive(Debug)]
pub enum MExpr {
    MemLoc(MMemLoc),
    Addr(MAddr),
    BinOp { left_type: Type, op: Opcode, l: MOperand, r: MOperand, },
    UnarOp { ret_type: Type, op: Opcode, hs: MOperand },
    Call { typ: FuncType, f: MCallable, a: Vec<MOperand>, sret: Option<MMemLoc> },
    Ref { on: MAddr },
    // TODO: replace operand with memloc here?
    Deref { to: Type, on: MOperand },
    Void,
}

#[derive(Debug)]
pub enum MCallable {
    Func(UniqueFuncInfo),
    FirstClass(MMemLoc),
}

#[derive(Debug)]
pub enum MAddrExpr {
    Expr(MExpr),
    Addr(MAddr),
    Member { object_type: Type, object: MAddr, field_index: u32 },
    Index { array_type: Type, element_type: Type, object: MAddr, index: MOperand },
}
