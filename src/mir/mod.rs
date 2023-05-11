use std::{collections::{BTreeSet, BTreeMap}, fmt::{Display, Formatter}};

use crate::{VarName, parse::Opcode, UniqueFuncInfo, Type, CompData, hlr::{hlr_data_output::FuncOutput, expr_tree::{HNodeData, ExprTree}, hlr_data::{Variables, VariableInfo}}, FuncType, TypeEnum, ArrayType, typ::ReturnStyle};

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

    pub fn new_variable(&mut self, mut base_name: &str, typ: Type) -> VarName {
        let mut base_name = String::from(base_name);
        let mut var_name = VarName::from(&*base_name);

        while self.variables.contains_key(&var_name) {
            base_name += "_";
            var_name = VarName::from(&*base_name);
        }

        self.variables.insert(var_name, VariableInfo { typ, arg_index: None, });

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

impl std::fmt::Debug for MLine {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use MLine::*;

        match self {
            Set { l, r } => write!(f, "rset {:?} <- {:?}", l, r),
            SetAddr { l, r } => write!(f, "aset {:?} <- {:?}", l, r),
            Store { l, val } => write!(f, "stor {:?} <- {:?}", l, val),
            Return(val) => write!(f, "; {:?}", val),
            Marker(id) => write!(f, "marker {}", id),
            Goto(id) => write!(f, "goto {}", id),
            Expr(expr) => write!(f, "{:?}", expr),
            Branch { if_, yes, no } => write!(f, "if {:?} goto {} else {}", if_, yes, no),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct MReg(u32);

impl ToString for MReg {
    fn to_string(&self) -> String { format!("{self}") }
}
impl Display for MReg {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result { write!(f, "_{}", self.0) }
}
impl std::fmt::Debug for MReg {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result { write!(f, "%{self}") }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct MAddrReg(u32);

impl ToString for MAddrReg {
    fn to_string(&self) -> String { format!("{self}") }
}
impl Display for MAddrReg {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result { write!(f, "-{}", self.0) }
}
impl std::fmt::Debug for MAddrReg {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result { write!(f, "%{self}") }
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub enum MMemLoc {
    Reg(MReg),
    Var(VarName),
}

impl std::fmt::Debug for MMemLoc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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

impl std::fmt::Debug for MAddr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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

impl std::fmt::Debug for MLit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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
    Lit(MLit),
    Addr(MAddr),
    BinOp { ret_type: Type, op: Opcode, l: MOperand, r: MOperand, },
    UnarOp { ret_type: Type, op: Opcode, hs: MOperand },
    Array { elem_type: Type, elems: Vec<MOperand> },
    Call { typ: FuncType, f: UniqueFuncInfo, a: Vec<MOperand> },
    Cast { to: Type, from: MOperand },
    Ref { on: MAddr },
    Deref { to: Type, on: MOperand },
}

#[derive(Debug)]
pub enum MAddrExpr {
    Expr(MExpr),
    Addr(MAddr),
    Member { object_type: Type, object: MAddr, field_index: u32 },
    Index { array_type: Type, element_type: Type, object: MAddr, index: MOperand },
}

#[derive(Copy, Clone, Debug)]
pub enum MathType {
    Int,
    Float,
    Bool,
}

pub fn mir(hlr: FuncOutput, comp_data: &CompData) -> MIR {
    let mut mir = MIR { 
        dependencies: hlr.get_func_dependencies(),
        lines: Vec::new(), 
        variables: hlr.data_flow,
        info: hlr.info,
        func_type: hlr.func_type,
        block_count: 0,
        reg_count: 0,
        addr_reg_count: 0,
        reg_types: BTreeMap::new(),
    };

    build_block(hlr.tree.get(hlr.tree.root), &hlr.tree, &mut mir);

    if crate::XC_DEBUG {
        for (l, line) in mir.lines.iter().enumerate() {
            // print the index with three digits of space
            println!("{:03}: {}", l, format!("{:?}", line).replace("\n", " "));
        }
    }

    mir
}

fn build_block(node: HNodeData, tree: &ExprTree, mir: &mut MIR) {
    let HNodeData::Block { stmts, .. }  = node else { unreachable!() };

    fn new_block_id(mir: &mut MIR) -> u32 {
        let current_block = mir.block_count;
        mir.block_count += 1;
        current_block
    }

    for stmt in stmts {
        match tree.get(stmt) {
            HNodeData::Number { .. } | HNodeData::Float { .. } | HNodeData::Bool { .. } | HNodeData::Ident { .. } => {},
            HNodeData::MakeVar { name, rhs, .. } => panic!(),
            HNodeData::Set { lhs, rhs, .. } => {
                let lhs = build_as_addr(tree.get(lhs), &tree, mir);
                let rhs = build_as_operand(tree.get(rhs), &tree, mir);

                mir.lines.push(MLine::Store { l: lhs, val: rhs });
            },
            HNodeData::Return { to_return: Some(expr), .. } => {
                let operand = build_as_operand(tree.get(expr), &tree, mir);

                mir.lines.push(MLine::Return(Some(operand)));
            },
            HNodeData::Return { to_return: None, .. } => {
                mir.lines.push(MLine::Return(None));
            },
            HNodeData::While { w, d } => {
                let beginwhile = new_block_id(mir);
                let whilecode = new_block_id(mir);
                let pastwhile = new_block_id(mir);

                mir.lines.push(MLine::Goto(beginwhile));
                mir.lines.push(MLine::Marker(beginwhile));

                let w = build_as_operand(tree.get(w), tree, mir);

                mir.lines.push(MLine::Branch { 
                    if_: w, 
                    yes: whilecode,
                    no: pastwhile,
                });

                mir.lines.push(MLine::Marker(whilecode));

                build_block(tree.get(d), tree, mir);

                mir.lines.push(MLine::Goto(beginwhile));

                mir.lines.push(MLine::Marker(pastwhile));
            },
            HNodeData::IfThen { i, t, .. } => {
                let i = build_as_operand(tree.get(i), tree, mir);

                let then = new_block_id(mir);
                let after = new_block_id(mir);

                mir.lines.push(MLine::Branch { 
                    if_: i, 
                    yes: then,
                    no: after,
                });

                mir.lines.push(MLine::Marker(then));

                build_block(tree.get(t), tree, mir);
                mir.lines.push(MLine::Goto(after));

                mir.lines.push(MLine::Marker(after));
            }
            _ => {
                let expr = build_as_expr(tree.get(stmt), tree, mir);
                mir.lines.push(MLine::Expr(expr));
            },
        }
    }
}

fn build_as_memloc(node: HNodeData, tree: &ExprTree, add_to: &mut MIR) -> MMemLoc {
    if let HNodeData::Ident { name, .. } = node {
        return MMemLoc::Var(name);
    }

    MMemLoc::Reg(build_as_reg(node, tree, add_to))
}

fn build_as_addr(node: HNodeData, tree: &ExprTree, mir: &mut MIR) -> MAddr {
    match node {
        HNodeData::Ident { name, .. } => {
            if mir.variables[&name].arg_index.is_some() {
                let new_var_name = VarName::from(&*(name.to_string() + "addr"));

                if !mir.variables.contains_key(&new_var_name) {
                    mir.lines.insert(0, MLine::Store {
                        l: MAddr::Var(new_var_name.clone()),
                        val: MOperand::MemLoc(MMemLoc::Var(name.clone())),
                    });

                    mir.variables.insert(new_var_name.clone(), VariableInfo {
                        typ: mir.variables[&name].typ.clone(),
                        arg_index: None,
                    });
                }

                MAddr::Var(new_var_name)
            } else {
                MAddr::Var(name)
            }
        },
        HNodeData::UnarOp { ret_type, op, hs } if op == Opcode::Deref => {
            let load = build_as_memloc(tree.get(hs), tree, mir);
            let new_areg = mir.new_addr_reg();

            mir.lines.push(MLine::SetAddr { 
                l: new_areg, 
                r: MAddrExpr::Expr(MExpr::MemLoc(load)) 
            });

            MAddr::Reg(new_areg)
        },
        _ => MAddr::Reg(build_as_addr_reg(node, tree, mir)),
    }
}

fn build_as_operand(node: HNodeData, tree: &ExprTree, add_to: &mut MIR) -> MOperand {
    match node {
        HNodeData::Number { lit_type, value } => {
            MOperand::Lit(MLit::Int { size: lit_type.size() as u32 * 8, val: value })
        },
        HNodeData::Float { lit_type, value } => {
            MOperand::Lit(MLit::Float { size: lit_type.size() as u32 * 8, val: value })
        },
        HNodeData::Bool { value } => MOperand::Lit(MLit::Bool(value)),
        _ => {
            MOperand::MemLoc(build_as_memloc(node, tree, add_to))
        },
    }
}

fn build_as_reg(node: HNodeData, tree: &ExprTree, mir: &mut MIR) -> MReg {
    let l = mir.new_reg(node.ret_type());
    let r = build_as_expr(node, tree, mir);

    mir.lines.push(MLine::Set { l, r, });

    l
}

fn build_as_addr_reg(node: HNodeData, tree: &ExprTree, mir: &mut MIR) -> MAddrReg {
    let l = mir.new_addr_reg();
    let r = build_as_addr_expr(node, tree, mir);

    mir.lines.push(MLine::SetAddr { l, r, });

    l
}

pub fn build_as_expr(node: HNodeData, tree: &ExprTree, mir: &mut MIR) -> MExpr {
    let ret_type = node.ret_type();

    match node {
        HNodeData::BinOp { lhs, op, rhs, .. } => {
            let lhs = build_as_operand(tree.get(lhs), tree, mir);
            let rhs = build_as_operand(tree.get(rhs), tree, mir);

            MExpr::BinOp { ret_type, op, l: lhs, r: rhs }
        },
        HNodeData::UnarOp { op, hs, .. } => {
            match op {
                Opcode::Ref => MExpr::Ref { 
                    on: build_as_addr(tree.get(hs), tree, mir) 
                },
                Opcode::Deref => MExpr::Deref { 
                    to: node.ret_type(),
                    on: build_as_operand(tree.get(hs), tree, mir) 
                },
                _ => MExpr::UnarOp { 
                    ret_type, 
                    op, 
                    hs: build_as_operand(tree.get(hs), tree, mir), 
                },
            }
        },
        HNodeData::Call { ref f, ref a, ref ret_type, .. } => {
            let info = tree.unique_func_info_of_call(&node);
            let typ = FuncType {
                ret: ret_type.clone(),
                args: a.iter().map(|a| tree.get(*a).ret_type()).collect(),
            };
            let a = a.iter().map(|a| build_as_operand(tree.get(*a), tree, mir)).collect();

            MExpr::Call { typ, f: info, a }
        },
        HNodeData::ArrayLit { var_type, parts, initialize } => {
            let TypeEnum::Array(ArrayType { base, .. }) = var_type.as_type_enum() else { unreachable!() };
            let parts = parts.into_iter().map(|part| build_as_operand(tree.get(part), tree, mir)).collect();

            MExpr::Array {
                elem_type: base.clone(),
                // TODO: unify "elems" and "parts" throughout the entire codebase
                elems: parts,
            }
        }
        HNodeData::Member { .. } | HNodeData::Index { .. } => {
            let addr = build_as_addr(node, tree, mir);

            MExpr::Addr(addr)
        }
        _ => todo!("{:?}", node),
    }
}

pub fn build_as_addr_expr(node: HNodeData, tree: &ExprTree, mir: &mut MIR) -> MAddrExpr {
    let ret_type = node.ret_type();

    match node {
        HNodeData::Member { ret_type, object, field, .. } => {
            let object_node = tree.get(object);
            let object_type = object_node.ret_type().complete_deref();
            let object = build_as_addr(object_node, tree, mir);

            let TypeEnum::Struct(struct_type) = 
                object_type.as_type_enum() else { unreachable!() };
            let field_index = struct_type.get_field_index(&field).unwrap() as u32;
            
            MAddrExpr::Member { object_type, object, field_index }
        }
        HNodeData::Index { object, index, .. } => {
            let array_type = tree.get(object).ret_type();
            let TypeEnum::Array(ArrayType { base, .. }) = array_type.as_type_enum() 
                else { unreachable!() };
            let element_type = base.clone();

            let object = build_as_addr(tree.get(object), tree, mir);

            let index = build_as_operand(tree.get(index), tree, mir);

            MAddrExpr::Index { array_type, element_type, object, index }
        }
        HNodeData::ArrayLit { .. } => {
            let new_var = mir.new_variable("temp_storage", node.ret_type());
            let addr = MAddr::Var(new_var);

            let expr = build_as_operand(node, tree, mir);

            mir.lines.push(MLine::Store { l: addr.clone(), val: expr });

            MAddrExpr::Addr(addr)
        },
        _ => panic!("{node:?}"),
    }
}
