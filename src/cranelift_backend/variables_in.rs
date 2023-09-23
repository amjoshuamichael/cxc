use std::collections::BTreeSet;

use crate::{mir::{MLine, MExpr, MMemLoc, MAddr, MOperand, MCallable, MAddrExpr, MIR}, hlr::hlr_data::VarID, VarName};

// only gets variables that are loaded, not ones that are set. this data is required to 
// fill in cranelift-ir block parameters. also, this code does not remove duplicates.
//
// https://www.reddit.com/r/ProgrammingLanguages/comments/9z8qu3/a_new_compiler_backend_called_cranelift_uses/

#[derive(Default, Debug, PartialEq, Eq, Hash)]
pub struct VarsInMIR {
    pub register_vars: BTreeSet<VarID>,
    pub stack_vars: BTreeSet<VarID>,
    pub globals: BTreeSet<VarName>,
}

pub fn variables_in(mir: &MIR) -> VarsInMIR {
    let mut vars_in_mir = VarsInMIR::default();

    for line in &mir.lines {
        variables_in_line_inner(line, &mut vars_in_mir);
    }

    vars_in_mir.register_vars.retain(|register_var| !vars_in_mir.stack_vars.contains(register_var));

    vars_in_mir
}

pub fn variables_in_line(mline: &MLine) -> Vec<VarID> {
    let mut vars_in_mir = VarsInMIR::default();

    variables_in_line_inner(mline, &mut vars_in_mir);

    vars_in_mir.register_vars.into_iter().chain(vars_in_mir.stack_vars.into_iter()).collect()
}

fn variables_in_line_inner(mline: &MLine, vars: &mut VarsInMIR) {
    match mline {
        MLine::Set { r, .. } => variables_in_expr(r, vars),
        MLine::SetAddr { r, .. } => variables_in_addr_expr(r, vars),
        MLine::Store { l, val } => {
            variables_in_operand(val, vars);
            variables_in_addr(l, vars);
        }
        MLine::MemCpy { from, to, len } => {
            variables_in_addr(from, vars);
            variables_in_addr(to, vars);
            variables_in_operand(len, vars);
        },
        MLine::MemMove { from, to, len } => {
            variables_in_operand(from, vars);
            variables_in_operand(to, vars);
            variables_in_operand(len, vars);
        },
        MLine::Return(Some(on)) => variables_in_operand(on, vars),
        MLine::Expr(expr) => variables_in_expr(expr, vars),
        MLine::Branch { if_, .. } => variables_in_operand(if_, vars),
        MLine::Return(None)
        | MLine::Marker(_)
        | MLine::Goto(_) => {},
    }
}

fn variables_in_expr(mexpr: &MExpr, vars: &mut VarsInMIR) {
    match mexpr {
        MExpr::MemLoc(memloc) => variables_in_memloc(memloc, vars),
        MExpr::Addr(addr) => variables_in_addr(addr, vars),
        MExpr::BinOp { l, r, .. } => {
            variables_in_operand(l, vars);
            variables_in_operand(r, vars);
        },
        MExpr::UnarOp { hs, .. } => variables_in_operand(hs, vars),
        MExpr::Call { f, a, .. } => {
            variables_in_callable(f, vars);

            for arg in a { variables_in_operand(arg, vars); }
        },
        MExpr::Free { ptr } => variables_in_operand(ptr, vars),
        MExpr::Ref { on } => variables_in_addr(on, vars),
        MExpr::Deref { on, .. } => variables_in_memloc(on, vars),
        MExpr::Alloc { len } => variables_in_operand(len, vars),
        MExpr::Void => {},
    }
}

fn variables_in_operand(moperand: &MOperand, vars: &mut VarsInMIR) {
    match moperand {
        MOperand::Memloc(memloc) => variables_in_memloc(memloc, vars),
        MOperand::Lit(_) => {},
    }
}

fn variables_in_memloc(memloc: &MMemLoc, vars: &mut VarsInMIR) {
    match memloc {
        MMemLoc::Var(id) => { vars.register_vars.insert(*id); },
        MMemLoc::Global(name) => { vars.globals.insert(name.clone()); },
        MMemLoc::Reg(_) => {},
    }
}

fn variables_in_addr(addr: &MAddr, vars: &mut VarsInMIR) {
    match addr {
        MAddr::Var(id) => { vars.stack_vars.insert(*id); },
        MAddr::Reg(_) => {},
    }
}

fn variables_in_addr_expr(expr: &MAddrExpr, vars: &mut VarsInMIR) {
    match expr {
        MAddrExpr::Expr(expr) => variables_in_expr(expr, vars),
        MAddrExpr::Addr(addr) => variables_in_addr(addr, vars),
        MAddrExpr::Member { object, .. } => variables_in_addr(object, vars),
        MAddrExpr::Index { object, index, .. } => {
            variables_in_addr(object, vars);
            variables_in_operand(index, vars);
        },
    }
}

fn variables_in_callable(callable: &MCallable, vars: &mut VarsInMIR) {
    match callable {
        MCallable::FirstClass(memloc) => variables_in_memloc(memloc, vars),
        MCallable::Func(_) => {},
    }
}
