use crate::{mir::{MLine, MExpr, MMemLoc, MAddr, MOperand, MCallable, MAddrExpr}, VarName};

// only gets variables that are loaded, not ones that are set. this data is required to 
// fill in cranelift-ir block parameters. also, this code does not remove duplicates.
//
// https://www.reddit.com/r/ProgrammingLanguages/comments/9z8qu3/a_new_compiler_backend_called_cranelift_uses/

#[derive(PartialEq, Eq, Hash)]
pub struct VarInMIR {
    pub name: VarName,
    // variables that are referenced need to be placed on the stack, and can't be 
    // allocated in cranelift registered
    pub is_referenced: bool,
}

pub fn variables_in(mline: &MLine) -> Vec<VarInMIR> {
    match mline {
        MLine::Set { r, .. } => variables_in_expr(r),
        MLine::SetAddr { r, .. } => variables_in_addr_expr(r),
        MLine::Store { val, .. } => variables_in_operand(val),
        MLine::MemCpy { from, to, .. } => {
            let from = variables_in_addr(from);
            let to = variables_in_addr(to);
            from.into_iter().chain(to.into_iter()).collect()
        },
        MLine::Return(Some(on)) => variables_in_operand(on),
        MLine::Return(None) => Vec::new(),
        MLine::Marker(_) => Vec::new(),
        MLine::Goto(_) => Vec::new(),
        MLine::Expr(expr) => variables_in_expr(expr),
        MLine::Branch { if_, .. } => variables_in_operand(if_),
    }
}

fn variables_in_expr(mexpr: &MExpr) -> Vec<VarInMIR> {
    match mexpr {
        MExpr::MemLoc(memloc) => variables_in_memloc(memloc),
        MExpr::Addr(addr) => variables_in_addr(addr),
        MExpr::BinOp { l, r, .. } => {
            let l = variables_in_operand(l);
            let r = variables_in_operand(r);
            l.into_iter().chain(r.into_iter()).collect()
        },
        MExpr::UnarOp { hs, .. } => variables_in_operand(hs),
        MExpr::Call { f, a, .. } => {
            let f = variables_in_callable(f);
            let a = a.into_iter().map(variables_in_operand).flatten();
            a.chain(f.into_iter()).collect()
        },
        MExpr::Ref { on } => variables_in_addr(on),
        MExpr::Deref { on, .. } => variables_in_operand(on),
        MExpr::Void => Vec::new(),
    }
}

fn variables_in_operand(moperand: &MOperand) -> Vec<VarInMIR> {
    match moperand {
        MOperand::MemLoc(memloc) => variables_in_memloc(memloc),
        MOperand::Lit(_) => Vec::new(),
    }
}

fn variables_in_memloc(memloc: &MMemLoc) -> Vec<VarInMIR> {
    match memloc {
        MMemLoc::Reg(_) => Vec::new(),
        MMemLoc::Var(name) => vec![VarInMIR {
            name: name.clone(),
            is_referenced: false,
        }],
    }
}

fn variables_in_addr(addr: &MAddr) -> Vec<VarInMIR> {
    match addr {
        MAddr::Reg(_) => Vec::new(),
        MAddr::Var(name) => vec![VarInMIR {
            name: name.clone(),
            is_referenced: true,
        }],
    }
}

fn variables_in_addr_expr(expr: &MAddrExpr) -> Vec<VarInMIR> {
    match expr {
        MAddrExpr::Expr(expr) => variables_in_expr(expr),
        MAddrExpr::Addr(addr) => variables_in_addr(addr),
        MAddrExpr::Member { object, .. } => variables_in_addr(object),
        MAddrExpr::Index { object, index, .. } => {
            let object = variables_in_addr(object);
            let index = variables_in_operand(index);
            object.into_iter().chain(index.into_iter()).collect()
        },
    }
}

fn variables_in_callable(callable: &MCallable) -> Vec<VarInMIR> {
    match callable {
        MCallable::Func(_) => Vec::new(),
        MCallable::FirstClass(memloc) => variables_in_memloc(memloc),
    }
}
