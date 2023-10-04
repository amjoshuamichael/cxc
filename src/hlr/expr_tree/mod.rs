use slotmap::{new_key_type, SlotMap};
use HNodeData::*;

use crate::lex::{TypeName, VarName};
use crate::typ::can_transform::TransformationList;
use crate::{parse::*, FuncQuery};
use crate::Type;
use crate::unit::Global;
use super::hlr_data::{VarID, FuncRep, GotoLabelID, VariableInfo};

use std::collections::{HashSet, HashMap};
use std::fmt::{Debug, Formatter};

mod expr_tree_helper;
mod quick;

pub use quick::*;

#[derive(Default, Clone)]
pub struct ExprTree {
    pub root: ExprID,
    pub nodes: SlotMap<ExprID, ExprNode>,
}

impl Debug for ExprTree {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), std::fmt::Error> {
        writeln!(fmt)?;
        for expr in &self.nodes {
            writeln!(
                fmt,
                "{:?} <- {:?} : {:?} : {:?}",
                expr.0,
                expr.1.parent,
                expr.1.data,
                expr.1.data.ret_type(),
            )?
        }

        Ok(())
    }
}

new_key_type! {
    pub struct ExprID;
}

impl ExprID { 
    pub fn oprhaned() -> Self { Self::default() } 
}

#[derive(Clone)]
pub struct ExprNode {
    parent: ExprID,
    pub data: HNodeData,
}

impl ExprNode {
    #[allow(dead_code)]
    fn to_string(&self, variables: &SlotMap<VarID, super::hlr_data::VariableInfo>) -> String {
        let code = match &self.data {
            Number { value, .. } => format!("{value}"),
            Float { value, .. } => format!("{value:?}"),
            Bool { value, .. } => format!("{value:?}"),
            StructLit { var_type, fields, .. } => format!("{var_type:?} {{ {fields:?} }}"),
            ArrayLit { parts, .. } => format!("{parts:?}"),
            Call { query, a, .. } => format!("{:?}({:?})", query.name, a),
            IndirectCall { f, a, .. } => format!("{f:?}({a:?})"),
            Ident { var_id: id, .. } => format!("{}", variables[*id].name),
            AccessAlias(name) => format!("{name}"),
            GotoLabel(name) => format!(":{name:?}"),
            Goto(name) => format!("{name}"),
            GlobalLoad { global, .. } => format!("global({global:?})"),
            Set { lhs, rhs, .. } => format!("{lhs:?} = {rhs:?}"),
            Member { object, field, .. } => format!("{object:?}.{field}"),
            Index { object, index, .. } => format!("{object:?}[{index:?}]"),
            UnarOp { op, hs, .. } => format!("{op:?} {hs:?}"),
            Transform { hs, .. } => format!("+{hs:?}"),
            BinOp { lhs, op, rhs, .. } => format!("{lhs:?} {op:?} {rhs:?}"),
            IfThen { i, t, .. } => format!("? {i:?} {t:?}"),
            IfThenElse { i, t, e, .. } => {
                format!("? {i:?} {t:?} : {e:?}")
            },
            While { w, d, .. } => format!("@ {w:?} {d:?}"),
            Block { stmts, .. } => format!("{{{stmts:?}}}"),
            Return { to_return, .. } => format!("! {to_return:?}"),
        };

        format!("{code} :: {:?}", self.data.ret_type())
    }
}

#[derive(Clone, Debug)]
pub enum HNodeData {
    Number {
        lit_type: Type,
        value: u64,
    },
    Float {
        lit_type: Type,
        value: f64,
    },
    Bool {
        value: bool,
    },
    StructLit {
        var_type: Type,
        fields: Vec<(VarName, ExprID)>,
        initialize: InitOpts,
    },
    ArrayLit {
        var_type: Type,
        parts: Vec<ExprID>,
        initialize: InitOpts,
    },
    Ident {
        var_type: Type,
        var_id: VarID,
    },
    AccessAlias(VarName),
    GotoLabel(GotoLabelID),
    Goto(VarName),
    GlobalLoad {
        var_type: Type,
        global: Global,
    },
    Set {
        lhs: ExprID,
        rhs: ExprID,
    },
    Call {
        ret_type: Type,
        query: FuncQuery,
        a: Vec<ExprID>,
        sret: Option<ExprID>,
    },
    IndirectCall {
        ret_type: Type,
        f: ExprID,
        a: Vec<ExprID>,
        sret: Option<ExprID>,
    },
    Member {
        ret_type: Type,
        object: ExprID,
        field: VarName,
    },
    Index {
        ret_type: Type,
        object: ExprID,
        index: ExprID,
    },
    UnarOp {
        ret_type: Type,
        op: Opcode,
        hs: ExprID,
    },
    Transform {
        hs: ExprID,
        ret_type: Type,
        steps: Option<TransformationList>,
    },
    BinOp {
        ret_type: Type,
        lhs: ExprID,
        op: Opcode,
        rhs: ExprID,
    },
    IfThen {
        ret_type: Type,
        i: ExprID,
        t: ExprID,
    },
    IfThenElse {
        ret_type: Type,
        i: ExprID,
        t: ExprID,
        e: ExprID,
    },
    While {
        w: ExprID,
        d: ExprID,
    },
    Block {
        ret_type: Type,
        stmts: Vec<ExprID>,
        declared: HashSet<VarID>,
        aliases: HashMap<VarName, ExprID>,
        withs: HashSet<ExprID>,
        goto_labels: HashMap<VarName, GotoLabelID>,
    },
    Return {
        ret_type: Type,
        to_return: Option<ExprID>,
    },
}

impl HNodeData {
    pub fn ret_type(&self) -> Type {
        match self {
            Number { lit_type, .. } | Float { lit_type, .. } => lit_type.clone(),
            Bool { .. } => Type::bool(),
            While { .. } | Set { .. } | GotoLabel { .. } | Goto { .. } => Type::void(),
            AccessAlias { .. } => Type::unknown(),
            Ident { var_type, .. }
            | GlobalLoad { var_type, .. }
            | StructLit { var_type, .. }
            | ArrayLit { var_type, .. } => var_type.clone(),
            BinOp { ret_type, .. }
            | Return { ret_type, .. }
            | UnarOp { ret_type, .. }
            | Transform { ret_type, .. }
            | IfThen { ret_type, .. }
            | IfThenElse { ret_type, .. }
            | Call { ret_type, .. }
            | IndirectCall { ret_type, .. }
            | Block { ret_type, .. }
            | Index { ret_type, .. }
            | Member { ret_type, .. } => ret_type.clone(),
        }
    }

    pub fn ret_type_mut(&mut self) -> Option<&mut Type> {
        match self {
            Number { ref mut lit_type, .. }
            | Float { ref mut lit_type, .. } => Some(lit_type),
            Bool { .. } 
            | While { .. } 
            | Set { .. } 
            | AccessAlias { .. } 
            | GotoLabel { .. } 
            | Goto { .. } => None,
            Ident { ref mut var_type, .. }
            | GlobalLoad { ref mut var_type, .. }
            | StructLit { ref mut var_type, .. }
            | ArrayLit { ref mut var_type, .. } => Some(var_type),
            BinOp { ref mut ret_type, .. }
            | Return { ref mut ret_type, .. }
            | UnarOp { ref mut ret_type, .. }
            | Transform { ref mut ret_type, .. }
            | IfThen { ref mut ret_type, .. }
            | IfThenElse { ref mut ret_type, .. }
            | Call { ref mut ret_type, .. }
            | IndirectCall { ref mut ret_type, .. }
            | Block { ref mut ret_type, .. }
            | Index { ref mut ret_type, .. }
            | Member { ref mut ret_type, .. } => Some(ret_type),
        }
    }

    pub fn to_string(
        &self, 
        tree: &ExprTree, 
        variables: &SlotMap<VarID, VariableInfo>,
    ) -> String {
        const RED: &str = "\x1b[91m";
        const GREEN: &str = "\x1b[92m";
        const YELLOW: &str = "\x1b[93m";
        const BLUE: &str = "\x1b[94m";
        const MAGENTA: &str = "\x1b[95m";
        const WHITE: &str = "\x1b[37m";
        match self {
            Number { value, lit_type } => format!("{MAGENTA}{value}{lit_type:?}{WHITE}"),
            Float { value, lit_type } => format!("{MAGENTA}{value}{lit_type:?}{WHITE}"),
            Bool { value, .. } => format!("{MAGENTA}{value}{WHITE}"),
            Ident { var_id: id, .. } => {
                if let VarName::Other(name) = &variables[*id].name {
                    format!("{BLUE}{name}{WHITE}")
                } else {
                    format!("{BLUE}{id:?}{WHITE}")
                }
            }
            AccessAlias(name) => format!("{BLUE}{name}{WHITE}"),
            GotoLabel(name) => format!("{YELLOW}:{name:?}{WHITE}"),
            Goto(name) => format!("{RED};{YELLOW}:{name}{WHITE}"),
            GlobalLoad { global, .. } => format!("{BLUE}({global:?}){WHITE}"),
            StructLit {
                var_type,
                fields,
                initialize,
            } => {
                let mut lit = WHITE.to_string();

                lit += &*match var_type.name() {
                    TypeName::Anonymous => format!("{var_type:?}"),
                    other => other.to_string(),
                };

                lit += " { \n";

                for field in fields.iter() {
                    lit += BLUE;
                    lit += &*field.0.to_string();
                    lit += WHITE;
                    lit += " = ";
                    lit += &*tree.get(field.1).to_string(tree, variables);
                    lit += WHITE;
                    lit += "\n";
                }

                match initialize {
                    InitOpts::Default => lit += "++ \n",
                    InitOpts::Uninit => lit += "-- \n",
                    InitOpts::NoFill => {},
                }

                lit = lit.replace("\n", "\n    ");
                lit += "} \n";

                lit
            },
            ArrayLit {
                parts, initialize, ..
            } => {
                let mut lit = "[".into();

                for (p, part) in parts.iter().enumerate() {
                    if p > 0 {
                        lit += ", ";
                    }
                    lit += &*tree.get(*part).to_string(tree, variables);
                }

                match initialize {
                    InitOpts::Default => lit += ", ++ ",
                    InitOpts::Uninit => lit += ", -- ",
                    InitOpts::NoFill => {},
                }

                lit += "]";

                lit
            },
            Set { lhs, rhs, .. } => {
                let mut lit = tree.get(*lhs).to_string(tree, variables);
                lit += " = ";
                lit += &*tree.get(*rhs).to_string(tree, variables);
                lit
            },
            Call {
                a: args,
                query,
                sret,
                ..
            } => {
                let mut call = match &query.relation {
                    TypeRelation::Static(typ) => format!("{typ:?}") + "::",
                    TypeRelation::MethodOf(typ) => format!("({typ:?})") + ".",
                    TypeRelation::Unrelated => String::default(),
                };

                call += GREEN;
                call += &*query.name;

                if !query.generics.is_empty() {
                    call += WHITE;
                    call += "<";
                    for (g, generic) in query.generics.iter().enumerate() {
                        if g > 0 {
                            call += ", ";
                        }
                        call += &*format!("{:?}", generic);
                    }
                    call += WHITE;
                    call += ">";
                }

                call += "(";

                if let Some(sret) = sret {
                    call += "-> ";
                    call += &*tree.get(*sret).to_string(tree, variables);
                    call += " | ";
                }

                for (a, arg) in args.iter().enumerate() {
                    if a > 0 {
                        call += ", ";
                    }
                    call += &*tree.get(*arg).to_string(tree, variables);
                    call += WHITE;
                }

                call += ")";
                call
            },
            IndirectCall { f, a: args, .. } => {
                let mut call = "(".into();
                call += &*tree.get(*f).to_string(tree, variables);
                call += ")";

                call += "(";
                for (a, arg) in args.iter().enumerate() {
                    if a > 0 {
                        call += ", ";
                    }
                    call += &*tree.get(*arg).to_string(tree, variables);
                }
                call += ")";
                call
            },
            Member { object, field, .. } => {
                let mut member = WHITE.to_string();
                member += "(";
                member += &*tree.get(*object).to_string(tree, variables);
                member += WHITE;
                member += ")";
                member += ".";
                member += BLUE;
                member += &*field.to_string();
                member += WHITE;
                member
            },
            Index { object, index, .. } => {
                let mut object = tree.get(*object).to_string(tree, variables);
                object += "[";
                object += &*tree.get(*index).to_string(tree, variables);
                object += "]";
                object
            },
            UnarOp { op, hs, .. } => {
                YELLOW.to_string() + &*op.to_string() + &*tree.get(*hs).to_string(tree, variables)
            },
            Transform { hs, .. } => {
                YELLOW.to_string() + "+" + &*tree.get(*hs).to_string(tree, variables)
            },
            BinOp { lhs, op, rhs, .. } => {
                let mut binop = tree.get(*lhs).to_string(tree, variables);
                binop += " ";
                binop += &*op.to_string();
                binop += " ";
                binop + &*tree.get(*rhs).to_string(tree, variables)
            },
            IfThen { i, t, .. } => {
                let mut it = RED.to_string() + "? ".into();
                it += WHITE;
                it += &*tree.get(*i).to_string(tree, variables);
                it += " ";
                it += WHITE;
                it += &*tree.get(*t).to_string(tree, variables);
                it += WHITE;
                it
            },
            IfThenElse { i, t, e, .. } => {
                let mut ite = RED.to_string() + "? ".into();
                ite += WHITE;
                ite += &*tree.get(*i).to_string(tree, variables);
                ite += " ";
                ite += WHITE;
                ite += &*tree.get(*t).to_string(tree, variables);
                ite += RED;
                ite += " : ";
                ite += WHITE;
                ite += &*tree.get(*e).to_string(tree, variables);
                ite += WHITE;
                ite
            },
            While { w, d, .. } => {
                let mut wh = "@ ".into();
                wh += &*tree.get(*w).to_string(tree, variables);
                wh += " ";
                wh += &*tree.get(*d).to_string(tree, variables);
                wh
            },
            Block { stmts, .. } => {
                let mut bl: String = "{".into();
                for stmt in stmts.iter() {
                    bl += "\n";
                    bl += &*tree.get(*stmt).to_string(tree, variables);
                    bl += WHITE;
                }
                bl = bl.replace("\n", "\n    ");
                bl += "\n}";
                bl += WHITE;
                bl
            },
            Return { to_return, .. } => {
                let mut ret = RED.to_string() + "; " + WHITE;
                if let Some(to_return) = to_return {
                    ret += &*tree.get(*to_return).to_string(tree, variables);
                }
                ret
            },
        }
    }

    pub fn void() -> Self {
        HNodeData::StructLit {
            var_type: Type::empty(),
            fields: Vec::new(),
            initialize: InitOpts::NoFill,
        }
    }

    pub fn new_block() -> Self {
        HNodeData::Block {
            ret_type: Type::void(),
            stmts: Vec::new(),
            declared: HashSet::new(),
            aliases: HashMap::new(),
            withs: HashSet::new(),
            goto_labels: HashMap::new(),
        }
    }
}
