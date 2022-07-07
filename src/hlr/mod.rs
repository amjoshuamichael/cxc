pub mod expr_tree;
pub mod hlr_data;
mod maybe_type;
pub mod program_info;
#[cfg(test)]
mod tests;
pub mod type_group;
mod type_inference;

use crate::parse::prelude::*;
use inkwell::types::*;
use std::collections::HashSet;
use std::fmt::{Debug, Formatter};
use std::sync::Arc;

pub mod prelude {
    pub use super::{
        expr_tree::type_from_name, expr_tree::ExprID, expr_tree::ExprTree, expr_tree::GeneralReturnType,
        expr_tree::GeneralReturnType::*, expr_tree::NodeData, hlr, hlr_data::HLR, program_info::ProgramInfo,
        type_group::TypeGroup, type_inference::*, BaseType, Type,
    };
}

use prelude::*;

pub fn hlr(prog: Script) -> HLR {
    let func_as_expr = match &prog.0[0] {
        Declaration::Function { code, .. } => code,
        _ => unreachable!(),
    };

    let mut output = HLR::from(func_as_expr.clone());
    infer_types(&mut output);

    if crate::DEBUG {
        println!("--------HLR DATA--------");
        println!("{:?}", output.tree);
    }

    output
}

#[derive(Clone, Default)]
pub struct Type {
    base: Arc<BaseType>,
    pub ref_count: u8,
}

impl Type {
    fn name(&self) -> String {
        "&".repeat(self.ref_count.into()) + &self.base.name
    }

    pub fn gen_ret_type(&self) -> GeneralReturnType {
        pub fn type_from_name(input: &str) -> GeneralReturnType {
            match input {
                "prim::i8" | "prim::i16" | "prim::i32" | "prim::i64" => PrimInt,
                "prim::f8" | "prim::f16" | "prim::f32" | "prim::f64" => PrimFloat,
                s if s.chars().next() == Some('&') => PrimRef(Box::new(type_from_name(&input[1..]))),
                _ => todo!(),
            }
        }

        type_from_name(&self.name())
    }
}

impl Debug for Type {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), std::fmt::Error> {
        print!("Type({})", self.name());

        Ok(())
    }
}

impl From<&Arc<BaseType>> for Type {
    fn from(base_type: &Arc<BaseType>) -> Type {
        Type {
            base: base_type.clone(),
            ..Default::default()
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct BaseType {
    pub name: String,
    pub fields: HashSet<String>,
}

impl BaseType {
    pub fn new_prim(name: &str) -> Self {
        BaseType {
            name: String::from("prim::") + name.into(),
            ..Default::default()
        }
    }

    pub fn new_under(name: &str) -> Self {
        BaseType {
            name: String::from("_::") + name.into(),
            ..Default::default()
        }
    }
}
