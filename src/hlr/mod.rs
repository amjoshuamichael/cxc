pub mod expr_tree;
pub mod hlr_data;
pub mod type_group;
mod type_inference;

use crate::core_lib::CORE_LIB;
use crate::parse::*;
use crate::unit::Globals;
use std::collections::HashSet;
use std::fmt::{Debug, Formatter};
use std::sync::Arc;

pub mod prelude {
    pub use super::{
        expr_tree::ExprID, expr_tree::ExprTree, expr_tree::GeneralReturnType,
        expr_tree::GeneralReturnType::*, expr_tree::NodeData, hlr,
        hlr_data::FuncRep, type_group::TypeGroup, type_inference::*, BaseType, Type,
    };
}

use indexmap::IndexMap;
use prelude::*;

pub fn hlr(args: Vec<VarDecl>, code: Expr, globals: &Globals) -> FuncRep {
    let mut output = FuncRep::from(args, code);
    dbg!(&output);
    infer_types(&mut output, globals);

    if crate::DEBUG {
        println!("--------HLR DATA--------");
        println!("{:?}", output.tree);
    }

    output
}

#[derive(Clone)]
pub struct Type {
    base: Arc<BaseType>,
    pub ref_count: u8,
    pub function_args: Option<Vec<Type>>,
}

impl Type {
    pub fn name(&self) -> String {
        "&".repeat(self.ref_count.into()) + &self.base.name
    }

    pub fn gen_ret_type(&self) -> GeneralReturnType {
        pub fn type_from_name(input: &str) -> GeneralReturnType {
            match input {
                "prim::i8" | "prim::i16" | "prim::i32" | "prim::i64" => PrimInt,
                "prim::f8" | "prim::f16" | "prim::f32" | "prim::f64" => PrimFloat,
                s if s[0..1] == *"&" => PrimRef,
                _ => panic!("could not find type: {input}"),
            }
        }

        type_from_name(&self.name())
    }

    pub fn func_ret_type(&self) -> Self {
        assert!(self.function_args.is_some());

        Self {
            base: self.base.clone(),
            ref_count: self.ref_count,
            function_args: None,
        }
    }

    fn none() -> Self {
        CORE_LIB.get_spec(&TypeSpec::new("_::none", 0)).unwrap()
    }

    pub fn func_with_args(&self, args: Vec<Type>) -> Self {
        Self {
            base: self.base.clone(),
            ref_count: self.ref_count,
            function_args: Some(args),
        }
    }
}

impl Debug for Type {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), std::fmt::Error> {
        print!("Type({})", self.name());

        Ok(())
    }
}

#[derive(Clone, Debug, Default)]
pub struct BaseType {
    pub name: String,
    pub fields: IndexMap<String, Type>,
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
