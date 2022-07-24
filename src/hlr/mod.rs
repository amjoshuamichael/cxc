pub mod expr_tree;
pub mod hlr_data;
pub mod type_group;
mod type_inference;

use crate::core_lib::CORE_LIB;
use crate::parse::*;
use crate::unit::Globals;
use inkwell::context::Context;
use inkwell::types::*;
use inkwell::values::BasicValue;
use inkwell::AddressSpace;
use std::collections::HashSet;
use std::fmt::{Debug, Formatter};
use std::lazy::Lazy;
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

pub fn hlr(
    args: Vec<VarDecl>,
    code: Expr,
    globals: &Globals,
    types: &TypeGroup,
) -> FuncRep {
    let mut output = FuncRep::from(args, code, types);
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
        match &*self.name() {
            "prim:i8" | "prim:i16" | "prim:i32" | "prim:i64" => PrimInt,
            "prim:f8" | "prim:f16" | "prim:f32" | "prim:f64" => PrimFloat,
            s if s[0..1] == *"&" => PrimRef,
            _ => Struct,
        }
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
        CORE_LIB.get_spec(&TypeSpec::new("_:none", 0)).unwrap()
    }

    pub fn func_with_args(&self, args: Vec<Type>) -> Self {
        Self {
            base: self.base.clone(),
            ref_count: self.ref_count,
            function_args: Some(args),
        }
    }

    pub fn to_basic_type<'t>(&self, context: &'t Context) -> BasicTypeEnum<'t> {
        self.to_any_type(context).try_into().unwrap()
    }

    pub fn to_any_type<'t>(&self, context: &'t Context) -> AnyTypeEnum<'t> {
        match self.gen_ret_type() {
            PrimInt => context.i32_type().into(),
            PrimFloat => context.f32_type().into(),
            PrimRef => {
                let mut pointed_to_type = self.clone();
                pointed_to_type.ref_count = 0;
                pointed_to_type
                    .to_basic_type(context)
                    .ptr_type(AddressSpace::Generic)
                    .into()
            },
            Struct => {
                let field_types: Vec<BasicTypeEnum> = self
                    .base
                    .fields
                    .iter()
                    .map(|(_, typ)| typ.to_basic_type(context))
                    .collect();

                context
                    .struct_type(&field_types[..], true)
                    .as_any_type_enum()
            },
            _ => todo!(),
        }
    }

    pub fn get_field_type(&self, field_name: &String) -> &Type {
        if !matches!(self.gen_ret_type(), Struct) {
            panic!("object is not struct");
        }

        self.base.fields.get(field_name).unwrap()
    }

    pub fn get_field_index(&self, field_name: &String) -> usize {
        if !matches!(self.gen_ret_type(), Struct) {
            panic!("object is not struct");
        }

        self.base.fields.get_index_of(field_name).unwrap()
    }

    pub fn field_count(&self) -> usize {
        if !matches!(self.gen_ret_type(), Struct) {
            panic!("object is not struct");
        }

        self.base.fields.len()
    }
}

impl Debug for Type {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(fmt, "Type({})", self.name());

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
            name: String::from("prim:") + name.into(),
            ..Default::default()
        }
    }

    pub fn new_under(name: &str) -> Self {
        BaseType {
            name: String::from("_:") + name.into(),
            ..Default::default()
        }
    }

    pub fn new_struct(
        name: String,
        fields: impl Iterator<Item = (String, Type)>,
    ) -> Self {
        BaseType {
            name,
            fields: fields.collect(),
        }
    }
}
