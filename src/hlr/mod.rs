pub mod expr_tree;
pub mod hlr_data;
pub mod type_group;
mod type_inference;

use crate::parse::*;
use crate::unit::Globals;
use inkwell::context::Context;
use inkwell::types::*;
use inkwell::values::BasicValue;
use inkwell::AddressSpace;
use std::collections::HashSet;
use std::fmt::{Debug, Formatter};
use std::ops::Deref;
use std::sync::Arc;

pub mod prelude {
    pub use super::{
        expr_tree::ExprID, expr_tree::ExprTree, expr_tree::GenType,
        expr_tree::GenType::*, expr_tree::NodeData, hlr, hlr_data::FuncRep,
        type_group::TypeGroup, type_inference::*, StructType, Type, TypeEnum,
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
    dbg!(&output);

    if crate::DEBUG {
        println!("--------HLR DATA--------");
        println!("{:?}", output.tree);
    }

    output
}

pub trait Type {
    fn name(&self) -> String;
    fn gen_ret_type(&self) -> GenType;
    fn to_any_type<'t>(&self, context: &'t Context) -> AnyTypeEnum<'t>;
    fn as_type_enum(&self) -> TypeEnum;
}

#[derive(Clone, Default)]
pub enum TypeEnum {
    Int(Box<IntType>),
    Float(Box<FloatType>),
    Struct(Box<StructType>),
    Ref(Box<RefType>),
    Func(Box<FuncType>),

    #[default]
    Never,
}

impl Debug for TypeEnum {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(fmt, "Type({})", self.name());

        Ok(())
    }
}

impl TypeEnum {
    pub fn ref_x_times(&self, count: u8) -> TypeEnum {
        let mut output = self.clone();

        for _ in 0..count {
            output = output.get_ref();
        }

        output
    }

    pub fn deref_x_times(&self, count: u8) -> Option<TypeEnum> {
        let mut output = self.clone();

        for _ in 0..count {
            output = output.get_deref()?;
        }

        Some(output)
    }

    pub fn get_ref(&self) -> TypeEnum {
        TypeEnum::Ref(box RefType { base: self.clone() })
    }

    pub fn get_deref(&self) -> Option<TypeEnum> {
        match self {
            TypeEnum::Ref(t) => Some(t.base.as_type_enum()),
            _ => None,
        }
    }

    pub fn to_basic_type<'t>(&self, context: &'t Context) -> BasicTypeEnum<'t> {
        self.to_any_type(context).try_into().unwrap()
    }

    pub fn int_of_size(size: u32) -> TypeEnum {
        TypeEnum::Int(box IntType { size })
    }

    pub fn float_of_size(size: u32) -> TypeEnum {
        TypeEnum::Float(box FloatType { size })
    }

    pub fn func_with_args(&self, args: Vec<TypeEnum>) -> TypeEnum {
        FuncType {
            return_type: self.clone(),
            args: args.clone(),
        }
        .as_type_enum()
    }

    pub fn as_dyn(&self) -> Box<dyn Type> {
        match self {
            TypeEnum::Int(t) => t.clone(),
            TypeEnum::Float(t) => t.clone(),
            TypeEnum::Struct(t) => t.clone(),
            TypeEnum::Ref(t) => t.clone(),
            TypeEnum::Func(t) => t.clone(),
            _ => box NeverType(),
        }
    }
}

impl Type for TypeEnum {
    fn name(&self) -> String {
        self.as_dyn().name()
    }

    fn gen_ret_type(&self) -> GenType {
        self.as_dyn().gen_ret_type()
    }

    fn to_any_type<'t>(&self, context: &'t Context) -> AnyTypeEnum<'t> {
        self.as_dyn().to_any_type(context)
    }

    fn as_type_enum(&self) -> TypeEnum {
        self.as_dyn().as_type_enum()
    }
}

#[derive(Clone)]
pub struct RefType {
    base: TypeEnum,
}

impl Type for RefType {
    fn name(&self) -> String {
        "&".to_string() + &*self.base.name()
    }

    fn gen_ret_type(&self) -> GenType {
        PrimRef
    }

    fn to_any_type<'t>(&self, context: &'t Context) -> AnyTypeEnum<'t> {
        self.base
            .to_basic_type(context)
            .ptr_type(AddressSpace::Generic)
            .into()
    }

    fn as_type_enum(&self) -> TypeEnum {
        TypeEnum::Ref(box self.clone())
    }
}

#[derive(Clone)]
pub struct FuncType {
    pub return_type: TypeEnum,
    pub args: Vec<TypeEnum>,
}

impl Type for FuncType {
    fn name(&self) -> String {
        // TODO: make this good
        "a function".to_string()
        // let args_names = self.args.iter().map(|t| t.name()).sum();
        // let ret_name = self.return_type.name();

        // args_names + "->" + ret_name
    }

    fn gen_ret_type(&self) -> GenType {
        GenType::Func
    }

    fn to_any_type<'t>(&self, context: &'t Context) -> AnyTypeEnum<'t> {
        todo!()
    }

    fn as_type_enum(&self) -> TypeEnum {
        TypeEnum::Func(box self.clone())
    }
}

#[derive(Clone, Debug)]
pub struct StructType {
    pub fields: IndexMap<String, TypeEnum>,
}

impl StructType {
    pub fn get_field_type(&self, field_name: &String) -> TypeEnum {
        self.fields.get(field_name).unwrap().clone()
    }

    pub fn get_field_index(&self, field_name: &String) -> usize {
        self.fields.get_index_of(field_name).unwrap()
    }

    pub fn field_count(&self) -> usize {
        self.fields.len()
    }
}

impl Type for StructType {
    fn name(&self) -> String {
        // TODO: make this good lmao
        "TODO".to_string()
    }

    fn gen_ret_type(&self) -> GenType {
        GenType::Struct
    }

    fn to_any_type<'t>(&self, context: &'t Context) -> AnyTypeEnum<'t> {
        let field_types: Vec<BasicTypeEnum> = self
            .fields
            .iter()
            .map(|(_, typ)| typ.to_basic_type(context))
            .collect();

        context
            .struct_type(&field_types[..], true)
            .as_any_type_enum()
    }

    fn as_type_enum(&self) -> TypeEnum {
        TypeEnum::Struct(box self.clone())
    }
}

#[derive(Clone)]
pub struct IntType {
    // when we need to support 2-billion-bit integers, we'll be ready
    pub size: u32,
}

impl Type for IntType {
    fn name(&self) -> String {
        String::from("int")
    }

    fn gen_ret_type(&self) -> GenType {
        GenType::PrimInt
    }

    fn to_any_type<'t>(&self, context: &'t Context) -> AnyTypeEnum<'t> {
        context.custom_width_int_type(self.size).as_any_type_enum()
    }

    fn as_type_enum(&self) -> TypeEnum {
        TypeEnum::Int(box self.clone())
    }
}

#[derive(Clone)]
pub struct FloatType {
    // when we need to support 2-billion-bit integers, we'll be ready
    pub size: u32,
}

impl Type for FloatType {
    fn name(&self) -> String {
        String::from("todo")
    }

    fn gen_ret_type(&self) -> GenType {
        GenType::PrimFloat
    }

    fn to_any_type<'t>(&self, context: &'t Context) -> AnyTypeEnum<'t> {
        // TODO: implement different sized floats
        context.f32_type().as_any_type_enum()
    }

    fn as_type_enum(&self) -> TypeEnum {
        TypeEnum::Float(box self.clone())
    }
}

#[derive(Clone)]
pub struct NeverType();

impl Type for NeverType {
    fn name(&self) -> String {
        String::from("NEVER")
    }

    fn gen_ret_type(&self) -> GenType {
        panic!()
    }

    fn to_any_type<'t>(&self, context: &'t Context) -> AnyTypeEnum<'t> {
        panic!()
    }

    fn as_type_enum(&self) -> TypeEnum {
        TypeEnum::Never
    }
}
