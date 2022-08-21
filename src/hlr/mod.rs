pub mod expr_tree;
pub mod hlr_data;
pub mod type_group;
mod type_inference;

use crate::parse::*;
use crate::unit::Globals;
use inkwell::context::Context;
use inkwell::types::AnyType;
use inkwell::types::AnyTypeEnum;
use inkwell::types::BasicMetadataTypeEnum;
use inkwell::types::BasicType;
use inkwell::types::BasicTypeEnum;
use inkwell::values::BasicMetadataValueEnum;
use inkwell::values::BasicValue;
use inkwell::AddressSpace;
use std::collections::HashSet;
use std::fmt::{Debug, Formatter};
use std::ops::Deref;
use std::sync::Arc;

pub mod prelude {
    pub use super::{
        expr_tree::ExprID, expr_tree::ExprTree, expr_tree::NodeData, hlr,
        hlr_data::FuncRep, type_group::TypeGroup, type_inference::*, StructType,
        Type as TypeTrait, TypeArc as Type, TypeEnum,
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
    infer_types(&mut output, globals);

    if crate::DEBUG {
        dbg!(&output);
    }

    output
}

#[derive(Clone)]
pub struct TypeArc(Arc<TypeEnum>);

impl Debug for TypeArc {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl TypeArc {
    pub fn ref_x_times(mut self, count: u8) -> TypeArc {
        for _ in 0..count {
            self = self.get_ref();
        }

        self
    }

    pub fn deref_x_times(self, count: u8) -> Option<TypeArc> {
        let mut output = self.clone();

        for _ in 0..count {
            output = output.get_deref()?;
        }

        Some(output)
    }

    pub fn get_ref(self) -> TypeArc {
        TypeArc(Arc::new(TypeEnum::Ref(RefType { base: self })))
    }

    pub fn get_deref(self) -> Option<TypeArc> {
        match &*self.0 {
            TypeEnum::Ref(t) => Some(t.base.clone()),
            _ => None,
        }
    }

    pub fn to_basic_type<'t>(&self, context: &'t Context) -> BasicTypeEnum<'t> {
        self.to_any_type(context).try_into().unwrap()
    }

    pub fn int_of_size(size: u32) -> TypeArc {
        TypeArc(Arc::new(TypeEnum::Int(IntType { size })))
    }

    pub fn float_of_size(size: u32) -> TypeArc {
        TypeArc(Arc::new(TypeEnum::Float(FloatType { size })))
    }

    pub fn new_struct(fields: IndexMap<String, TypeArc>) -> TypeArc {
        TypeArc(Arc::new(TypeEnum::Struct(StructType { fields })))
    }

    pub fn never() -> TypeArc {
        TypeArc(Arc::new(TypeEnum::Never))
    }

    pub fn func_with_args(self, args: Vec<TypeArc>) -> TypeArc {
        TypeArc(Arc::new(TypeEnum::Func(FuncType {
            return_type: self,
            args,
        })))
    }

    pub fn as_type_enum<'a>(&self) -> &TypeEnum {
        let output = &*self.0;
        output
    }
}

impl Type for TypeArc {
    fn name(&self) -> String {
        self.as_type_enum().name()
    }

    fn to_any_type<'t>(&self, context: &'t Context) -> AnyTypeEnum<'t> {
        self.as_type_enum().to_any_type(context)
    }
}

pub trait Type {
    fn name(&self) -> String;
    fn to_any_type<'t>(&self, context: &'t Context) -> AnyTypeEnum<'t>;
}

#[derive(Default)]
pub enum TypeEnum {
    Int(IntType),
    Float(FloatType),
    Struct(StructType),
    Ref(RefType),
    Func(FuncType),

    #[default]
    Never,
}

impl Debug for TypeEnum {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(fmt, "{}", self.name());

        Ok(())
    }
}

impl Deref for TypeEnum {
    type Target = dyn Type;

    fn deref(&self) -> &Self::Target {
        match self {
            TypeEnum::Int(t) => t,
            TypeEnum::Float(t) => t,
            TypeEnum::Func(t) => t,
            TypeEnum::Struct(t) => t,
            TypeEnum::Ref(t) => t,
            _ => &NEVER_STATIC,
        }
    }
}

pub struct RefType {
    base: TypeArc,
}

impl Type for RefType {
    fn name(&self) -> String {
        "&".to_string() + &*self.base.name()
    }

    fn to_any_type<'t>(&self, context: &'t Context) -> AnyTypeEnum<'t> {
        self.base
            .to_basic_type(context)
            .ptr_type(AddressSpace::Generic)
            .into()
    }
}

pub struct FuncType {
    pub return_type: TypeArc,
    pub args: Vec<TypeArc>,
}

impl Type for FuncType {
    fn name(&self) -> String {
        let args_names: String = self
            .args
            .iter()
            .map(|t| t.name())
            .collect::<Vec<String>>()
            .join(", ");
        let ret_name = self.return_type.name();

        format!("({args_names}) -> {ret_name}")
    }

    fn to_any_type<'t>(&self, context: &'t Context) -> AnyTypeEnum<'t> {
        let return_type = self.return_type.to_basic_type(context);
        let args: Vec<BasicMetadataTypeEnum> = self
            .args
            .iter()
            .map(|t| t.to_basic_type(context).into())
            .collect();

        return_type.fn_type(&args[..], true).try_into().unwrap()
    }
}

pub struct StructType {
    pub fields: IndexMap<String, TypeArc>,
}

impl StructType {
    pub fn get_field_type(&self, field_name: &String) -> TypeArc {
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
        // the indexmap crate formats a struct like this:
        // { x: i32, y: i32, }
        //
        // to this:
        // { "x": i32, "y": i32, }
        //
        // here, we remove the quotes.
        let struct_with_quotes = format!("{:?}", self.fields);

        struct_with_quotes.chars().filter(|c| *c != '"').collect()
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
}

pub struct IntType {
    // when we need to support 2-billion-bit integers, we'll be ready
    pub size: u32,
}

impl Type for IntType {
    fn name(&self) -> String {
        format!("i{}", self.size)
    }

    fn to_any_type<'t>(&self, context: &'t Context) -> AnyTypeEnum<'t> {
        context.custom_width_int_type(self.size).as_any_type_enum()
    }
}

pub struct FloatType {
    pub size: u32,
}

impl Type for FloatType {
    fn name(&self) -> String {
        format!("f{}", self.size)
    }

    fn to_any_type<'t>(&self, context: &'t Context) -> AnyTypeEnum<'t> {
        // TODO: implement different sized floats
        context.f32_type().as_any_type_enum()
    }
}

pub struct NeverType();

pub static NEVER_STATIC: NeverType = NeverType();

impl Type for NeverType {
    fn name(&self) -> String {
        String::from("NEVER")
    }

    fn to_any_type<'t>(&self, context: &'t Context) -> AnyTypeEnum<'t> {
        panic!()
    }
}
