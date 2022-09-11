pub mod expr_tree;
pub mod hlr_data;
pub mod type_group;
mod type_inference;

use crate::lex::VarName;
use crate::parse::*;
use crate::unit::Functions;
use inkwell::context::Context;
use inkwell::types::AnyType;
use inkwell::types::AnyTypeEnum;
use inkwell::types::BasicMetadataTypeEnum;
use inkwell::types::BasicType;
use inkwell::types::BasicTypeEnum;
use inkwell::AddressSpace;
use std::collections::HashSet;
use std::fmt::{Debug, Formatter};
use std::ops::Deref;
use std::sync::Arc;

pub mod prelude {
    pub use super::{
        expr_tree::ExprID, expr_tree::ExprTree, expr_tree::NodeData, hlr,
        hlr_data::FuncRep, type_group::TypeGroup, type_inference::*, Kind,
        StructType, Type, TypeEnum,
    };
}

use indexmap::IndexMap;
use prelude::*;

pub fn hlr(
    args: Vec<VarDecl>,
    code: Expr,
    globals: &Functions,
    types: &TypeGroup,
    generics: Vec<TypeAlias>,
) -> FuncRep {
    let mut output = FuncRep::from(args, code, types, generics);
    infer_types(&mut output, globals);

    if crate::DEBUG {
        dbg!(&output);
    }

    output
}

#[derive(Clone, PartialEq, Eq)]
pub struct Type(Arc<TypeEnum>);

impl Debug for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl Type {
    fn new(type_enum: TypeEnum) -> Self { Self(Arc::new(type_enum)) }

    pub fn ref_x_times(mut self, count: u8) -> Type {
        for _ in 0..count {
            self = self.get_ref();
        }

        self
    }

    pub fn complete_deref(self) -> Type {
        let mut output = self;

        while let Some(derefed) = output.clone().get_deref() {
            output = derefed;
        }

        output
    }

    pub fn deref_x_times(self, count: u8) -> Option<Type> {
        let mut output = self.clone();

        for _ in 0..count {
            output = output.get_deref()?;
        }

        Some(output)
    }

    pub fn get_ref(self) -> Type {
        Type(Arc::new(TypeEnum::Ref(RefType { base: self })))
    }

    pub fn get_deref(self) -> Option<Type> {
        match &*self.0 {
            TypeEnum::Ref(t) => Some(t.base.clone()),
            _ => None,
        }
    }

    pub fn get_array(self, count: u32) -> Type {
        Type(Arc::new(TypeEnum::Array(ArrayType { base: self, count })))
    }

    pub fn to_basic_type<'t>(&self, context: &'t Context) -> BasicTypeEnum<'t> {
        self.to_any_type(context).try_into().unwrap()
    }

    pub fn i(size: u32) -> Type { Type::new(TypeEnum::Int(IntType { size })) }

    pub fn f16() -> Type { Type::new(TypeEnum::Float(FloatType::F16)) }

    pub fn f32() -> Type { Type::new(TypeEnum::Float(FloatType::F32)) }

    pub fn f64() -> Type { Type::new(TypeEnum::Float(FloatType::F64)) }

    pub fn f(size: FloatType) -> Type { Type::new(TypeEnum::Float(size)) }

    pub fn new_struct(
        fields: IndexMap<VarName, Type>,
        methods: HashSet<VarName>,
    ) -> Type {
        Type(Arc::new(TypeEnum::Struct(StructType { fields, methods })))
    }

    pub fn never() -> Type { Type(Arc::new(TypeEnum::Never)) }

    pub fn func_with_args(self, args: Vec<Type>) -> Type {
        Type(Arc::new(TypeEnum::Func(FuncType {
            return_type: self,
            args,
        })))
    }

    pub fn as_type_enum<'a>(&self) -> &TypeEnum {
        let output = &*self.0;
        output
    }

    pub fn is_never(&self) -> bool { matches!(self.as_type_enum(), TypeEnum::Never) }
}

impl Kind for Type {
    fn name(&self) -> String { self.as_type_enum().name() }

    fn to_any_type<'t>(&self, context: &'t Context) -> AnyTypeEnum<'t> {
        self.as_type_enum().to_any_type(context)
    }
}

pub trait Kind {
    fn name(&self) -> String;
    fn to_any_type<'t>(&self, context: &'t Context) -> AnyTypeEnum<'t>;
}

#[derive(Default, PartialEq, Eq)]
pub enum TypeEnum {
    Int(IntType),
    Float(FloatType),
    Struct(StructType),
    Ref(RefType),
    Func(FuncType),
    Array(ArrayType),

    #[default]
    Never,
}

impl Debug for TypeEnum {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(fmt, "{}", self.name())
    }
}

impl Deref for TypeEnum {
    type Target = dyn Kind;

    fn deref(&self) -> &Self::Target {
        match self {
            TypeEnum::Int(t) => t,
            TypeEnum::Float(t) => t,
            TypeEnum::Func(t) => t,
            TypeEnum::Struct(t) => t,
            TypeEnum::Ref(t) => t,
            TypeEnum::Array(t) => t,
            TypeEnum::Never => &NEVER_STATIC,
        }
    }
}

#[derive(PartialEq, Eq)]
pub struct RefType {
    base: Type,
}

impl Kind for RefType {
    fn name(&self) -> String { "&".to_string() + &*self.base.name() }

    fn to_any_type<'t>(&self, context: &'t Context) -> AnyTypeEnum<'t> {
        self.base
            .to_basic_type(context)
            .ptr_type(AddressSpace::Generic)
            .into()
    }
}

#[derive(PartialEq, Eq)]
pub struct FuncType {
    pub return_type: Type,
    pub args: Vec<Type>,
}

impl Kind for FuncType {
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

#[derive(PartialEq, Eq)]
pub struct StructType {
    pub fields: IndexMap<VarName, Type>,
    pub methods: HashSet<VarName>,
}

impl StructType {
    pub fn get_field_type(&self, field_name: &VarName) -> Option<Type> {
        self.fields.get(field_name).cloned()
    }

    pub fn get_full_method_name(&self, field_name: &VarName) -> Option<&VarName> {
        self.methods.iter().find(|m| m == &field_name)
    }

    pub fn get_field_index(&self, field_name: &VarName) -> usize {
        self.fields.get_index_of(field_name).unwrap()
    }

    pub fn field_count(&self) -> usize { self.fields.len() }
}

impl Kind for StructType {
    fn name(&self) -> String {
        // the indexmap crate formats a struct like this:
        // { x: i32, y: i32, }
        //
        // to this:
        // { "x": i32, "y": i32, }
        //
        // here, we remove the quotes.
        let struct_with_quotes = format!("{:?} | {:?}", self.fields, self.methods);

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

#[derive(PartialEq, Eq)]
pub struct IntType {
    // when we need to support 2-billion-bit integers, we'll be ready
    pub size: u32,
}

impl Kind for IntType {
    fn name(&self) -> String { format!("i{}", self.size) }

    fn to_any_type<'t>(&self, context: &'t Context) -> AnyTypeEnum<'t> {
        context.custom_width_int_type(self.size).as_any_type_enum()
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum FloatType {
    F16,
    F32,
    F64,
}

impl Kind for FloatType {
    fn name(&self) -> String {
        match self {
            FloatType::F16 => "f16",
            FloatType::F32 => "f32",
            FloatType::F64 => "f64",
        }
        .to_string()
    }

    fn to_any_type<'t>(&self, context: &'t Context) -> AnyTypeEnum<'t> {
        match self {
            FloatType::F16 => context.f16_type().as_any_type_enum(),
            FloatType::F32 => context.f32_type().as_any_type_enum(),
            FloatType::F64 => context.f64_type().as_any_type_enum(),
        }
    }
}

#[derive(PartialEq, Eq)]
pub struct NeverType();

pub static NEVER_STATIC: NeverType = NeverType();

impl Kind for NeverType {
    fn name(&self) -> String { String::from("NEVER") }

    fn to_any_type<'t>(&self, context: &'t Context) -> AnyTypeEnum<'t> {
        context.i32_type().as_any_type_enum()
    }
}

#[derive(PartialEq, Eq)]
pub struct ArrayType {
    base: Type,
    count: u32,
}

impl Kind for ArrayType {
    fn name(&self) -> String { format!("[{:?}; {}]", self.base, self.count) }

    fn to_any_type<'t>(&self, context: &'t Context) -> AnyTypeEnum<'t> {
        self.base
            .to_basic_type(context)
            .array_type(self.count)
            .as_any_type_enum()
    }
}

impl ArrayType {
    pub fn base(&self) -> Type { self.base.clone() }
}
