use crate::lex::{TypeName, VarName};
use memoize::memoize;

use std::fmt::{Debug, Formatter};
use std::hash::Hash;
use std::ops::Deref;
use std::sync::Arc;

mod kind;
mod size;
mod typ_or_alias;
pub use kind::Kind;
pub use typ_or_alias::TypeOrAlias;

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct Type(Arc<TypeData>);

impl Debug for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl Default for Type {
    fn default() -> Self { Type::never() }
}

impl Type {
    fn new(type_enum: TypeEnum) -> Self { Self(Arc::new(type_enum.into())) }

    pub fn size(&self) -> usize { size::size_of_type(self.clone()) }

    pub fn can_be_returned_directly(&self) -> bool {
        match self.clone().as_type_enum() {
            TypeEnum::Int(IntType { size }) => *size <= 64,
            TypeEnum::Ref(..) => true,
            TypeEnum::Float(_) => true,
            TypeEnum::Array(_) => false,
            _ => self.size() <= 8,
        }
    }

    pub fn ref_x_times(mut self, count: u8) -> Type {
        for _ in 0..count {
            self = self.get_ref();
        }

        self
    }

    pub fn complete_deref(mut self) -> Type {
        while let Some(derefed) = self.clone().get_deref() {
            self = derefed;
        }

        self
    }

    pub fn deref_x_times(self, count: u8) -> Option<Type> {
        let mut output = self.clone();

        for _ in 0..count {
            output = output.get_deref()?;
        }

        Some(output)
    }

    pub fn get_ref(self) -> Type { Type::new(TypeEnum::Ref(RefType { base: self })) }

    pub fn get_deref(self) -> Option<Type> {
        match &self.0.type_enum {
            TypeEnum::Ref(t) => Some(t.base.clone()),
            _ => None,
        }
    }

    pub fn get_array(self, count: u32) -> Type {
        Type::new(TypeEnum::Array(ArrayType { base: self, count }))
    }

    pub fn i(size: u32) -> Type { Type::new(TypeEnum::Int(IntType { size })) }

    pub fn f16() -> Type { Type::new(TypeEnum::Float(FloatType::F16)) }

    pub fn f32() -> Type { Type::new(TypeEnum::Float(FloatType::F32)) }

    pub fn f64() -> Type { Type::new(TypeEnum::Float(FloatType::F64)) }

    pub fn f(size: impl Into<FloatType>) -> Type {
        Type::new(TypeEnum::Float(size.into()))
    }

    pub fn bool() -> Type { Type::new(TypeEnum::Bool(BoolType)) }

    pub fn new_struct(fields: Vec<(VarName, Type)>, methods: Vec<VarName>) -> Type {
        Type::new(TypeEnum::Struct(StructType { fields, methods }))
    }

    pub fn never() -> Type { Type::new(TypeEnum::Never) }

    pub fn func_with_args(self, args: Vec<Type>) -> Type {
        Type::new(TypeEnum::Func(FuncType {
            ret_type: self,
            args,
        }))
    }

    pub fn as_type_enum<'a>(&self) -> &TypeEnum { &self.0.type_enum }

    pub fn name(&self) -> &Option<TypeName> { &self.0.name }

    // panics if there is more than one reference to the inner TypeData
    pub fn with_name(self, name: TypeName) -> Self {
        let mut type_data = Arc::try_unwrap(self.0).unwrap();
        type_data.name = Some(name);
        Self(Arc::from(type_data))
    }

    pub fn is_never(&self) -> bool { matches!(self.as_type_enum(), TypeEnum::Never) }
}

#[derive(Default, Hash, PartialEq, Eq)]
pub struct TypeData {
    type_enum: TypeEnum,
    name: Option<TypeName>,
}

impl Debug for TypeData {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), std::fmt::Error> {
        match &self.name {
            Some(name) => write!(fmt, "{} = {:?}", name, self.type_enum),
            None => write!(fmt, "{:?}", self.type_enum),
        }
    }
}

impl From<TypeEnum> for TypeData {
    fn from(type_enum: TypeEnum) -> Self {
        Self {
            type_enum,
            name: None,
        }
    }
}

impl TypeData {
    fn new(type_enum: TypeEnum, name: Option<TypeName>) -> Self {
        Self { type_enum, name }
    }

    pub fn is_named(&self) -> bool { self.name.is_some() }
}

#[derive(Default, Hash, PartialEq, Eq)]
pub enum TypeEnum {
    Int(IntType),
    Float(FloatType),
    Struct(StructType),
    Ref(RefType),
    Func(FuncType),
    Array(ArrayType),
    Bool(BoolType),

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
            TypeEnum::Bool(t) => t,
            TypeEnum::Never => &NEVER_STATIC,
        }
    }
}

#[derive(PartialEq, Eq, Hash)]
pub struct RefType {
    base: Type,
}

#[derive(PartialEq, Eq, Hash)]
pub struct FuncType {
    pub ret_type: Type,
    pub args: Vec<Type>,
}

impl FuncType {
    pub fn ret_type(&self) -> Type { self.ret_type.clone() }
    pub fn args(&self) -> Vec<Type> { self.args.clone() }
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub struct StructType {
    pub fields: Vec<(VarName, Type)>,
    pub methods: Vec<VarName>,
}

impl StructType {
    pub fn get_field_type(&self, field_name: &VarName) -> Option<Type> {
        for field in &self.fields {
            if field.0 == *field_name {
                return Some(field.1.clone());
            }
        }

        return None;
    }

    pub fn get_full_method_name(&self, field_name: &VarName) -> Option<&VarName> {
        self.methods.iter().find(|m| m == &field_name)
    }

    pub fn get_field_index(&self, field_name: &VarName) -> usize {
        self.fields
            .iter()
            .position(|field| field.0 == *field_name)
            .unwrap()
    }

    pub fn field_count(&self) -> usize { self.fields.len() }
}

#[derive(PartialEq, Eq, Hash)]
pub struct IntType {
    // when we need to support 2-billion-bit integers, we'll be ready
    pub size: u32,
}

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
pub enum FloatType {
    F16,
    F32,
    F64,
}

impl Into<FloatType> for u32 {
    fn into(self) -> FloatType {
        match self {
            16 => FloatType::F16,
            32 => FloatType::F32,
            64 => FloatType::F64,
            _ => panic!("{} is an invalid float size", self),
        }
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub struct BoolType;

#[derive(PartialEq, Eq, Hash)]
pub struct NeverType();

pub static NEVER_STATIC: NeverType = NeverType();

#[derive(PartialEq, Hash, Eq)]
pub struct ArrayType {
    pub base: Type,
    pub count: u32,
}