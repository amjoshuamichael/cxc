use crate::lex::{TypeName, VarName};

use std::fmt::{Debug, Formatter};
use std::hash::Hash;
use std::ops::Deref;
use std::sync::Arc;

mod kind;
mod nested_field_count;
mod size;
mod typ_or_alias;
use inkwell::types::FunctionType;
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

#[derive(PartialEq, Eq, Debug)]
pub enum ReturnStyle {
    Direct,
    ThroughI64,
    ThroughI64I32,
    ThroughI64I64,
    Pointer,
    Void,
}

impl Type {
    fn new(type_enum: TypeEnum) -> Self { Self(Arc::new(type_enum.into())) }

    pub fn size(&self) -> usize { size::size_of_type(self.clone()) }

    pub fn nested_field_count(&self) -> usize {
        let mut total = 0;

        match self.as_type_enum() {
            TypeEnum::Struct(struct_type) => {
                for (_, field_type) in &struct_type.fields {
                    total += field_type.nested_field_count()
                }
            },
            TypeEnum::Array(array_type) => {
                total +=
                    array_type.count as usize * array_type.base.nested_field_count();
            },
            _ => total += 1,
        }

        total
    }

    pub fn return_style(&self) -> ReturnStyle {
        match self.as_type_enum() {
            TypeEnum::Int(_)
            | TypeEnum::Ref(_)
            | TypeEnum::Float(_)
            | TypeEnum::Bool(_)
            | TypeEnum::Func(_) => ReturnStyle::Direct,
            TypeEnum::Struct(_) | TypeEnum::Array(_) => {
                if self.size() > 16 {
                    ReturnStyle::Pointer
                } else if self.nested_field_count() == 1 {
                    ReturnStyle::Direct
                } else if self.size() == 16 {
                    ReturnStyle::ThroughI64I64
                } else if self.size() == 12 {
                    ReturnStyle::ThroughI64I32
                } else if self.size() == 8 {
                    ReturnStyle::ThroughI64
                } else if self.size() == 8 {
                    ReturnStyle::Direct
                } else {
                    todo!("cannot return style for this type: {:?}", self);
                }
            },
            TypeEnum::Never => ReturnStyle::Void,
            _ => todo!("cannot return style for this type: {:?}", self),
        }
    }

    pub fn raw_return_type(&self) -> Type {
        match self.return_style() {
            ReturnStyle::ThroughI64 => Type::i(64),
            ReturnStyle::ThroughI64I32 => Type::new_struct(vec![
                (VarName::from("ret_0"), Type::i(64)),
                (VarName::from("ret_1"), Type::i(32)),
            ]),
            ReturnStyle::ThroughI64I64 => Type::new_struct(vec![
                (VarName::from("ret_0"), Type::i(64)),
                (VarName::from("ret_1"), Type::i(64)),
            ]),
            ReturnStyle::Pointer | ReturnStyle::Void => Type::never(),
            ReturnStyle::Direct => self.clone(),
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

    pub fn get_ref(&self) -> Type {
        Type::new(TypeEnum::Ref(RefType { base: self.clone() }))
    }

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

    pub fn new_struct(fields: Vec<(VarName, Type)>) -> Type {
        Type::new(TypeEnum::Struct(StructType { fields }))
    }

    pub fn opaque_with_size(size: u32, name: &str) -> Type {
        Type::new(TypeEnum::Opaque(OpaqueType { size }))
            .with_name(TypeName::from(name))
    }

    pub fn opaque_type<T>() -> Type {
        let full_name = std::any::type_name::<T>();
        let scope_end_position = full_name.rfind(":").unwrap();
        let basic_name = &full_name[scope_end_position..];

        Type::new(TypeEnum::Opaque(OpaqueType {
            size: std::mem::size_of::<T>() as u32,
        }))
        .with_name(TypeName::from(basic_name))
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
    Opaque(OpaqueType),
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
            TypeEnum::Opaque(t) => t,
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

#[derive(PartialEq, Hash, Eq)]
pub struct OpaqueType {
    pub size: u32,
}
