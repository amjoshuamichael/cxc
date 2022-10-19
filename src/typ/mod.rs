use crate::lex::VarName;

use std::collections::HashSet;
use std::fmt::{Debug, Formatter};
use std::hash::Hash;
use std::ops::Deref;
use std::sync::Arc;

use indexmap::IndexMap;

mod kind;
pub use kind::Kind;

#[derive(Clone, Hash, PartialEq, Eq)]
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

#[derive(Default, Hash, PartialEq, Eq)]
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

#[derive(PartialEq, Eq, Hash)]
pub struct RefType {
    base: Type,
}

#[derive(PartialEq, Eq, Hash)]
pub struct FuncType {
    pub return_type: Type,
    pub args: Vec<Type>,
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

impl Hash for StructType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for (name, typ) in &self.fields {
            name.hash(state);
            typ.hash(state);
        }

        for method in &self.methods {
            method.hash(state);
        }
    }
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

#[derive(PartialEq, Eq, Hash)]
pub struct NeverType();

pub static NEVER_STATIC: NeverType = NeverType();

#[derive(PartialEq, Hash, Eq)]
pub struct ArrayType {
    base: Type,
    count: u32,
}

impl ArrayType {
    pub fn base(&self) -> Type { self.base.clone() }
}
