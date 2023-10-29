use crate::cache::Cache;
use crate::errors::{TErr, TResult};
use crate::hlr::hlr_data_output::HLR;
use crate::lex::{TypeName, VarName};
use crate::parse::TypeSpec;
use crate::FuncQuery;
use crate::{CompData, TypeRelation};
use std::collections::hash_map::DefaultHasher;
use std::fmt::{Debug, Formatter};
use std::hash::{Hash, Hasher};
use std::ops::Deref;
use std::sync::Arc;

pub mod can_transform;
pub mod fields_iter;
pub mod spec_from_type;
mod kind;
mod nested_field_count;
mod size;
mod abi_styles;

use cxc_derive::XcReflect;

pub use kind::Kind;
pub use abi_styles::{ReturnStyle, realize_return_style, realize_arg_style, ArgStyle};
use crate::xc_opaque;

use crate as cxc;

#[derive(PartialEq, Eq, Hash, Clone, XcReflect)]
pub struct Type(Arc<TypeData>);

impl Debug for Type {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result { write!(f, "{:?}", self.0) }
}

impl Default for Type {
    fn default() -> Self { Type::unknown() }
}

impl Type {
    pub fn new(type_enum: TypeEnum) -> Self { Self(Arc::new(type_enum.into())) }

    pub fn size(&self) -> usize { 
        self.0.cached_size.retrieve_or_call(|| size::size_of_type(self.clone()) )
    }

    pub fn raw_return_type(&self, abi: ABI) -> Type {
        realize_return_style(self.return_style(abi), self)
    }

    pub fn raw_arg_type(&self, abi: ABI) -> Type {
        realize_arg_style(self.arg_style(abi), self)
    }

    pub fn complete_deref(mut self) -> Type {
        while let Some(derefed) = self.clone().get_deref() {
            self = derefed;
        }

        self
    }

    pub fn get_ref(&self) -> Type { Type::new(TypeEnum::Ref(RefType { base: self.clone() })) }

    pub fn get_deref(&self) -> Option<Type> {
        match self.remove_wrappers().as_type_enum() {
            TypeEnum::Ref(t) => Some(t.base.clone()),
            _ => None,
        }
    }

    pub fn get_array(self, count: u32) -> Type {
        Type::new(TypeEnum::Array(ArrayType { base: self, count }))
    }

    pub fn wrap(self) -> Type { Type::new_tuple(vec![self]) }

    pub fn as_u64(self) -> u64 { self.into_raw() as u64 }
    pub fn into_raw(self) -> *const TypeData { Arc::into_raw(self.0) }

    /// # Safety
    ///
    /// Ensure that data is a valid pointer to a TypeData
    pub unsafe fn from_raw(data: *const TypeData) -> Self { Self(Arc::from_raw(data)) }

    pub fn i(size: u32) -> Type { Type::new(TypeEnum::Int(IntType::new(size, true))) }
    pub fn u(size: u32) -> Type { Type::new(TypeEnum::Int(IntType::new(size, false))) }

    pub fn f16() -> Type { Type::new(TypeEnum::Float(FloatType::F16)) }

    pub fn f32() -> Type { Type::new(TypeEnum::Float(FloatType::F32)) }

    pub fn f64() -> Type { Type::new(TypeEnum::Float(FloatType::F64)) }

    pub fn f(size: impl Into<FloatType>) -> Type { Type::new(TypeEnum::Float(size.into())) }

    pub fn bool() -> Type { Type::new(TypeEnum::Bool) }

    pub fn empty() -> Type { Type::new_struct(Vec::new()) }

    pub fn new_struct(fields: Vec<Field>) -> Type {
        Type::new(TypeEnum::Struct(StructType {
            fields,
            repr: Repr::Rust,
        }))
    }

    pub fn new_tuple(fields: Vec<Type>) -> Type {
        let indexed_fields = fields
            .into_iter()
            .enumerate()
            .map(|(i, field)| Field { 
                name: VarName::TupleIndex(i), 
                typ: field, 
                inherited: false 
            })
            .collect();

        Type::new(TypeEnum::Struct(StructType {
            fields: indexed_fields,
            repr: Repr::Rust,
        }))
    }

    pub fn unknown() -> Type { Type::new(TypeEnum::Unknown) }

    pub fn void() -> Type { Type::new(TypeEnum::Void) }
    pub fn void_ptr() -> Type { Type::new(TypeEnum::Void).get_ref() }

    pub fn func_with_args(self, args: Vec<Type>, abi: ABI) -> Type {
        Type::new(TypeEnum::Func(FuncType { ret: self, args, abi }))
    }

    pub fn remove_wrappers(&self) -> &Type {
        match self.as_type_enum() {
            TypeEnum::Destructor(DestructorType { base, .. }) => base,
            _ => self
        }
    }

    pub fn as_type_enum(&self) -> &TypeEnum { &self.0.type_enum }
    pub fn clone_type_enum(&self) -> TypeEnum { self.0.type_enum.clone() }

    pub fn name(&self) -> &TypeName { &self.0.name }
    pub fn generics(&self) -> &Vec<Type> { &self.0.generics }

    pub(crate) fn with_name(self, name: TypeName) -> Self {
        self.modify_type_data(|data| data.name = name.clone())
    }

    pub(crate) fn with_generics(self, generics: &[Type]) -> Self {
        self.modify_type_data(|data| data.generics = generics.to_owned())
    }

    pub fn modify_type_data(self, function: impl FnOnce(&mut TypeData)) -> Self {
        match Arc::try_unwrap(self.0) {
            Ok(mut type_data) => {
                function(&mut type_data);
                Self(Arc::from(type_data))
            },
            Err(arc) => {
                let mut type_data = (*arc).clone();
                function(&mut type_data);
                Self(Arc::from(type_data))
            },
        }
    }

    pub fn is_unknown(&self) -> bool { matches!(self.as_type_enum(), TypeEnum::Unknown) }
    pub fn is_known(&self) -> bool { !self.is_unknown() }
    pub fn is_void(&self) -> bool { matches!(self.as_type_enum(), TypeEnum::Void) }
    pub fn is_float(&self) -> bool { matches!(self.as_type_enum(), TypeEnum::Float { .. }) }
    pub fn is_empty(&self) -> bool { self == &Type::empty() }

    pub fn repr(&self) -> Repr {
        match self.as_type_enum() {
            TypeEnum::Struct(StructType { repr, .. }) => *repr,
            _ => Repr::Rust,
        }
    }

    pub fn is_primitive(&self) -> bool {
        !matches!(
            self.as_type_enum(), 
            TypeEnum::Struct(_) | 
            TypeEnum::Array(_)
        )
    }

    pub fn is_ref(&self) -> bool {
        matches!(self.as_type_enum(), TypeEnum::Ref(_))
    }

    pub fn full_name(&self) -> String { format!("{self:?}") }
}

#[derive(PartialEq, Eq, Default, Hash, Clone, XcReflect)]
#[xc_opaque]
pub struct TypeData {
    pub type_enum: TypeEnum,
    pub name: TypeName,
    pub generics: Vec<Type>,
    pub cached_size: Cache<usize>,
}

impl Debug for TypeData {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), std::fmt::Error> {
        match &self.name {
            TypeName::Other(name) => {
                write!(fmt, "{}", name)?;

                let mut generics = self.generics.iter();

                if let Some(generic) = generics.next() {
                    write!(fmt, "<{:?}", generic)?;

                    for generic in generics {
                        write!(fmt, "{:?}, ", generic)?;
                    }

                    write!(fmt, ">")?;
                }

                Ok(())
            },
            _ => write!(fmt, "{:?}", self.type_enum),
        }
    }
}

impl From<TypeEnum> for TypeData {
    fn from(type_enum: TypeEnum) -> Self {
        Self {
            type_enum,
            ..Default::default()
        }
    }
}

impl TypeData {
    pub fn is_named(&self) -> bool { self.name != TypeName::Anonymous }
}

#[derive(PartialEq, Eq, Default, Hash, Clone, XcReflect)]
pub enum TypeEnum {
    Int(IntType),
    Float(FloatType),
    Struct(StructType),
    Ref(RefType),
    Func(FuncType),
    Array(ArrayType),
    Destructor(DestructorType),
    Bool,
    Void,

    #[default]
    Unknown,
}

impl Debug for TypeEnum {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(fmt, "{}", self.to_string())
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
            TypeEnum::Destructor(t) => t,
            TypeEnum::Bool => &BoolType,
            TypeEnum::Void => &VOID_STATIC,
            TypeEnum::Unknown => &UNKNOWN_STATIC,
        }
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Debug, XcReflect)]
pub struct RefType {
    pub base: Type,
}

/// The ABI of a function, similar to the possible strings after rust's `extern` keyword.
/// See: https://doc.rust-lang.org/beta/reference/items/functions.html#extern-function-qualifier
#[derive(Copy, PartialEq, Eq, Hash, Clone, PartialOrd, Ord, Debug, XcReflect)]
pub enum ABI {
    /// Works like extern "Rust".
    Rust,
    /// Works like extern "C"– happens to be the cxc default when declaring functions.
    C,
    /// No ABI, arguments are passed in as-is. This is used in function types when they
    /// have moved past the ABI steps in the compiler. It is very unsafe to use in 
    /// practice.
    None,
}

#[derive(PartialEq, Eq, Hash, Clone, Debug, XcReflect)]
pub struct FuncType {
    pub ret: Type,
    pub args: Vec<Type>,
    pub abi: ABI,
}

#[derive(PartialEq, Eq, Hash, Copy, Clone, Default, Debug, XcReflect)]
pub enum Repr {
    #[default]
    Rust,
    C,
    Transparent,
}

#[derive(PartialEq, Eq, Hash, Debug, Default, Clone, XcReflect)]
pub struct StructType {
    pub fields: Vec<Field>,
    pub repr: Repr,
}

#[derive(PartialEq, Eq, Hash, Debug, Default, Clone, XcReflect)]
pub struct Field {
    pub name: VarName,
    pub typ: Type,
    pub inherited: bool,
}

impl StructType {
    pub fn get_field_type(&self, field_name: &VarName) -> TResult<Type> {
        for field in &self.fields {
            if field.name == *field_name {
                return Ok(field.typ.clone());
            }
        }

        Err(TErr::FieldNotFound(TypeEnum::Struct(self.clone()), field_name.clone()))
    }

    pub fn get_field_index(&self, field_name: &VarName) -> TResult<usize> {
        self.fields
            .iter()
            .position(|field| field.name == *field_name)
            .ok_or(TErr::FieldNotFound(TypeEnum::Struct(self.clone()), field_name.clone()))
    }

    pub fn is_tuple(&self) -> bool { self.fields.is_empty() || matches!(self.fields[0].name, VarName::TupleIndex(0)) }

    pub fn largest_field(&self) -> Option<Type> {
        if self.fields.len() == 0 {
            return None
        }

        let mut types = self.fields.iter().map(|Field { typ, .. }| typ).collect::<Vec<_>>();
        types.sort_by(|a, b| b.size().cmp(&a.size()));

        Some(types[0].clone())
    }

    pub fn has_field(&self, check_name: &VarName) -> bool {
        self.fields.iter().any(|Field { name, .. }| check_name == name)
    }
}

#[derive(Copy, Debug, PartialEq, Eq, Hash, Clone, PartialOrd, Ord, XcReflect)]
pub struct IntType {
    pub size: IntSize,
    pub signed: bool,
}

#[derive(Copy, Debug, PartialEq, Eq, Hash, Clone, PartialOrd, Ord, XcReflect)]
pub enum IntSize {
    _8,
    _16,
    _32,
    _64,
    _128,
}

impl IntSize {
    pub const fn to_num(self) -> usize {
        match self {
            IntSize::_8 => 8,
            IntSize::_16 => 16,
            IntSize::_32 => 32,
            IntSize::_64 => 64,
            IntSize::_128 => 128,
        }
    }
}

impl IntType {
    pub fn new(size: u32, signed: bool) -> Self {
        let size = match size {
            8 => IntSize::_8,
            16 => IntSize::_16,
            32 => IntSize::_32,
            64 => IntSize::_64,
            128 => IntSize::_128,
            _ => panic!("{size} is an invalid int size"),
        };

        Self { size, signed }
    }
}

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy, PartialOrd, Ord, XcReflect)]
pub enum FloatType {
    F16,
    F32,
    F64,
}

impl From<u32> for FloatType {
    fn from(size: u32) -> FloatType {
        match size {
            16 => FloatType::F16,
            32 => FloatType::F32,
            64 => FloatType::F64,
            _ => panic!("{size} is an invalid float size"),
        }
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Copy, XcReflect)]
pub struct BoolType;

#[derive(PartialEq, Eq, Hash, Clone, XcReflect)]
pub struct UnknownType();
static UNKNOWN_STATIC: UnknownType = UnknownType();

#[derive(PartialEq, Eq, Hash, Clone, XcReflect)]
pub struct VoidType();
static VOID_STATIC: VoidType = VoidType();

#[derive(PartialEq, Hash, Eq, Clone, XcReflect)]
pub struct ArrayType {
    pub base: Type,
    pub count: u32,
}

#[derive(Clone, XcReflect)]
pub struct DestructorType {
    pub base: Type,
    pub destructor: Arc<HLR>,
}

impl PartialEq for DestructorType {
    fn eq(&self, other: &Self) -> bool {
        self.base == other.base &&
        self.destructor.from == other.destructor.from
    }
}

impl Eq for DestructorType {}

impl Hash for DestructorType {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.base.hash(state);
        self.destructor.from.hash(state);
    }
}
