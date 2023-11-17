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

pub use kind::TypeEnumVariant;
pub use abi_styles::{ReturnStyle, realize_return_style, realize_arg_style, ArgStyle};
use crate::xc_opaque;

use crate as cxc;

/// One of the most important structs in the compiler, [`Type`] holds a data type.
/// 
/// Cxc's structural type system is reflected in its compiler design. Instead of holding
/// a type id, or the name of a type, this object holds everything needed to describe and
/// analyze the contents of the type, including the types behind references. Internally,
/// [`Type`] holds a `Arc<TypeData>`, which contains a [`TypeName`], generics, and a 
/// [`TypeEnum`]. `TypeEnum` holds the actual type information–the integer sizes, the struct 
/// fields, etc. `TypeEnum`s containing variants like [`StructType`] and [`FuncType`] hold 
/// their inner data, like fields and arguments, behind `Type` objects as well. 
///
/// In this way, data types stretch out into an immutable network of `Type`s, each pointing 
/// to each other, making type inspection trivial. For instance, if you wanted to find the 
/// first argument of a `FuncType`, you would do it like this:
/// 
/// ```ignore
/// let typ = /* something ... */;
/// let FuncType { args, .. } = type.as_type_enum() else { panic!("not a func!") };
/// let first_arg = args[0].clone();
/// ```
///
/// This has minimal overhead. If you wanted to place the `first_arg` somewhere else,
/// calling `Type::func_with_args` would not clone the inner types, and would instead
/// create new references. Because they are just `Arc`s, `Type`s are 8 bytes wide, and 
/// cloning has very little overhead.
#[derive(PartialEq, Eq, Hash, Clone, Default, XcReflect)]
pub struct Type(Arc<TypeData>);

impl Debug for Type {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result { write!(f, "{}", self.0.full_name()) }
}

impl Type {
    /// Moves a [`TypeEnum`] into a new [`Type`].
    pub fn new(type_enum: TypeEnum) -> Self { 
        Self(Arc::new(TypeData {
            type_enum,
            ..Default::default()
        })) 
    }

    /// Calculates the size of the [`Type`], in bytes. The inner [`TypeData`] caches the size
    /// once it has already been calculated, so this should be a pretty low-overhead call.
    pub fn size(&self) -> usize { 
        self.0.cached_size.retrieve_or_call(|| size::size_of_type(self.clone()) )
    }

    /// Creates a new [`Type`], containing a reference to this type.
    pub fn get_ref(&self) -> Type { Type::new(TypeEnum::Ref(RefType { base: self.clone() })) }

    /// If this is holding a [`RefType`], returns a clone of the type that is being pointed
    /// to. Otherwise, returns [`None`].
    pub fn get_deref(&self) -> Option<Type> {
        match self.as_type_enum() {
            TypeEnum::Ref(t) => Some(t.base.clone()),
            _ => None,
        }
    }

    /// Creates a new [`Type`], containing an [`ArrayType`] with this type as a base, and
    /// a given count as the array length.
    pub fn get_array(self, count: u32) -> Type {
        Type::new(TypeEnum::Array(ArrayType { base: self, count }))
    }

    /// Crates a signed int type with the given `size`.
    /// # Panics
    /// - Panics if the size is not a valid [`IntSize`].
    pub fn i(size: u32) -> Type { Type::new(TypeEnum::Int(IntType::new(size, true))) }
    /// Crates an unsigned int type with the given `size`.
    /// # Panics
    /// - Panics if the size is not a valid [`IntSize`].
    pub fn u(size: u32) -> Type { Type::new(TypeEnum::Int(IntType::new(size, false))) }

    /// Creates an `f32` type.
    pub fn f32() -> Type { Type::new(TypeEnum::Float(FloatType::F32)) }

    /// Creates an `f64` type.
    pub fn f64() -> Type { Type::new(TypeEnum::Float(FloatType::F64)) }

    /// Creates a floating point type using the given [`FloatType`].
    pub fn f(size: impl Into<FloatType>) -> Type { Type::new(TypeEnum::Float(size.into())) }

    /// Creates a `bool` type.
    pub fn bool() -> Type { Type::new(TypeEnum::Bool) }

    /// Creates a new struct, with the given [`Field`]s.
    pub fn new_struct(fields: Vec<Field>) -> Type {
        Type::new(TypeEnum::Struct(StructType {
            fields,
            repr: Repr::default(),
        }))
    }

    /// Creates a new union, with the given [`Field`]s.
    pub fn new_union(fields: Vec<Field>) -> Type {
        Type::new(TypeEnum::Union(UnionType {
            fields,
            repr: Repr::default(),
        }))
    }

    /// Creates a new empty struct type, with no fields.
    pub fn empty_struct() -> Type { Type::new_struct(Vec::new()) }

    /// Creates a new tuple, with the given [`Type`]s. None of the fields are inherited.
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
            repr: Repr::default(),
        }))
    }

    /// Creates a new unknown type. (See: [`UnknownType`])
    pub fn unknown() -> Type { Type::new(TypeEnum::Unknown) }

    /// Creates a new void type. (See: [`UnknownType`])
    pub fn void() -> Type { Type::new(TypeEnum::Void) }

    /// Creates a new function type, with `self` as the return type, and a given set of
    /// arguments.
    pub fn func_with_args(self, args: Vec<Type>, abi: ABI) -> Type {
        Type::new(TypeEnum::Func(FuncType { ret: self, args, abi }))
    }

    /// Derefs the type to a non-reference by repeatedly calling [`Type::get_deref`].
    pub fn complete_deref(mut self) -> Type {
        while let Some(derefed) = self.clone().get_deref() {
            self = derefed;
        }

        self
    }

    /// Gets the inner type behind any wrappers. For example, this converts `&i32 ~ 
    /// free(self)` to `&i32`. Works with [`DestructorType`]s. 
    ///
    /// This does not remove the wrappers on individual fields, so calling this on 
    /// `{ x: &i32 ~ free(self) }` will return `{ x: &i32 ~ free(self) }`, *not* 
    /// `{ x: &i32 }`.
    ///
    /// This also works recursively, so if you happen to create a type like `&i32 ~ 
    /// free(self) ~ free(self)`, calling `remove_wrappers` on it will convert it to 
    /// `&i32`.
    pub fn remove_wrappers(&self) -> &Type {
        match self.as_type_enum() {
            TypeEnum::Destructor(DestructorType { base, .. }) => base.remove_wrappers(),
            _ => self,
        }
    }

    /// Gets the inner TypeEnum of a type, as a referenece.
    pub fn as_type_enum(&self) -> &TypeEnum { &self.0.type_enum }
    /// Gets the inner TypeEnum of a type, as a clone.
    pub fn clone_type_enum(&self) -> TypeEnum { self.0.type_enum.clone() }

    /// Gets the type's inner [`TypeName`].
    pub fn name(&self) -> &TypeName { &self.0.name }
    /// Gets the type's inner generics, which it will have if it was created using a type
    /// declaration with generics.
    pub fn generics(&self) -> &Vec<Type> { &self.0.generics }

    pub(crate) fn with_name(self, name: TypeName) -> Self {
        self.modify_type_data(|data| data.name = name.clone())
    }

    pub(crate) fn with_generics(self, generics: &[Type]) -> Self {
        self.modify_type_data(|data| data.generics = generics.to_owned())
    }

    pub(crate) fn modify_type_data(self, function: impl FnOnce(&mut TypeData)) -> Self {
        match Arc::try_unwrap(self.0) {
            Ok(mut type_data) => {
                function(&mut type_data);
                type_data.cached_hash.clear();
                type_data.cached_size.clear();
                Self(Arc::from(type_data))
            },
            Err(arc) => {
                let mut type_data = (*arc).clone();
                function(&mut type_data);
                Self(Arc::from(type_data))
            },
        }
    }

    /// Checks if the type is an [`UnknownType`].
    pub fn is_unknown(&self) -> bool { matches!(self.as_type_enum(), TypeEnum::Unknown) }
    /// Checks if the type is not an [`UnknownType`].
    pub fn is_known(&self) -> bool { !self.is_unknown() }
    /// Checks if the type is a [`VoidType`].
    pub fn is_void(&self) -> bool { matches!(self.as_type_enum(), TypeEnum::Void) }
    /// Checks if the type is a [`FloatType`].
    pub fn is_float(&self) -> bool { matches!(self.as_type_enum(), TypeEnum::Float { .. }) }
    /// Checks if the type is a [`RefType`].
    pub fn is_ref(&self) -> bool { matches!(self.as_type_enum(), TypeEnum::Ref(_)) }

    /// Checks if the type is an empty struct.
    pub fn repr(&self) -> Repr {
        match self.as_type_enum() {
            TypeEnum::Struct(StructType { repr, .. }) => *repr,
            _ => Repr::default(),
        }
    }
}

#[derive(PartialEq, Eq, Default, Clone, XcReflect)]
#[xc_opaque]
pub(crate) struct TypeData {
    pub type_enum: TypeEnum,
    pub name: TypeName,
    pub generics: Vec<Type>,
    pub cached_size: Cache<usize>,
    pub cached_hash: Cache<u64>,
}

impl Hash for TypeData {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let mini_hash = self.cached_hash.retrieve_or_call(|| {
            let mut hasher = ahash::AHasher::default();
            self.type_enum.hash(&mut hasher);
            self.name.hash(&mut hasher);
            self.generics.hash(&mut hasher);
            hasher.finish()
        });

        mini_hash.hash(state)
    }
}

impl TypeData {
    fn full_name(&self) -> String {
        match &self.name {
            TypeName::Other(name) => {
                let mut output = name.to_string();

                if !self.generics.is_empty() {
                    for (g, generic) in self.generics.iter().enumerate() {
                        let generic_full_name = generic.full_name();

                        output += &*if g == 0 {
                                format!("<{generic_full_name}")
                            } else if g == self.generics.len() - 1 {
                                format!("{generic_full_name}>")
                            } else {
                                format!("{generic_full_name}, ")
                            };
                    }

                    output += ">";
                }

                output
            },
            _ => self.type_enum.full_name(),
        }
    }
}

#[derive(PartialEq, Eq, Default, Hash, Clone, XcReflect)]
/// The inner data of a type. This exists as a wrapper over the various types that
/// implement [`TypeEnumVariant`]. You can access a given [`Type`]'s inner TypeEnum using
/// [`Type::as_type_enum`] and [`Type::clone_type_enum`].
pub enum TypeEnum {
    Int(IntType),
    Float(FloatType),
    Struct(StructType),
    Union(UnionType),
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
        write!(fmt, "{}", self.full_name())
    }
}

impl Deref for TypeEnum {
    type Target = dyn TypeEnumVariant;

    fn deref(&self) -> &Self::Target {
        match self {
            TypeEnum::Int(t) => t,
            TypeEnum::Float(t) => t,
            TypeEnum::Func(t) => t,
            TypeEnum::Struct(t) => t,
            TypeEnum::Union(t) => t,
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
/// A reference data type.
pub struct RefType {
    pub base: Type,
}

/// The ABI of a function, similar to the possible strings after rust's `extern` keyword.

#[derive(Copy, PartialEq, Eq, Hash, Clone, PartialOrd, Ord, Debug, XcReflect)]
pub enum ABI {
    /// Works like extern "Rust".
    /// See [the Rust documentation](https://doc.rust-lang.org/beta/reference/items/functions.html#extern-function-qualifier) for more explanation.
    Rust,
    /// Works like extern "C". also happens to be the current cxc default when declaring 
    /// functions. See [the Rust documentation](https://doc.rust-lang.org/beta/reference/items/functions.html#extern-function-qualifier) for more explanation.
    C,
    /// No ABI, arguments are passed in as-is. This is used in function types when they
    /// have moved past the ABI steps in the compiler. It is very unsafe to use in 
    /// practice.
    // TODO: remove
    None,
}

#[derive(PartialEq, Eq, Hash, Clone, Debug, XcReflect)]
/// A function data type. This is used both for the value of a first class function, and
/// to represent the specified argument and return types for a declared and compiled 
/// function.
pub struct FuncType {
    pub ret: Type,
    pub args: Vec<Type>,
    pub abi: ABI,
}

#[derive(PartialEq, Eq, Hash, Copy, Clone, Default, Debug, XcReflect)]
/// The way that the data of a struct is organized. This currently has no effect.
pub enum Repr {
    #[default]
    Rust,
    C,
    Transparent,
}

#[derive(PartialEq, Eq, Hash, Debug, Default, Clone, XcReflect)]
/// The individual field of a [StructType].
pub struct Field {
    pub name: VarName,
    pub typ: Type,
    pub inherited: bool,
}

#[derive(PartialEq, Eq, Hash, Debug, Default, Clone, XcReflect)]
/// A struct data type, including the [Repr].
pub struct StructType {
    pub fields: Vec<Field>,
    pub repr: Repr,
}

impl StructType {
    /// For a field at a given index, returns the offset in bytes. For instance, calling
    /// `.field_offset_in_bytes(2)` on struct `{ u32, f32, i8 }` would return 8.
    pub fn field_offset_in_bytes(&self, field_index: usize) -> usize {
        let mut size_sum: usize = 0;

        for field in &self.fields[0..field_index] {
            let size = field.typ.size();
            let field_alignment = size::size_of_largest_field_in(&field.typ);

            size_sum = size_sum.next_multiple_of(field_alignment);
            size_sum += size;
        }

        let last_field_alignment = 
            size::size_of_largest_field_in(&self.fields[field_index].typ);

        size_sum.next_multiple_of(last_field_alignment)
    }

    /// Gets the index of a particular field, given a [`VarName`]. Returns [`None`] if the 
    /// field doesn't exist.
    pub fn get_field_index(&self, field_name: &VarName) -> Option<usize> {
        self.fields
            .iter()
            .position(|field| field.name == *field_name)
    }

    /// Checks if this Struct is a tuple by analyzing its field names. Here, the empty 
    /// struct classifies as a tuple.
    pub fn is_tuple(&self) -> bool {
        self.fields.is_empty() || 
            matches!(self.fields[0].name, VarName::TupleIndex(0))
    }

    /// Checks if this field exists on this [`StructType`].
    pub fn has_field(&self, check_name: &VarName) -> bool {
        self.fields.iter().any(|Field { name, .. }| check_name == name)
    }
}

#[derive(PartialEq, Eq, Hash, Debug, Default, Clone, XcReflect)]
/// A union data type, including the [Repr].
pub struct UnionType {
    pub fields: Vec<Field>,
    pub repr: Repr,
}

impl UnionType {
    pub fn largest_field(&self) -> Option<&Type> {
        self.fields.iter().map(|field| &field.typ).max_by_key(|typ| typ.size())
    }
}

/// An integer data type, wrapping [`IntSize`].
#[derive(Copy, Debug, PartialEq, Eq, Hash, Clone, PartialOrd, Ord, XcReflect)]
pub struct IntType {
    pub size: IntSize,
    pub signed: bool,
}

/// The size of an integer type (e.g. u8, i64).
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

    pub fn from_num(size: usize) -> Self {
        match size {
            8 => IntSize::_8,
            16 => IntSize::_16,
            32 => IntSize::_32,
            64 => IntSize::_64,
            128 => IntSize::_128,
            _ => panic!("{size} is an invalid int size"),
        }
    }
}

impl IntType {
    pub fn new(size: u32, signed: bool) -> Self {
        Self { size: IntSize::from_num(size as usize), signed }
    }
}

/// A floating point data type, with several variations.
#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy, PartialOrd, Ord, XcReflect)]
pub enum FloatType {
    F32,
    F64,
}

impl From<usize> for FloatType {
    fn from(size: usize) -> FloatType {
        match size {
            32 => FloatType::F32,
            64 => FloatType::F64,
            _ => panic!("{size} is an invalid float size"),
        }
    }
}

/// A boolean data type.
#[derive(PartialEq, Eq, Hash, Clone, Copy, XcReflect)]
pub struct BoolType;

/// This represents the "Unknown" data type. By the time the compiler gets past the type
/// inference step of the HLR passes, this should not be present. Notably, unknown
/// is the default value for [`TypeEnum`] and [`Type`].
#[derive(PartialEq, Eq, Hash, Clone, XcReflect)]
pub struct UnknownType();
static UNKNOWN_STATIC: UnknownType = UnknownType();

/// A void data type.
#[derive(PartialEq, Eq, Hash, Clone, XcReflect)]
pub struct VoidType();
static VOID_STATIC: VoidType = VoidType();

/// An array data type.
#[derive(PartialEq, Hash, Eq, Clone, XcReflect)]
pub struct ArrayType {
    pub base: Type,
    pub count: u32,
}

/// A Destructor data type.
#[derive(Clone, XcReflect)]
pub struct DestructorType {
    pub base: Type,
    /// Notably, the destructor code is stored with the DestructorType in an Arc<HLR>, 
    /// not a [`crate::FuncID`].
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
