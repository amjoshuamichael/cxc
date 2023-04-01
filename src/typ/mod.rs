use crate::errors::{TErr, TResult};
use crate::lex::{TypeName, VarName};
use crate::parse::TypeSpec;
use crate::typ::fields_iter::PrimitiveFieldsIter;
use crate::UniqueFuncInfo;
use crate::{CompData, TypeRelation};
use std::collections::hash_map::DefaultHasher;
use std::fmt::{Debug, Formatter};
use std::hash::{Hash, Hasher};
use std::ops::Deref;
use std::sync::Arc;

pub mod could_come_from;
pub mod fields_iter;
pub mod invalid_state;
mod kind;
mod nested_field_count;
mod size;

use cxc_derive::{XcReflect};
use inkwell::types::FunctionType;
use invalid_state::InvalidState;
pub use kind::Kind;
use crate::xc_opaque;

use crate as cxc;

#[derive(Clone, PartialEq, Hash, Eq, PartialOrd, Ord, XcReflect)]
pub struct Type(Arc<TypeData>);

impl Debug for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result { write!(f, "{:?}", self.0) }
}

impl Default for Type {
    fn default() -> Self { Type::unknown() }
}

#[derive(PartialEq, Eq, Debug, XcReflect)]
pub enum ReturnStyle {
    Direct,
    ThroughI32,
    ThroughI64,
    ThroughI32I32,
    ThroughI64I32,
    ThroughI64I64,
    MoveIntoI64I64,
    Sret,
    Void,
}

pub fn realize_return_style(return_style: ReturnStyle, on: &Type) -> Type {
    match return_style {
        ReturnStyle::ThroughI32 => Type::i(32),
        ReturnStyle::ThroughI64 => Type::i(64),
        ReturnStyle::ThroughI32I32 => Type::new_tuple(vec![Type::i(32); 2]),
        ReturnStyle::ThroughI64I32 => 
            Type::new_tuple(vec![Type::i(64), Type::i(32)]),
        ReturnStyle::ThroughI64I64 | ReturnStyle::MoveIntoI64I64 => 
            Type::new_tuple(vec![Type::i(64), Type::i(64)]),
        ReturnStyle::Sret | ReturnStyle::Void => Type::void(),
        ReturnStyle::Direct => on.clone(),
    }
}

impl Type {
    pub fn new(type_enum: TypeEnum) -> Self { Self(Arc::new(type_enum.into())) }

    pub fn size(&self) -> usize { size::size_of_type(self.clone()) as usize }

    pub fn rust_return_style(&self) -> ReturnStyle {
        if self.is_void() {
            return ReturnStyle::Void;
        }

        let size = self.size();
        let fields = self.primitive_fields_iter().take(4).collect::<Vec<_>>();

        if size > 4 && size <= 8 && fields.len() == 2 && 
            fields[0].size() <= 4 && fields[1].size() <= 4 {
            ReturnStyle::ThroughI32I32
        } else if fields.len() >= 3 {
            ReturnStyle::Sret
        } else {
            self.return_style()
        }
    }

    pub fn return_style(&self) -> ReturnStyle {
        use TypeEnum::*;

        fn return_style_from_size(size: usize) -> ReturnStyle {
            if size > 16 {
                ReturnStyle::Sret
            } else if size == 16 {
                ReturnStyle::ThroughI64I64
            } else if size == 12 {
                ReturnStyle::ThroughI64I32
            } else if size <= 8 {
                ReturnStyle::ThroughI64
            } else if size <= 4 {
                ReturnStyle::ThroughI32
            } else {
                todo!("cannot find return style for type of size {size}")
            }
        }

        match self.as_type_enum() {
            Int(_) | Ref(_) | Float(_) | Bool(_) | Func(_) => ReturnStyle::Direct,
            Struct(_) | Array(_) | Variant(_) => {
                let size = self.size();

                if size > 16 {
                    return ReturnStyle::Sret;
                }

                let mut prim_iter = PrimitiveFieldsIter::new(self.clone());
                if let Some(first_field) = prim_iter.next() && 
                    let Some(second_field) = prim_iter.next() {

                    if first_field.size() == 4 && second_field.size() == 8 {
                        ReturnStyle::MoveIntoI64I64
                    } else {
                        return_style_from_size(size)
                    }
                } else {
                    ReturnStyle::Direct
                }
            },
            Sum(sum_type) => {
                let size = self.size();

                if size > 16 {
                    return ReturnStyle::Sret;
                }

                if sum_type.has_internal_discriminant() {
                    sum_type.largest_variant_data().return_style()
                } else {
                    return_style_from_size(size)
                }
            },
            Void => ReturnStyle::Void,
            Unknown => panic!("cannot return unknown type"),
        }
    }

    pub fn is_subtype_of(&self, of: &Type) -> bool {
        match self.as_type_enum() {
            TypeEnum::Variant(VariantType { parent, .. }) 
                if let TypeEnum::Sum(of_sum) = of.as_type_enum() => parent == of_sum,
            other => other == of.as_type_enum(),
        }
    }

    pub fn are_subtypes(lhs: &Self, rhs: &Self) -> bool {
        lhs.is_subtype_of(rhs) || rhs.is_subtype_of(lhs)
    }

    pub fn raw_return_type(&self) -> Type {
        realize_return_style(self.return_style(), self)
    }

    pub fn rust_raw_return_type(&self) -> Type {
        realize_return_style(self.rust_return_style(), self)
    }

    pub fn id(&self) -> u64 {
        let mut hasher = DefaultHasher::new();
        self.as_type_enum().hash(&mut hasher);
        self.name().hash(&mut hasher);
        self.generics().hash(&mut hasher);
        hasher.finish()
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
        let mut output = self;

        for _ in 0..count {
            output = output.get_deref()?;
        }

        Some(output)
    }

    pub fn get_ref(&self) -> Type { Type::new(TypeEnum::Ref(RefType { base: self.clone() })) }

    pub fn get_deref(self) -> Option<Type> {
        match &self.0.type_enum {
            TypeEnum::Ref(t) => Some(t.base.clone()),
            _ => None,
        }
    }

    pub fn get_auto_deref(&self, comp_data: &CompData) -> TResult<Type> {
        if let TypeEnum::Ref(RefType { base }) = self.as_type_enum() {
            Ok(base.clone())
        } else {
            Ok(comp_data
                .get_func_type(&UniqueFuncInfo::new(
                    &"deref".into(),
                    &TypeRelation::MethodOf(self.get_ref()),
                    self.generics().clone(),
                ))
                .map_err(|_| TErr::CantDeref(self.clone()))?
                .ret)
        }
    }

    pub fn deref_chain(&self, comp_data: &CompData) -> Vec<Type> {
        let mut chain = Vec::new();
        chain.push(self.clone());

        while let Ok(derefed) = chain.last().cloned().unwrap().get_auto_deref(comp_data) {
            chain.push(derefed);
        }

        chain
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

    pub fn i(size: u32) -> Type { Type::new(TypeEnum::Int(IntType { size, signed: true })) }
    pub fn u(size: u32) -> Type {
        Type::new(TypeEnum::Int(IntType {
            size,
            signed: false,
        }))
    }

    pub fn f16() -> Type { Type::new(TypeEnum::Float(FloatType::F16)) }

    pub fn f32() -> Type { Type::new(TypeEnum::Float(FloatType::F32)) }

    pub fn f64() -> Type { Type::new(TypeEnum::Float(FloatType::F64)) }

    pub fn f(size: impl Into<FloatType>) -> Type { Type::new(TypeEnum::Float(size.into())) }

    pub fn bool() -> Type { Type::new(TypeEnum::Bool(BoolType)) }

    pub fn empty() -> Type { Type::new_struct(Vec::new()) }

    pub fn new_struct(fields: Vec<(VarName, Type)>) -> Type {
        Type::new(TypeEnum::Struct(StructType {
            fields,
            repr: Repr::Rust,
        }))
    }

    pub fn new_tuple(fields: Vec<Type>) -> Type {
        let indexed_fields = fields
            .into_iter()
            .enumerate()
            .map(|(i, field)| ((i as u32).into(), field))
            .collect();

        Type::new(TypeEnum::Struct(StructType {
            fields: indexed_fields,
            repr: Repr::Rust,
        }))
    }

    pub fn new_sum(mut variants: Vec<(TypeName, Type)>) -> Type {
        if variants.len() == 2
            && variants[0].1 != Type::empty()
            && variants[1].1 == Type::empty()
        {
            variants.swap(0, 1);
        }

        let largest_variant_index = variants
            .iter()
            .enumerate()
            .max_by_key(|(_, (_, typ))| typ.size())
            .unwrap()
            .0;

        Type::new(TypeEnum::Sum(SumType {
            variants,
            largest_variant_index,
        }))
    }

    pub fn unknown() -> Type { Type::new(TypeEnum::Unknown) }

    pub fn void() -> Type { Type::new(TypeEnum::Void) }
    pub fn void_ptr() -> Type { Type::new(TypeEnum::Void).get_ref() }

    pub fn func_with_args(self, args: Vec<Type>) -> Type {
        Type::new(TypeEnum::Func(FuncType { ret: self, args }))
    }

    pub fn as_type_enum(&self) -> &TypeEnum { &self.0.type_enum }
    pub fn clone_type_enum(&self) -> TypeEnum { self.0.type_enum.clone() }

    pub fn name(&self) -> &TypeName { &self.0.name }
    pub fn generics(&self) -> &Vec<Type> { &self.0.generics }
    pub fn from_function(&self) -> &TypeName { &self.0.from_function }
    pub fn parameters(&self) -> &Vec<Type> { &self.0.parameters }

    pub(crate) fn with_name(self, name: TypeName) -> Self {
        self.modify_type_data(|data| data.name = name.clone())
    }

    pub(crate) fn with_generics(self, generics: &[Type]) -> Self {
        self.modify_type_data(|data| data.generics = generics.to_owned())
    }

    pub fn with_from_function(self, name: TypeName) -> Self {
        self.modify_type_data(|data| data.from_function = name.clone())
    }

    pub fn with_parameters(self, parameters: &[Type]) -> Self {
        self.modify_type_data(|data| data.parameters = parameters.to_owned())
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
    pub fn is_empty(&self) -> bool { self == &Type::empty() }

    pub fn is_shallow(&self) -> bool {
        !self.fields_iter().any(|field| matches!(field.as_type_enum(), TypeEnum::Ref(_)))
    }
}

impl From<Type> for TypeSpec {
    fn from(typ: Type) -> TypeSpec { TypeSpec::Type(typ) }
}

#[derive(Default, Hash, PartialEq, Eq, Clone, PartialOrd, Ord, XcReflect)]
#[xc_opaque]
pub struct TypeData {
    pub type_enum: TypeEnum,
    pub name: TypeName,
    pub generics: Vec<Type>,
    pub from_function: TypeName,
    pub parameters: Vec<Type>,
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

#[derive(Default, Hash, PartialEq, Eq, Clone, PartialOrd, Ord, XcReflect)]
pub enum TypeEnum {
    Int(IntType),
    Float(FloatType),
    Struct(StructType),
    Sum(SumType),
    Variant(VariantType),
    Ref(RefType),
    Func(FuncType),
    Array(ArrayType),
    Bool(BoolType),
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
            TypeEnum::Bool(t) => t,
            TypeEnum::Sum(t) => t,
            TypeEnum::Variant(t) => t,
            TypeEnum::Void => &VOID_STATIC,
            TypeEnum::Unknown => &UNKNOWN_STATIC,
        }
    }
}

#[derive(PartialEq, Eq, Hash, Clone, PartialOrd, Ord, Debug, XcReflect)]
pub struct RefType {
    pub base: Type,
}

#[derive(PartialEq, Eq, Hash, Clone, PartialOrd, Ord, Debug, XcReflect)]
pub struct FuncType {
    pub ret: Type,
    pub args: Vec<Type>,
}

#[derive(PartialEq, Eq, Hash, Clone, Default, PartialOrd, Ord, Debug, XcReflect)]
pub enum Repr {
    #[default]
    Rust,
    C,
}

#[derive(PartialEq, Eq, Hash, Debug, Default, Clone, PartialOrd, Ord, XcReflect)]
pub struct StructType {
    pub fields: Vec<(VarName, Type)>,
    pub repr: Repr,
}

impl StructType {
    pub fn get_field_type(&self, field_name: &VarName) -> TResult<Type> {
        for field in &self.fields {
            if field.0 == *field_name {
                return Ok(field.1.clone());
            }
        }

        Err(TErr::FieldNotFound(self.clone(), field_name.clone()))
    }

    pub fn get_field_index(&self, field_name: &VarName) -> TResult<usize> {
        self.fields
            .iter()
            .position(|field| field.0 == *field_name)
            .ok_or(TErr::FieldNotFound(self.clone(), field_name.clone()))
    }

    pub fn is_tuple(&self) -> bool { self.fields.is_empty() || matches!(self.fields[0].0, VarName::TupleIndex(0)) }
}

// TODO: is not interroperable with a rust sum type with 0 variants.
#[derive(PartialEq, Clone, Eq, Hash, Debug, PartialOrd, Ord, XcReflect)]
pub struct SumType {
    pub variants: Vec<(TypeName, Type)>,
    largest_variant_index: usize,
}

impl SumType {
    pub fn get_variant_type(
        &self,
        variant_name: &TypeName,
    ) -> TResult<Type> {
        let (variant_index, variant_type) = self
            .variants
            .iter()
            .enumerate()
            .find(|(_, (name, _))| name == variant_name)
            .ok_or(TErr::VariantNotFound(self.clone(), variant_name.clone()))
            .map(|(index, (_, t))| (index, t.clone()))?;

        Ok(Type::new(TypeEnum::Variant(VariantType {
            tag: variant_index as u32,
            parent: self.clone(),
            variant_type,
        })))
    }

    pub fn get_variant_index(&self, field_name: &TypeName) -> usize {
        self.variants
            .iter()
            .position(|field| &field.0 == field_name)
            .unwrap()
    }

    pub fn largest_variant_data(&self) -> Type {
        self.variants[self.largest_variant_index].1.clone()
    }

    pub fn largest_variant_as_struct(&self) -> Type {
        VariantType::as_struct_no_parent(self, &self.largest_variant_data())
    }

    pub fn largest_variant_index(&self) -> usize { self.largest_variant_index }

    pub fn has_internal_discriminant(&self) -> bool {
        if self.variants.is_empty() {
            return false;
        }

        let mut nonempty_variants = self.variants.iter().filter(|(_, typ)| !typ.is_empty());
        if let Some((_, nonempty_variant)) = nonempty_variants.next() {
            if nonempty_variants.count() != 0 {
                return false;
            }

            nonempty_variant
                .invalid_state((self.variants.len() - 2) as u32)
                .is_some()
        } else {
            false
        }
    }
}

#[derive(PartialEq, Eq, Hash, Debug, Clone, PartialOrd, Ord, XcReflect)]
pub struct VariantType {
    pub tag: u32,
    pub parent: SumType,
    pub variant_type: Type,
}

impl VariantType {
    pub fn as_struct(&self) -> Type {
        Self::as_struct_no_parent(&self.parent, &self.variant_type)
    }

    pub fn parent_type(&self) -> Type {
        Type::new(TypeEnum::Sum(self.parent.clone()))
    }

    pub fn as_struct_no_parent(parent: &SumType, variant_type: &Type) -> Type {
        let tag = ("tag".into(), Type::i(8));

        let mut fields = match variant_type.as_type_enum() {
            TypeEnum::Struct(StructType { fields, .. }) => fields.clone(),
            _ => vec![("data".into(), variant_type.clone())],
        };

        if !parent.has_internal_discriminant() {
            fields.insert(0, tag);
        }

        let as_struct_no_padding = Type::new_struct(fields.clone());

        let padding_amount = if &parent.largest_variant_data() != variant_type {
            parent.largest_variant_as_struct().size() - as_struct_no_padding.size()
        } else {
            0
        };

        if padding_amount > 0 {
            fields.push(("padding".into(), Type::i(8).get_array(padding_amount as u32)));
        }

        Type::new_struct(fields)
    }
}

#[derive(PartialEq, Eq, Hash, Clone, PartialOrd, Ord, XcReflect)]
pub struct IntType {
    // when we need to support 2-billion-bit integers, we'll be ready
    pub size: u32,
    pub signed: bool,
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

#[derive(PartialEq, Eq, Hash, Clone, Copy, PartialOrd, Ord, XcReflect)]
pub struct BoolType;

#[derive(PartialEq, Eq, Hash, Clone, PartialOrd, Ord, XcReflect)]
pub struct UnknownType();
static UNKNOWN_STATIC: UnknownType = UnknownType();

#[derive(PartialEq, Eq, Hash, Clone, PartialOrd, Ord, XcReflect)]
pub struct VoidType();
static VOID_STATIC: VoidType = VoidType();

#[derive(PartialEq, Hash, Eq, Clone, PartialOrd, Ord, XcReflect)]
pub struct ArrayType {
    pub base: Type,
    pub count: u32,
}
