use crate::errors::{TErr, TResult};
use crate::lex::{TypeName, VarName};
use crate::parse::TypeSpec;
use crate::typ::fields_iter::PrimitiveFieldsIter;
use std::collections::hash_map::DefaultHasher;
use std::fmt::{Debug, Formatter};
use std::hash::{Hash, Hasher};
use std::ops::Deref;
use std::sync::Arc;

pub mod could_come_from;
pub mod fields_iter;
mod kind;
mod nested_field_count;
mod size;

use inkwell::types::FunctionType;
pub use kind::Kind;

use self::fields_iter::FieldsIter;

#[derive(Clone, PartialEq, Hash, Eq, PartialOrd, Ord)]
pub struct Type(Arc<TypeData>);

impl Debug for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result { write!(f, "{:?}", self.0) }
}

impl Default for Type {
    fn default() -> Self { Type::unknown() }
}

#[derive(PartialEq, Eq, Debug)]
pub enum ReturnStyle {
    Direct,
    ThroughI64,
    ThroughI64I32,
    ThroughI64I64,
    MoveIntoI64I64,
    Sret,
    Void,
}

impl Type {
    fn new(type_enum: TypeEnum) -> Self { Self(Arc::new(type_enum.into())) }

    pub fn size(&self) -> usize { size::size_of_type(self.clone()) as usize }

    pub fn return_style(&self) -> ReturnStyle {
        use TypeEnum::*;

        fn return_style_from_size(size: usize) -> ReturnStyle {
            if size > 16 {
                ReturnStyle::Sret
            } else if size == 16 {
                ReturnStyle::ThroughI64I64
            } else if size == 12 {
                ReturnStyle::ThroughI64I32
            } else if size == 8 {
                ReturnStyle::ThroughI64
            } else if size == 4 {
                ReturnStyle::Direct
            } else {
                todo!("cannot find return style for type of size {size}")
            }
        }

        match self.as_type_enum() {
            Int(_) | Ref(_) | Float(_) | Bool(_) | Func(_) => ReturnStyle::Direct,
            Struct(_) | Array(_) | Opaque(_) | Variant(_) => {
                if self.size() > 16 {
                    return ReturnStyle::Sret;
                }

                let mut prim_iter = PrimitiveFieldsIter::new(self.clone());
                if let Some(first_field) = prim_iter.next() && 
                    let Some(second_field) = prim_iter.next() {
                    if first_field.size() == 4 && second_field.size() == 8 {
                        ReturnStyle::MoveIntoI64I64
                    } else {
                        return_style_from_size(self.size())
                    }
                } else {
                    ReturnStyle::Direct
                }
            },
            Sum(sum_type) => {
                if self.size() > 16 {
                    return ReturnStyle::Sret;
                }

                if sum_type.is_discriminant_nullref() {
                    sum_type.largest_variant().return_style()
                } else {
                    return_style_from_size(self.size())
                }
            },
            Void => ReturnStyle::Void,
            Unknown => panic!("cannot return unknown type"),
        }
    }

    pub fn is_subtype_of(&self, of: &Type) -> bool {
        match self.as_type_enum() {
            TypeEnum::Variant(VariantType { parent, .. }) => parent == of,
            other => other == of.as_type_enum(),
        }
    }

    pub fn are_subtypes(lhs: &Self, rhs: &Self) -> bool {
        lhs.is_subtype_of(rhs) || rhs.is_subtype_of(lhs)
    }

    pub fn raw_return_type(&self) -> Type {
        match self.return_style() {
            ReturnStyle::ThroughI64 => Type::i(64),
            ReturnStyle::ThroughI64I32 => Type::new_struct(vec![
                (VarName::from("0"), Type::i(64)),
                (VarName::from("1"), Type::i(32)),
            ]),
            ReturnStyle::ThroughI64I64 | ReturnStyle::MoveIntoI64I64 => Type::new_struct(vec![
                (VarName::from("0"), Type::i(64)),
                (VarName::from("1"), Type::i(64)),
            ]),
            ReturnStyle::Sret | ReturnStyle::Void => Type::void(),
            ReturnStyle::Direct => self.clone(),
        }
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
        let mut output = self.clone();

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

    pub fn get_array(self, count: u32) -> Type {
        Type::new(TypeEnum::Array(ArrayType { base: self, count }))
    }

    pub fn wrap(self) -> Type { Type::new_tuple(vec![self]) }

    pub fn as_u64(self) -> u64 { self.into_raw() as u64 }
    pub fn into_raw(self) -> *const TypeData { Arc::into_raw(self.0) }
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
        Type::new(TypeEnum::Struct(StructType { fields }))
    }

    pub fn new_tuple(fields: Vec<Type>) -> Type {
        let indexed_fields = fields
            .into_iter()
            .enumerate()
            .map(|(i, field)| (VarName::from(i.to_string()), field))
            .collect();

        Type::new(TypeEnum::Struct(StructType {
            fields: indexed_fields,
        }))
    }

    pub fn new_sum(mut variants: Vec<(TypeName, Type)>) -> Type {
        if variants.len() == 2
            && variants[0].1 != Type::empty()
            && variants[1].1 == Type::empty()
        {
            variants.swap(0, 1);
        }

        Type::new(TypeEnum::Sum(SumType { variants }))
    }

    pub fn opaque_with_size(size: u32, name: &str) -> Type {
        Type::new(TypeEnum::Opaque(OpaqueType { size })).with_name(TypeName::from(name))
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

    pub fn with_name(self, name: TypeName) -> Self {
        self.modify_type_data(|data| data.name = name.clone())
    }

    pub fn with_generics(self, generics: &Vec<Type>) -> Self {
        self.modify_type_data(|data| data.generics = generics.clone())
    }

    pub fn with_from_function(self, name: TypeName) -> Self {
        self.modify_type_data(|data| data.from_function = name.clone())
    }

    pub fn with_parameters(self, parameters: &Vec<Type>) -> Self {
        self.modify_type_data(|data| data.parameters = parameters.clone())
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
}

impl Into<TypeSpec> for Type {
    fn into(self) -> TypeSpec { TypeSpec::Type(self) }
}

#[derive(Default, Hash, PartialEq, Eq, Clone, PartialOrd, Ord)]
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

#[derive(Default, Hash, PartialEq, Eq, Clone, PartialOrd, Ord)]
pub enum TypeEnum {
    Int(IntType),
    Float(FloatType),
    Struct(StructType),
    Sum(SumType),
    Variant(VariantType),
    Opaque(OpaqueType),
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
            TypeEnum::Opaque(t) => t,
            TypeEnum::Sum(t) => t,
            TypeEnum::Variant(t) => t,
            TypeEnum::Void => &VOID_STATIC,
            TypeEnum::Unknown => &UNKNOWN_STATIC,
        }
    }
}

#[derive(PartialEq, Eq, Hash, Clone, PartialOrd, Ord)]
pub struct RefType {
    base: Type,
}

#[derive(PartialEq, Eq, Hash, Clone, PartialOrd, Ord)]
pub struct FuncType {
    pub ret: Type,
    pub args: Vec<Type>,
}

impl FuncType {
    pub fn ret_type(&self) -> Type { self.ret.clone() }
    pub fn args(&self) -> Vec<Type> { self.args.clone() }
}

#[derive(PartialEq, Eq, Hash, Debug, Clone, PartialOrd, Ord)]
pub struct StructType {
    pub fields: Vec<(VarName, Type)>,
}

impl StructType {
    pub fn get_field_type(&self, field_name: &VarName) -> TResult<Type> {
        for field in &self.fields {
            if field.0 == *field_name {
                return Ok(field.1.clone());
            }
        }

        return Err(TErr::FieldNotFound(self.clone(), field_name.clone()));
    }

    pub fn get_field_index(&self, field_name: &VarName) -> usize {
        self.fields
            .iter()
            .position(|field| field.0 == *field_name)
            .unwrap()
    }

    pub fn field_count(&self) -> usize { self.fields.len() }

    pub fn is_tuple(&self) -> bool { self.fields.len() == 0 || &*self.fields[0].0 == "0" }
}

#[derive(PartialEq, Clone, Eq, Hash, Debug, PartialOrd, Ord)]
pub struct SumType {
    pub variants: Vec<(TypeName, Type)>,
}

impl SumType {
    // parent arc contains the Type object which contains this SumType. it is
    // required to fill in the parent field of the variant type.
    pub fn get_variant_type(
        &self,
        parent_arc: &Type,
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
            parent: parent_arc.clone(),
            variant_type,
        })))
    }

    pub fn get_variant_index(&self, field_name: &TypeName) -> usize {
        self.variants
            .iter()
            .position(|field| &field.0 == field_name)
            .unwrap()
    }

    pub fn largest_variant(&self) -> Type {
        self.variants
            .iter()
            .map(|(_, typ)| typ)
            .max_by_key(|typ| typ.size())
            .unwrap()
            .clone()
    }

    pub fn is_discriminant_nullref(&self) -> bool {
        if self.variants.len() == 2
            && self.variants[0].1 == Type::empty()
            && self.variants[1].1 != Type::empty()
        {
            let nonempty_variant = &self.variants[1].1;

            let first_type_is_ref = matches!(nonempty_variant.as_type_enum(), TypeEnum::Ref(_));
            let first_type_contains_ref: bool = FieldsIter::new(nonempty_variant.clone())
                .any(|typ| matches!(typ.as_type_enum(), TypeEnum::Ref(_)));

            first_type_is_ref || first_type_contains_ref
        } else {
            false
        }
    }
}

#[derive(PartialEq, Eq, Hash, Debug, Clone, PartialOrd, Ord)]
pub struct VariantType {
    pub tag: u32,
    pub parent: Type,
    pub variant_type: Type,
}

impl VariantType {
    pub fn parent(&self) -> Type { self.parent.clone() }

    pub fn as_struct(&self) -> Type {
        Type::new_struct(vec![
            ("tag".into(), Type::i(32)),
            ("data".into(), self.variant_type.clone()),
        ])
    }
}

#[derive(PartialEq, Eq, Hash, Clone, PartialOrd, Ord)]
pub struct IntType {
    // when we need to support 2-billion-bit integers, we'll be ready
    pub size: u32,
    pub signed: bool,
}

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy, PartialOrd, Ord)]
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

#[derive(PartialEq, Eq, Hash, Clone, Copy, PartialOrd, Ord)]
pub struct BoolType;

#[derive(PartialEq, Eq, Hash, Clone, PartialOrd, Ord)]
pub struct UnknownType();
static UNKNOWN_STATIC: UnknownType = UnknownType();

#[derive(PartialEq, Eq, Hash, Clone, PartialOrd, Ord)]
pub struct VoidType();
static VOID_STATIC: VoidType = VoidType();

#[derive(PartialEq, Hash, Eq, Clone, PartialOrd, Ord)]
pub struct ArrayType {
    pub base: Type,
    pub count: u32,
}

#[derive(PartialEq, Hash, Eq, Clone, PartialOrd, Ord)]
pub struct OpaqueType {
    pub size: u32,
}
