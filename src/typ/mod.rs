use crate::lex::{TypeName, VarName};
use crate::parse::TypeSpec;
use lazy_static::lazy_static;

use std::collections::HashSet;
use std::fmt::{Debug, Formatter};
use std::hash::Hash;
use std::ops::Deref;
use std::sync::{Arc, Mutex};

mod kind;
mod nested_field_count;
mod size;
mod sum_type_optimization;

use inkwell::types::FunctionType;
pub use kind::Kind;

use self::sum_type_optimization::FieldsIter;

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct Type(Arc<TypeData>);

impl Debug for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result { write!(f, "{:?}", self.0) }
}

impl Default for Type {
    fn default() -> Self { Type::never() }
}

lazy_static! {
    static ref SAVED_TYPES: Mutex<HashSet<Type>> = Mutex::new(HashSet::new());
}

#[derive(PartialEq, Eq, Debug)]
pub enum ReturnStyle {
    Direct,
    ThroughI64,
    ThroughI64I32,
    ThroughI64I64,
    Sret,
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
                total += array_type.count as usize * array_type.base.nested_field_count();
            },
            TypeEnum::Sum(_) => total = 2,
            _ => total = 1,
        }

        total
    }

    pub fn return_style(&self) -> ReturnStyle {
        use TypeEnum::*;

        let (size, is_one_thing) = match self.as_type_enum() {
            Int(_) | Ref(_) | Float(_) | Bool(_) | Func(_) => return ReturnStyle::Direct,
            Struct(_) | Array(_) | Opaque(_) | Variant(_) => {
                (self.size(), self.nested_field_count() == 1)
            },
            Sum(sum_type) => (sum_type.largest_variant().size() + 4, false),
            Unknown | Void => return ReturnStyle::Void,
        };

        if size > 16 {
            ReturnStyle::Sret
        } else if is_one_thing {
            ReturnStyle::Direct
        } else if size == 16 {
            ReturnStyle::ThroughI64I64
        } else if size == 12 {
            ReturnStyle::ThroughI64I64
        } else if size == 8 {
            ReturnStyle::ThroughI64
        } else if size == 4 {
            ReturnStyle::Direct
        } else {
            todo!("cannot return style for this type: {:?}", self);
        }
    }

    pub fn raw_return_type(&self) -> Type {
        match self.return_style() {
            ReturnStyle::ThroughI64 => Type::i(64),
            ReturnStyle::ThroughI64I32 => Type::new_struct(vec![
                (VarName::from("0"), Type::i(64)),
                (VarName::from("1"), Type::i(32)),
            ]),
            ReturnStyle::ThroughI64I64 => Type::new_struct(vec![
                (VarName::from("0"), Type::i(64)),
                (VarName::from("1"), Type::i(64)),
            ]),
            ReturnStyle::Sret | ReturnStyle::Void => Type::void(),
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

    pub fn as_u64(self) -> u64 { self.into_raw() as u64 }
    pub fn into_raw(self) -> *const TypeData { Arc::into_raw(self.0) }
    pub unsafe fn from_raw(data: *const TypeData) -> Self { Self(Arc::from_raw(data)) }

    pub fn i(size: u32) -> Type { Type::new(TypeEnum::Int(IntType { size })) }

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

    pub fn never() -> Type { Type::new(TypeEnum::Unknown) }

    pub fn void() -> Type { Type::new(TypeEnum::Void) }
    pub fn void_ptr() -> Type { Type::new(TypeEnum::Void).get_ref() }

    pub fn func_with_args(self, args: Vec<Type>) -> Type {
        Type::new(TypeEnum::Func(FuncType {
            ret_type: self,
            args,
        }))
    }

    pub fn as_type_enum<'a>(&self) -> &TypeEnum { &self.0.type_enum }

    pub fn name(&self) -> &Option<TypeName> { &self.0.name }
    pub fn generics(&self) -> &Vec<Type> { &self.0.generics }

    // panics if there is more than one reference to the inner TypeData
    pub fn with_name(self, name: TypeName) -> Self {
        let mut type_data = Arc::try_unwrap(self.0).unwrap();
        type_data.name = Some(name);
        Self(Arc::from(type_data))
    }

    // panics if there is more than one reference to the inner TypeData
    pub fn with_generics(self, generics: Vec<Type>) -> Self {
        let mut type_data = Arc::try_unwrap(self.0).unwrap();
        type_data.generics = generics;
        Self(Arc::from(type_data))
    }

    pub fn is_unknown(&self) -> bool { matches!(self.as_type_enum(), TypeEnum::Unknown) }
    pub fn is_void(&self) -> bool { matches!(self.as_type_enum(), TypeEnum::Void) }
}

impl Into<TypeSpec> for Type {
    fn into(self) -> TypeSpec { TypeSpec::Type(self) }
}

#[derive(Default, Hash, PartialEq, Eq)]
pub struct TypeData {
    type_enum: TypeEnum,
    name: Option<TypeName>,
    generics: Vec<Type>,
}

impl Debug for TypeData {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), std::fmt::Error> {
        match &self.name {
            Some(name) => write!(fmt, "{}", name),
            None => write!(fmt, "{:?}", self.type_enum),
        }
    }
}

impl From<TypeEnum> for TypeData {
    fn from(type_enum: TypeEnum) -> Self {
        Self {
            type_enum,
            name: None,
            generics: Vec::new(),
        }
    }
}

impl TypeData {
    pub fn is_named(&self) -> bool { self.name.is_some() }
}

#[derive(Default, Hash, PartialEq, Eq)]
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
            TypeEnum::Sum(t) => t,
            TypeEnum::Variant(t) => t,
            TypeEnum::Void => &VOID_STATIC,
            TypeEnum::Unknown => &UNKNOWN_STATIC,
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

#[derive(PartialEq, Clone, Eq, Hash, Debug)]
pub struct SumType {
    pub variants: Vec<(TypeName, Type)>,
}

impl SumType {
    // parent arc contains the Type object which contains this SumType. it is
    // required to fill in the parent field of the variant type.
    pub fn get_variant_type(&self, parent_arc: &Type, field_name: &TypeName) -> Option<Type> {
        let (variant_index, variant_type) = self
            .variants
            .iter()
            .enumerate()
            .find(|(_, (name, _))| name == field_name)
            .map(|(index, (_, t))| (index, t.clone()))?;

        Some(Type::new(TypeEnum::Variant(VariantType {
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

#[derive(PartialEq, Eq, Hash, Debug)]
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
pub struct UnknownType();
static UNKNOWN_STATIC: UnknownType = UnknownType();

#[derive(PartialEq, Eq, Hash)]
pub struct VoidType();
static VOID_STATIC: VoidType = VoidType();

#[derive(PartialEq, Hash, Eq)]
pub struct ArrayType {
    pub base: Type,
    pub count: u32,
}

#[derive(PartialEq, Hash, Eq)]
pub struct OpaqueType {
    pub size: u32,
}
