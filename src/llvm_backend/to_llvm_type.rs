use std::cell::RefCell;
use std::iter::once;
use inkwell::types::{BasicMetadataTypeEnum, FunctionType};
use std::collections::btree_map::BTreeMap;

use crate::*;
use inkwell::AddressSpace;
use inkwell::context::Context;
use inkwell::types::{AnyTypeEnum, BasicTypeEnum, AnyType, BasicType};

pub trait ToLLVMType {
    fn to_any_type(&self, context: &'static Context) -> AnyTypeEnum<'static>;

    fn to_basic_type(&self, context: &'static Context) -> BasicTypeEnum<'static> {
        self.to_any_type(context).try_into().unwrap()
    }
}

impl ToLLVMType for Type {
    fn to_any_type(&self, context: &'static Context) -> AnyTypeEnum<'static> {
        self.as_type_enum().to_any_type(context)
    }
}

impl ToLLVMType for TypeEnum {
    fn to_any_type(&self, context: &'static Context) -> AnyTypeEnum<'static> {
        match self {
            TypeEnum::Int(t) => t.to_any_type(context),
            TypeEnum::Float(t) => t.to_any_type(context),
            TypeEnum::Struct(t) => t.to_any_type(context),
            TypeEnum::Union(t) => t.to_any_type(context),
            TypeEnum::Ref(t) => t.to_any_type(context),
            TypeEnum::Func(t) => t.to_any_type(context),
            TypeEnum::Array(t) => t.to_any_type(context),
            TypeEnum::Destructor(DestructorType { base, .. }) => base.to_any_type(context),
            TypeEnum::Bool => BoolType.to_any_type(context),
            TypeEnum::Void => VoidType().to_any_type(context),
            TypeEnum::Unknown => UnknownType().to_any_type(context),
        }
    }
}

impl ToLLVMType for RefType {
    fn to_any_type(&self, context: &'static Context) -> AnyTypeEnum<'static> {
        if self.base.is_void() {
            context.i8_type().ptr_type(AddressSpace::default()).into()
        } else {
            self.base
                .to_basic_type(context)
                .ptr_type(AddressSpace::default())
                .into()
        }
    }
}

impl ToLLVMType for FuncType {
    fn to_any_type(&self, context: &'static Context) -> AnyTypeEnum<'static> {
        self.llvm_func_type(context)
            .ptr_type(AddressSpace::default())
            .as_any_type_enum()
    }
}

impl FuncType {
    pub fn llvm_func_type<'f>(&self, context: &'static Context) -> FunctionType<'f> {
        let return_style = self.ret.return_style(self.abi);

        if return_style != ReturnStyle::Pointer {
            let args: Vec<BasicMetadataTypeEnum> = self
                .args
                .iter()
                .map(|t| t.raw_arg_type(self.abi).to_basic_type(context).into())
                .collect();

            if self.ret.is_void() {
                context.void_type().fn_type(&args[..], false)
            } else {
                let return_type = self.ret.raw_return_type(self.abi);

                return_type.to_basic_type(context).fn_type(&args, false)
            }
        } else {
            let args: Vec<BasicMetadataTypeEnum> = once(&self.ret.clone().get_ref())
                .chain(self.args.iter())
                .map(|t| t.raw_arg_type(self.abi).to_basic_type(context).into())
                .collect();

            context.void_type().fn_type(&args[..], false)
        }
    }
}


impl ToLLVMType for StructType {
    fn to_any_type(&self, context: &'static Context) -> AnyTypeEnum<'static> {
        let field_types: Vec<BasicTypeEnum> = self
            .fields
            .iter()
            .map(|Field { typ, .. }| typ.to_basic_type(context))
            .collect();

        context
            .struct_type(&field_types[..], false)
            .as_any_type_enum()
    }
}

impl ToLLVMType for UnionType {
    fn to_any_type(&self, context: &'static Context) -> AnyTypeEnum<'static> {
        let size = self.largest_field().map(Type::size).unwrap_or(0);
        context.i8_type().array_type(size as u32).as_any_type_enum()
    }
}


impl ToLLVMType for IntType {
    fn to_any_type(&self, context: &'static Context) -> AnyTypeEnum<'static> {
        context.custom_width_int_type(self.size.to_num() as u32).as_any_type_enum()
    }
}

impl ToLLVMType for FloatType {
    fn to_any_type(&self, context: &'static Context) -> AnyTypeEnum<'static> {
        match self {
            FloatType::F32 => context.f32_type().as_any_type_enum(),
            FloatType::F64 => context.f64_type().as_any_type_enum(),
        }
    }
}

impl ToLLVMType for BoolType {
    fn to_any_type(&self, context: &'static Context) -> AnyTypeEnum<'static> {
        context.bool_type().as_any_type_enum()
    }
}

impl ToLLVMType for ArrayType {
    fn to_any_type(&self, context: &'static Context) -> AnyTypeEnum<'static> {
        self.base
            .to_basic_type(context)
            .array_type(self.count)
            .as_any_type_enum()
    }
}

impl ToLLVMType for UnknownType {
    fn to_any_type(&self, _: &'static Context) -> AnyTypeEnum<'static> {
        panic!("Unknown type cannot be converted to LLVM type")
    }
}

impl ToLLVMType for VoidType {
    fn to_any_type(&self, context: &'static Context) -> AnyTypeEnum<'static> {
        context.void_type().as_any_type_enum()
    }
}
