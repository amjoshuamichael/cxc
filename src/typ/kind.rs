use super::*;
use std::iter::once;

use inkwell::context::Context;
use inkwell::types::AnyType;
use inkwell::types::AnyTypeEnum;
use inkwell::types::BasicMetadataTypeEnum;
use inkwell::types::BasicType;
use inkwell::types::BasicTypeEnum;
use inkwell::AddressSpace;

pub trait Kind {
    fn name(&self) -> String;
    fn to_any_type<'t>(&self, context: &'t Context) -> AnyTypeEnum<'t>;

    fn to_basic_type<'t>(&self, context: &'t Context) -> BasicTypeEnum<'t> {
        self.to_any_type(context).try_into().unwrap()
    }
}

impl Kind for Type {
    fn name(&self) -> String { self.as_type_enum().name() }

    fn to_any_type<'t>(&self, context: &'t Context) -> AnyTypeEnum<'t> {
        self.as_type_enum().to_any_type(context)
    }
}

impl Kind for RefType {
    fn name(&self) -> String { "&".to_string() + &*format!("{:?}", self.base) }

    fn to_any_type<'t>(&self, context: &'t Context) -> AnyTypeEnum<'t> {
        if self.base.is_void() {
            context.i32_type().ptr_type(AddressSpace::default()).into()
        } else {
            self.base
                .to_basic_type(context)
                .ptr_type(AddressSpace::default())
                .into()
        }
    }
}

impl Kind for FuncType {
    fn name(&self) -> String {
        let args_names: String = self
            .args
            .iter()
            .map(|t| format!("{:?}", t))
            .collect::<Vec<String>>()
            .join(", ");
        let ret_name = &self.ret_type;

        format!("({args_names}) -> {ret_name:?}")
    }

    fn to_any_type<'t>(&self, context: &'t Context) -> AnyTypeEnum<'t> {
        self.llvm_func_type(context)
            .ptr_type(AddressSpace::default())
            .try_into()
            .unwrap()
    }
}

impl FuncType {
    pub fn llvm_func_type<'t>(&self, context: &'t Context) -> FunctionType<'t> {
        if self.ret_type.return_style() != ReturnStyle::Sret {
            let args: Vec<BasicMetadataTypeEnum> = self
                .args
                .iter()
                .map(|t| t.to_basic_type(context).into())
                .collect();

            if self.ret_type.is_void() {
                context.void_type().fn_type(&*args, false)
            } else {
                let return_type = self.ret_type.raw_return_type().to_basic_type(context);
                return_type.fn_type(&*args, false)
            }
        } else {
            let args: Vec<BasicMetadataTypeEnum> = once(&self.ret_type.clone().get_ref())
                .chain(self.args.iter())
                .map(|t| t.to_basic_type(context).into())
                .collect();

            context.void_type().fn_type(&args[..], true)
        }
    }
}

impl Kind for StructType {
    fn name(&self) -> String { format!("{:?}", self.fields) }

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

impl Kind for SumType {
    fn name(&self) -> String { format!("/{:?}/", self.variants) }

    fn to_any_type<'t>(&self, context: &'t Context) -> AnyTypeEnum<'t> {
        // find the largest type in variants
        let largest_variant = self.largest_variant().to_basic_type(context);

        context
            .struct_type(&[context.i32_type().as_basic_type_enum(), largest_variant], true)
            .as_any_type_enum()
    }
}

impl Kind for VariantType {
    fn name(&self) -> String { format!("{:?}.{}", self.parent, self.tag) }

    fn to_any_type<'t>(&self, context: &'t Context) -> AnyTypeEnum<'t> {
        context
            .struct_type(
                &[
                    context.i32_type().as_basic_type_enum(),
                    self.variant_type.to_basic_type(context),
                ],
                true,
            )
            .as_any_type_enum()
    }
}

impl Kind for IntType {
    fn name(&self) -> String { format!("i{}", self.size) }

    fn to_any_type<'t>(&self, context: &'t Context) -> AnyTypeEnum<'t> {
        context.custom_width_int_type(self.size).as_any_type_enum()
    }
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

impl Kind for BoolType {
    fn name(&self) -> String { "bool".into() }

    fn to_any_type<'t>(&self, context: &'t Context) -> AnyTypeEnum<'t> {
        context.bool_type().as_any_type_enum()
    }
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

impl Kind for UnknownType {
    fn name(&self) -> String { String::from("Unknown") }

    fn to_any_type<'t>(&self, _: &'t Context) -> AnyTypeEnum<'t> {
        panic!("Unknown type cannot be converted to LLVM type")
    }
}

impl Kind for VoidType {
    fn name(&self) -> String { String::from("Void") }

    fn to_any_type<'t>(&self, context: &'t Context) -> AnyTypeEnum<'t> {
        context.void_type().as_any_type_enum()
    }
}

impl Kind for OpaqueType {
    fn name(&self) -> String { format!("Opaque with size {} bytes", self.size) }

    fn to_any_type<'t>(&self, context: &'t Context) -> AnyTypeEnum<'t> {
        context
            .custom_width_int_type(self.size * 8)
            .as_any_type_enum()
    }
}
