use cranelift::codegen::ir::ArgumentPurpose;
use cranelift::prelude::AbiParam;
use cranelift::prelude::Signature;
use cranelift::prelude::Type as ClType;
use cranelift::prelude::isa::CallConv;
use cranelift::prelude::types as cl_types;
use crate::ABI;
use crate::ArrayType;
use crate::BoolType;
use crate::FuncType;
use crate::RefType;
use crate::TypeEnum;
use crate::typ::ArgStyle;
use crate::typ::DestructorType;
use crate::typ::Field;
use crate::typ::ReturnStyle;
use crate::typ::UnionType;
use crate::typ::UnknownType;
use crate::typ::VoidType;
use crate::{Type, IntType, FloatType, StructType};

pub fn func_type_to_signature(typ: &FuncType, sig: &mut Signature) {
    sig.params.clear();
    sig.returns.clear();

    let abi = typ.abi;

    let return_style = typ.ret.return_style(abi);

    if return_style == ReturnStyle::Pointer {
        let sret_abi = AbiParam::special(cl_types::I64, ArgumentPurpose::StructReturn);
        sig.params.push(sret_abi);
    }

    for typ in &typ.args {
        if typ.arg_style(abi) == ArgStyle::Pointer && crate::ARCH_x86 && cfg!(unix) {
            sig.params.push(AbiParam::special(
                cl_types::I64, 
                ArgumentPurpose::StructArgument(typ.size().next_multiple_of(8) as u32),
            ))
        } else {
            for cl_type in typ.raw_arg_type(abi).to_cl_type() {
                sig.params.push(AbiParam::new(cl_type));
            }
        }
    }
    

    let raw_return = typ.ret.raw_return_type(abi).to_cl_type();

    if crate::ARCH_x86 && cfg!(windows) {
        if (abi == ABI::Rust || abi == ABI::None) && 
            typ.ret.return_style(ABI::Rust) == ReturnStyle::Direct &&
            typ.ret.primitive_fields_iter().next().is_some() &&
            typ.ret.primitive_fields_iter().all(|typ| typ.is_float()) {
            // I don't know why this works, but I tried it and it did.
            //
            // When this is removed, Rust ABI returns of {f32, f32}, {f64, f32},
            // {f32, f64}, and {f64, f64} no longer work.
            sig.call_conv = CallConv::Cold;
        } else {
            sig.call_conv = CallConv::WindowsFastcall;
        }
    }

    for (c, cl_type) in raw_return.into_iter().enumerate() {
        sig.returns.push(AbiParam::new(cl_type));
    }
}

pub trait ToCLType {
    fn to_cl_type(&self) -> Vec<ClType>;
}

impl ToCLType for Type {
    fn to_cl_type(&self) -> Vec<ClType> {
        self.as_type_enum().to_cl_type()
    }
}

impl ToCLType for TypeEnum {
    fn to_cl_type(&self) -> Vec<ClType> {
        match self {
            TypeEnum::Int(t) => t.to_cl_type(),
            TypeEnum::Float(t) => t.to_cl_type(),
            TypeEnum::Struct(t) => t.to_cl_type(),
            TypeEnum::Union(t) => t.to_cl_type(),
            TypeEnum::Ref(t) => t.to_cl_type(),
            TypeEnum::Func(t) => t.to_cl_type(),
            TypeEnum::Array(t) => t.to_cl_type(),
            TypeEnum::Destructor(DestructorType { base, .. }) => base.to_cl_type(),
            TypeEnum::Bool => (&BoolType).to_cl_type(),
            TypeEnum::Void => VoidType().to_cl_type(),
            TypeEnum::Unknown => UnknownType().to_cl_type(),
        }
    }
}

impl ToCLType for IntType {
    fn to_cl_type(&self) -> Vec<ClType> {
        vec![ClType::int(self.size.to_num() as u16).unwrap()]
    }
}

impl ToCLType for FloatType {
    fn to_cl_type(&self) -> Vec<ClType> {
        vec![
            match self {
                FloatType::F32 => cl_types::F32,
                FloatType::F64 => cl_types::F64,
            }
        ]
    }
}

impl ToCLType for RefType {
    fn to_cl_type(&self) -> Vec<ClType> {
        vec![cl_types::I64]
    }
}

impl ToCLType for FuncType {
    fn to_cl_type(&self) -> Vec<ClType> {
        vec![cl_types::I64]
    }
}

impl ToCLType for StructType {
    fn to_cl_type(&self) -> Vec<ClType> {
        self.fields
            .iter()
            .map(|Field { typ, .. }| typ.to_cl_type())
            .flatten()
            .collect()
    }
}

impl ToCLType for UnionType {
    fn to_cl_type(&self) -> Vec<ClType> {
        let size = self.largest_field().map(Type::size).unwrap_or(0);
        std::iter::once(cl_types::I8).cycle().take(size).collect()
    }
}

impl ToCLType for BoolType {
    fn to_cl_type(&self) -> Vec<ClType> {
        vec![cl_types::I8]
    }
}

impl ToCLType for ArrayType {
    fn to_cl_type(&self) -> Vec<ClType> {
        let base_types = self.base.to_cl_type();
        let base_type_count = base_types.len();
        base_types.into_iter().cycle().take(base_type_count * self.count as usize).collect()
    }
}

impl ToCLType for UnknownType {
    fn to_cl_type(&self) -> Vec<ClType> {
        panic!("Unknown type cannot be converted to Cranelift type")
    }
}

impl ToCLType for VoidType {
    fn to_cl_type(&self) -> Vec<ClType> {
        Vec::new()
    }
}
