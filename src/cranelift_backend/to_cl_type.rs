use cranelift::codegen::ir::ArgumentPurpose;
use cranelift::prelude::AbiParam;
use cranelift::prelude::Signature;
use cranelift::prelude::Type as ClType;
use cranelift::prelude::types as cl_types;
use crate::ArrayType;
use crate::BoolType;
use crate::FuncType;
use crate::RefType;
use crate::TypeEnum;
use crate::typ::ReturnStyle;
use crate::typ::SumType;
use crate::typ::UnknownType;
use crate::typ::VariantType;
use crate::typ::VoidType;
use crate::{Type, IntType, FloatType, StructType};

pub fn func_type_to_signature(typ: &FuncType, sig: &mut Signature, as_rust: bool) {
    let return_style = if as_rust { 
        typ.ret.rust_return_style() 
    } else { 
        typ.ret.return_style() 
    };

    if return_style == ReturnStyle::Sret {
        let sret_abi = AbiParam::special(cl_types::I64, ArgumentPurpose::StructReturn);
        sig.params.push(sret_abi);
    }

    for typ in &typ.args {
        for cl_type in typ.raw_arg_type().to_cl_type() {
            sig.params.push(AbiParam::new(cl_type));
        }
    }

    let raw_return = if as_rust {
        typ.ret.rust_raw_return_type()
    } else { 
        typ.ret.raw_return_type()
    };

    for cl_type in raw_return.to_cl_type() {
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
            TypeEnum::Sum(t) => t.to_cl_type(),
            TypeEnum::Variant(t) => t.to_cl_type(),
            TypeEnum::Ref(t) => t.to_cl_type(),
            TypeEnum::Func(t) => t.to_cl_type(),
            TypeEnum::Array(t) => t.to_cl_type(),
            TypeEnum::Bool(t) => t.to_cl_type(),
            TypeEnum::Void => VoidType().to_cl_type(),
            TypeEnum::Unknown => UnknownType().to_cl_type(),
        }
    }
}

impl ToCLType for IntType {
    fn to_cl_type(&self) -> Vec<ClType> {
        vec![ClType::int(self.size as u16).unwrap()]
    }
}

impl ToCLType for FloatType {
    fn to_cl_type(&self) -> Vec<ClType> {
        vec![
            match self {
                FloatType::F16 => 
                    panic!("cranelift does not support half precision float types"),
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
            .map(|(_, typ)| typ.to_cl_type())
            .flatten()
            .collect()
    }
}

impl ToCLType for SumType {
    fn to_cl_type(&self) -> Vec<ClType> {
        let size = self.largest_variant_as_struct().size() as u32;

        match size {
            0..=8 => vec![ClType::int((size * 8) as u16).unwrap()],
            9..=16 => vec![cl_types::I32; size as usize / 4],
            17.. => {
                // TODO: enum variants like i1, i32, i32, i32, i32 won't work here
                let padding_size = size as usize - 4;
                vec![cl_types::I32; padding_size / 4 + 1]
            },
        }
    }
}

impl ToCLType for VariantType {
    fn to_cl_type(&self) -> Vec<ClType> {
        self.as_struct().to_cl_type()
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
