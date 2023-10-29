use crate::{TypeEnum, Type, typ::fields_iter::PrimitiveFieldsIter, XcReflect};

use crate::{self as cxc, IntType, FloatType};

use super::ABI;

#[derive(PartialEq, Eq, Debug, XcReflect)]
pub enum ReturnStyle {
    Direct,
    ThroughI32,
    ThroughI64,
    ThroughDouble,
    ThroughI32I32,
    ThroughF32F32,
    ThroughI64I32,
    ThroughI64I64,
    SRet,
    Void,
}

#[derive(PartialEq, Eq, Debug, XcReflect)]
pub enum ArgStyle {
    Direct,
    Ints(IntType, Option<IntType>),
    Pointer,
}

pub fn realize_return_style(return_style: ReturnStyle, on: &Type) -> Type {
    match return_style {
        ReturnStyle::ThroughI32 => Type::i(32),
        ReturnStyle::ThroughI64 => Type::i(64),
        ReturnStyle::ThroughDouble => Type::f(64),
        ReturnStyle::ThroughI32I32 => Type::new_tuple(vec![Type::i(32); 2]),
        ReturnStyle::ThroughF32F32 => Type::new_tuple(vec![Type::f(32); 2]),
        ReturnStyle::ThroughI64I32 => 
            Type::new_tuple(vec![Type::i(64), Type::i(32)]),
        ReturnStyle::ThroughI64I64 => 
            Type::new_tuple(vec![Type::i(64); 2]),
        ReturnStyle::SRet | ReturnStyle::Void => Type::void(),
        ReturnStyle::Direct => on.clone(),
    }
}

pub fn realize_arg_style(arg_style: ArgStyle, on: &Type) -> Type {
    match arg_style {
        ArgStyle::Direct => on.clone(),
        ArgStyle::Pointer => on.get_ref(),
        ArgStyle::Ints(l, None) => Type::new(TypeEnum::Int(l)),
        ArgStyle::Ints(l, Some(r)) => Type::new_tuple(vec![
            Type::new(TypeEnum::Int(l)),
            Type::new(TypeEnum::Int(r)),
        ]),
    }
}

impl Type {
    pub fn return_style(&self, abi: ABI) -> ReturnStyle {
        use TypeEnum::*;

        // only take five to avoid spending time 
        // taking huge chunks of large arrays
        let fields = self.primitive_fields_iter().take(5).collect::<Vec<_>>();

        match self.as_type_enum() {
            Int(_) | Ref(_) | Float(_) | Bool | Func(_) => ReturnStyle::Direct,
            Struct(_) | Array(_) | Destructor(_) => {
                let size = self.size();

                if abi == ABI::Rust && size > 8 && fields.len() >= 3 {
                    ReturnStyle::SRet
                } else if abi == ABI::Rust && fields.len() <= 2 {
                    ReturnStyle::Direct
                } else if size > 8 {
                    ReturnStyle::SRet
                } else if abi == ABI::Rust && fields.iter().all(Type::is_float) {
                    ReturnStyle::Direct
                } else if abi == ABI::C && size % 4 != 0 && fields.len() >= 3 {
                    ReturnStyle::SRet
                } else if fields.len() == 1 {
                    ReturnStyle::Direct
                } else if size > 12 {
                    ReturnStyle::ThroughI64I64
                } else if size > 8 {
                    ReturnStyle::ThroughI64I32
                } else if size > 4 {
                    ReturnStyle::ThroughI64
                } else {
                    ReturnStyle::ThroughI32
                }
            },
            Void => ReturnStyle::Void,
            Unknown => panic!("cannot return unknown type"),
        }
    }

    pub fn arg_style(&self, abi: ABI) -> ArgStyle {
        use TypeEnum::*;

        match self.as_type_enum() {
            Int(_) | Ref(_) | Float(_) | Bool | Func(_) => ArgStyle::Direct,
            _ => {
                let size = (self.size() * 8) as u32;

                // only take five to avoid spending time 
                // taking huge chunks of large arrays
                let fields = PrimitiveFieldsIter::new(self.clone())
                    .take(5).collect::<Vec<_>>();

                if abi == ABI::Rust {
                    match fields.len() {
                        0..=2 => {
                            match self.size() {
                                0..=16 => ArgStyle::Direct,
                                17.. => {
                                    ArgStyle::Pointer
                                },
                                _ => unreachable!()
                            }
                        },
                        3.. => {
                            match self.size() {
                                0 => ArgStyle::Direct,
                                1..=8 => {
                                    let int_type = 
                                        IntType::new(size.next_power_of_two(), false);
                                    ArgStyle::Ints(int_type, None)
                                },
                                9.. => {
                                    ArgStyle::Pointer
                                }
                                _ => unreachable!()
                            }
                        },
                        _ => unreachable!()
                    }
                } else {
                    match self.size() {
                        0 => ArgStyle::Direct,
                        _ if fields.len() == 1 && fields[0].is_float() => {
                            ArgStyle::Direct
                        },
                        s if s % 4 != 0 && fields.len() >= 3 => {
                            ArgStyle::Pointer
                        },
                        1..=8 => {
                            let int_type = IntType::new(size.next_power_of_two(), false);
                            ArgStyle::Ints(int_type, None)
                        },
                        9.. => {
                            ArgStyle::Pointer
                        },
                        _ => unreachable!()
                    }
                }
            }
        }
    }
}
