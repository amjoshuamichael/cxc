use crate::typ::DestructorType;
use crate::{TypeEnum, Type, typ::fields_iter::PrimitiveFieldsIter, XcReflect};

use crate::{self as cxc, IntType, FloatType};

use super::{ABI, IntSize};

#[derive(PartialEq, Eq, Debug, XcReflect)]
pub enum ReturnStyle {
    Direct,
    ThroughI32,
    ThroughI64,
    ThroughI32I32,
    ThroughI64I32,
    ThroughI64I64,
    ThroughF64,
    ThroughF32F32,
    ThroughF64F32,
    ThroughF64F64,
    SRet,
    Void,
}

#[derive(PartialEq, Eq, Debug, XcReflect)]
pub enum ArgStyle {
    Direct,
    Ints(IntSize, Option<IntSize>),
    Floats(FloatType, Option<FloatType>),
    Pointer,
}

pub fn realize_return_style(return_style: ReturnStyle, on: &Type) -> Type {
    match return_style {
        ReturnStyle::ThroughI32 => Type::i(32),
        ReturnStyle::ThroughI64 => Type::i(64),
        ReturnStyle::ThroughI32I32 => Type::new_tuple(vec![Type::i(32); 2]),
        ReturnStyle::ThroughI64I32 => Type::new_tuple(vec![Type::i(64), Type::i(32)]),
        ReturnStyle::ThroughI64I64 => Type::new_tuple(vec![Type::i(64); 2]),
        ReturnStyle::ThroughF64 => Type::f(64),
        ReturnStyle::ThroughF32F32 => Type::new_tuple(vec![Type::f(32); 2]),
        ReturnStyle::ThroughF64F32 => Type::new_tuple(vec![Type::f(64), Type::f(32)]),
        ReturnStyle::ThroughF64F64 => Type::new_tuple(vec![Type::f(64), Type::f(64)]),
        ReturnStyle::SRet | ReturnStyle::Void => Type::void(),
        ReturnStyle::Direct => on.clone(),
    }
}

pub fn realize_arg_style(arg_style: ArgStyle, on: &Type) -> Type {
    match arg_style {
        ArgStyle::Direct => on.clone(),
        ArgStyle::Pointer => on.get_ref(),
        ArgStyle::Ints(l, None) => Type::new(TypeEnum::Int(IntType { size: l, signed: false })),
        ArgStyle::Floats(l, None) => Type::new(TypeEnum::Float(l)),
        ArgStyle::Ints(l, Some(r)) => Type::new_tuple(vec![
            Type::new(TypeEnum::Int(IntType { size: l, signed: false })),
            Type::new(TypeEnum::Int(IntType { size: r, signed: false })),
        ]),
        ArgStyle::Floats(l, Some(r)) => Type::new_tuple(vec![
            Type::new(TypeEnum::Float(l)),
            Type::new(TypeEnum::Float(r)),
        ]),
    }
}


const MAX_REG_SIZE: usize = if cfg!(unix) { 16 } else { 8 };

impl Type {
    pub fn return_style(&self, abi: ABI) -> ReturnStyle {
        use TypeEnum::*;

        dbg!(self);
        match self.as_type_enum() {
            Int(_) | Ref(_) | Float(_) | Bool | Func(_) => ReturnStyle::Direct,
            Struct(_) | Array(_) => {
                // only take five to avoid spending time 
                // taking huge chunks of large arrays
                let fields = self.primitive_fields_iter().take(5).collect::<Vec<_>>();

                let size = self.size();

                let all_floats = fields.iter().all(Type::is_float);

                if abi == ABI::Rust {
                    if size > 8 && fields.len() >= 3 {
                        return ReturnStyle::SRet;
                    } else if fields.len() <= 2 {
                        return ReturnStyle::Direct;
                    }
                }

                if crate::ARCH_ARM &&
                    all_floats && 
                    fields.windows(2).all(|f| f[0] == f[1]) &&
                    fields.len() <= 4 {
                    return ReturnStyle::Direct;
                }

                if size > MAX_REG_SIZE {
                    return ReturnStyle::SRet;
                }

                if crate::ARCH_x86 && cfg!(windows) {
                    if abi == ABI::Rust && all_floats {
                        return ReturnStyle::Direct;
                    } else if abi == ABI::C && size % 4 != 0 && fields.len() >= 3 {
                        return ReturnStyle::SRet;
                    }
                }
                 
                if fields.len() == 1 {
                    ReturnStyle::Direct
                } else if all_floats && crate::ARCH_x86 && cfg!(unix)  {
                    match size {
                        0..=4 => ReturnStyle::ThroughI32,
                        4..=8 => ReturnStyle::ThroughF64,
                        8..=12 => ReturnStyle::ThroughF64F32,
                        _ => ReturnStyle::ThroughF64F64,
                    }
                } else {
                    match size {
                        0..=4 => ReturnStyle::ThroughI32,
                        4..=8 => ReturnStyle::ThroughI64,
                        8..=12 => ReturnStyle::ThroughI64I32,
                        _ => ReturnStyle::ThroughI64I64,
                    }
                }
            },
            Void => ReturnStyle::Void,
            Destructor(DestructorType { base, .. }) => base.return_style(abi),
            Unknown => panic!("cannot return unknown type"),
        }
    }

    pub fn arg_style(&self, abi: ABI) -> ArgStyle {
        use TypeEnum::*;

        match self.as_type_enum() {
            Int(_) | Ref(_) | Float(_) | Bool | Func(_) => ArgStyle::Direct,
            _ => {
                let size = self.size();

                // only take five to avoid spending time 
                // taking huge chunks of large arrays
                let fields = PrimitiveFieldsIter::new(self.clone())
                    .take(5).collect::<Vec<_>>();

                if abi == ABI::Rust {
                    match fields.len() {
                        0..=2 => {
                            match size {
                                0..=16 => ArgStyle::Direct,
                                17.. => {
                                    ArgStyle::Pointer
                                },
                                _ => unreachable!()
                            }
                        },
                        3.. => {
                            match size {
                                0 => ArgStyle::Direct,
                                1..=8 => {
                                    let int_size = (size * 8).next_power_of_two();
                                    ArgStyle::Ints(IntSize::from_num(int_size), None)
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
                    let all_floats = fields.iter().all(Type::is_float);

                    match self.size() {
                        _ if fields.len() == 0 || fields.len() == 1 => ArgStyle::Direct,
                        s if crate::ARCH_x86 && cfg!(windows) && s % 4 != 0 && fields.len() >= 3 => {
                            ArgStyle::Pointer
                        },
                        _ if crate::ARCH_ARM && all_floats &&
                            fields.windows(2).all(|f| f[0] == f[1]) &&
                            fields.len() <= 4 => {
                            ArgStyle::Direct
                        },
                        s if s > MAX_REG_SIZE => {
                            ArgStyle::Pointer
                        },
                        1..=8 => {
                            let first_size = (size * 8).next_power_of_two();

                            if all_floats && crate::ARCH_x86 && cfg!(unix) {
                                ArgStyle::Floats(FloatType::from(first_size), None)
                            } else {
                                ArgStyle::Ints(IntSize::from_num(first_size), None)
                            }
                        },
                        9..=16 => {
                            let second_size = (size * 8 - 64).next_power_of_two();

                            if all_floats && crate::ARCH_x86 && cfg!(unix) {
                                ArgStyle::Floats(
                                    FloatType::F64, 
                                    Some(FloatType::from(second_size))
                                )
                            } else {
                                ArgStyle::Ints(
                                    IntSize::_64, 
                                    Some(IntSize::from_num(second_size))
                                )
                            }
                        },
                        _ => unreachable!(),
                    }
                }
            }
        }
    }
}
