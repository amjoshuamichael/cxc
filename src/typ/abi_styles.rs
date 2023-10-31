use crate::typ::DestructorType;
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
            Struct(_) | Array(_) => {
                let size = self.size();

                if abi == ABI::Rust {
                    if size > 8 && fields.len() >= 3 {
                        return ReturnStyle::SRet;
                    } else if fields.len() <= 2 {
                        return ReturnStyle::Direct;
                    }
                }

                if crate::ARCH_ARM &&
                    fields.iter().all(Type::is_float) && 
                    fields.windows(2).all(|f| f[0] == f[1]) &&
                    fields.len() <= 4 {
                    return ReturnStyle::Direct;
                }

                const MAX_RET_SIZE: usize = if crate::ARCH_ARM { 16 } else { 8 };

                if size > MAX_RET_SIZE {
                    return ReturnStyle::SRet;
                }

                if crate::ARCH_x86 {
                    if abi == ABI::Rust && fields.iter().all(Type::is_float) {
                        return ReturnStyle::Direct;
                    } else if abi == ABI::C && size % 4 != 0 && fields.len() >= 3 {
                        return ReturnStyle::SRet;
                    }
                }
                 
                if fields.len() == 1 {
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
                                    let int_size = (size * 8).next_power_of_two() as u32;
                                    let int_type = IntType::new(int_size, false);
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
                        _ if fields.len() == 0 || fields.len() == 1 => ArgStyle::Direct,
                        s if crate::ARCH_x86 && s % 4 != 0 && fields.len() >= 3 => {
                            ArgStyle::Pointer
                        },
                        _ if crate::ARCH_ARM && 
                            fields.iter().all(Type::is_float) && 
                            fields.windows(2).all(|f| f[0] == f[1]) &&
                            fields.len() <= 4 => {
                            ArgStyle::Direct
                        },
                        1..=8 => {
                            let int_size = (size * 8).next_power_of_two() as u32;
                            let int_type = IntType::new(int_size, false);
                            ArgStyle::Ints(int_type, None)
                        },
                        9..=16 if crate::ARCH_ARM => {
                            let first_int_type = IntType::new(64, false);
                            let second_int_size = (size * 8 - 64).next_power_of_two() as u32;
                            let second_int_type = IntType::new(second_int_size, false);
                            ArgStyle::Ints(first_int_type, Some(second_int_type))
                        },
                        _ => {
                            ArgStyle::Pointer
                        },
                    }
                }
            }
        }
    }
}
