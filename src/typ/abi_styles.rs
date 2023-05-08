use crate::{TypeEnum, Type, typ::fields_iter::PrimitiveFieldsIter, XcReflect};

use crate as cxc;

#[derive(PartialEq, Eq, Debug, XcReflect)]
pub enum ReturnStyle {
    Direct,
    ThroughI32,
    ThroughI64,
    MoveIntoDouble,
    ThroughI32I32,
    ThroughF32F32,
    ThroughI64I32,
    ThroughI64I64,
    MoveIntoI64I64,
    Sret,
    Void,
}

#[derive(PartialEq, Eq, Debug, XcReflect)]
pub enum ArgStyle {
    Direct,
    Pointer,
}

pub fn realize_return_style(return_style: ReturnStyle, on: &Type) -> Type {
    match return_style {
        ReturnStyle::ThroughI32 => Type::i(32),
        ReturnStyle::ThroughI64 => Type::i(64),
        ReturnStyle::MoveIntoDouble => Type::f(64),
        ReturnStyle::ThroughI32I32 => Type::new_tuple(vec![Type::i(32); 2]),
        ReturnStyle::ThroughF32F32 => Type::new_tuple(vec![Type::f(32); 2]),
        ReturnStyle::ThroughI64I32 => 
            Type::new_tuple(vec![Type::i(64), Type::i(32)]),
        ReturnStyle::ThroughI64I64 | ReturnStyle::MoveIntoI64I64 => 
            Type::new_tuple(vec![Type::i(64); 2]),
        ReturnStyle::Sret | ReturnStyle::Void => Type::void(),
        ReturnStyle::Direct => on.clone(),
    }
}

pub fn realize_arg_style(arg_style: ArgStyle, on: &Type) -> Type {
    match arg_style {
        ArgStyle::Direct => on.clone(),
        ArgStyle::Pointer => on.get_ref(),
    }
}

impl Type {
    pub fn rust_return_style(&self) -> ReturnStyle {
        if self.is_void() {
            return ReturnStyle::Void;
        }

        let size = self.size();
        let fields = self.primitive_fields_iter().take(4).collect::<Vec<_>>();

        if fields.len() == 2 && size > 4 && size <= 8 &&
            fields[0].size() <= 4 && fields[1].size() <= 4 {
            if fields[0] == Type::f(32) && fields[1] == Type::f(32) {
                ReturnStyle::ThroughF32F32
            } else {
                ReturnStyle::ThroughI32I32
            }
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
}

impl Type {
    pub fn arg_style(&self) -> ArgStyle {
        use TypeEnum::*;

        match self.as_type_enum() {
            Int(_) | Ref(_) | Float(_) | Bool(_) | Func(_) => ArgStyle::Direct,
            _ => if self.size() <= 8 {
                ArgStyle::Direct
            } else {
                ArgStyle::Pointer
            },
        }
    }
}
