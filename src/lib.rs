#![allow(dead_code)]
#![feature(type_name_of_val)]
#![feature(let_chains)]
#![feature(type_alias_impl_trait)]
#![feature(box_syntax)]

pub static DEBUG: bool = false;

pub use parse::TypeRelation;
pub use typ::{
    ArrayType, BoolType, FloatType, FuncType, IntType, Kind, OpaqueType, RefType,
    StructType, Type, TypeData, TypeEnum,
};
pub use unit::{
    ExternalFuncAdd, Func, LLVMContext, UniqueFuncInfo, Unit, XcFunc, XcReflect,
    XcValue,
};

pub mod library {
    pub use crate::libraries::*;
}

#[cfg(feature = "cxc_derive")]
pub use cxc_derive::XcReflect;

mod hlr;
mod lex;
mod libraries;
mod parse;
mod to_llvm;
mod typ;
mod unit;
