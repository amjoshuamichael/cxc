#![feature(type_name_of_val)]
#![feature(int_roundings)]
#![feature(let_chains)]
#![feature(type_alias_impl_trait)]
#![feature(box_syntax)]
#![feature(type_changing_struct_update)]
#![feature(if_let_guard)]
#![feature(min_specialization)]
#![feature(unboxed_closures)]
#![feature(fn_traits)]
#![feature(tuple_trait)]

// HOW DEBUG FlAGS WORK:
// 1. xc-debug: enables debug printing for the compiler itself
// 2. llvm-debug: enables debug printing for the LLVM IR
// 3. show-bytes: enables debug printing for the bytes of each type, during
// tests using the xc_test! macro

pub static XC_DEBUG: bool = cfg!(feature = "xc-debug");
pub static LLVM_DEBUG: bool = cfg!(feature = "llvm-debug");

pub use lex::{TypeName, VarName};
pub use parse::{TypeDecl, TypeRelation};
pub use typ::{
    ArrayType, BoolType, FloatType, FuncType, IntType, Kind, RefType, Repr, StructType, Type,
    TypeData, TypeEnum,
};
pub use unit::{
    CompData, ExternalFuncAdd, Func, FuncDowncasted, UniqueFuncInfo, Unit, XcReflect, XcValue,
};

pub mod library {
    pub use crate::libraries::*;
    pub mod bit_array {
        pub use crate::libraries::std_lib::bit_array::BitArray;
    }
}

pub mod error {
    pub use crate::errors::CErr;
    pub use crate::parse::ParseError;
}

#[cfg(feature = "cxc_derive")]
pub use cxc_derive::{XcReflect, xc_opaque};

#[cfg(feature = "show-bytes")]
pub mod bytesof;
mod errors;
mod hlr;
mod lex;
mod libraries;
mod parse;
#[cfg(feature = "backend-llvm")]
mod to_llvm;
mod typ;
mod unit;
