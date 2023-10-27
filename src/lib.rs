#![allow(warnings)]
#![feature(type_name_of_val)]
#![feature(int_roundings)]
#![feature(let_chains)]
#![feature(if_let_guard)]
#![feature(unboxed_closures)]
#![feature(fn_traits)]
#![feature(tuple_trait)]
#![feature(exclusive_range_pattern)]
#![feature(concat_idents)]

// HOW THE DEBUG FlAGS WORK:
// 1. xc-debug: enables debug printing for the compiler itself
// 2. backend-debug: enables debug printing for backend, like LLVM or Cranelift
// 3. show-bytes: enables debug printing for the bytes of each type, during
// tests using the xc_test! macro

pub use lex::{TypeName, VarName};
pub use parse::{TypeDecl, TypeRelation};
pub use typ::{
    ArrayType, BoolType, FloatType, FuncType, IntType, Kind, RefType, Repr, StructType, Type,
    TypeData, TypeEnum, Field
};
pub use unit::{
    CompData, ExternalFuncAdd, Func, FuncDowncasted, FuncQuery, Unit, XcReflect, Value
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

pub use cxc_derive::{XcReflect, xc_opaque};

#[cfg(feature = "show-bytes")]
pub mod bytesof;
mod errors;
mod hlr;
mod mir;
mod lex;
mod libraries;
mod parse;
mod cache;

#[cfg(feature = "backend-llvm")]
mod llvm_backend;

#[cfg(feature = "backend-cranelift")]
mod cranelift_backend;

#[cfg(feature = "backend-interpreter")]
mod interpreter;

pub mod backend {
    #[cfg(feature = "backend-llvm")]
    pub use super::llvm_backend::LLVMBackend as Backend;

    #[cfg(feature = "backend-cranelift")]
    pub use super::cranelift_backend::CraneliftBackend as Backend;

    #[cfg(feature = "backend-interpreter")]
    pub use super::interpreter::InterpreterBackend as Backend;
}

mod typ;
mod unit;

#[cfg(not(any(feature = "backend-llvm", feature = "backend-cranelift", feature = "backend-interpreter")))]
compile_error!("No backend chosen: in your Cargo.toml, enable either backend-cranelift (recommended) or backend-llvm (requires LLVM to be installed)");

#[cfg(not(target_pointer_width = "64"))]
compile_error!("cxc only works for 64 bit architectures.");
