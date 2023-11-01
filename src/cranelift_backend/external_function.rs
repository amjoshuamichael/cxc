use cranelift::prelude::{types as cl_types, FunctionBuilder, InstBuilder, StackSlotKind, StackSlotData, Value as CLValue, Signature};
use cranelift_module::Module;


use crate::{FuncType, cranelift_backend::to_cl_type::{func_type_to_signature, ToCLType}, typ::ReturnStyle, unit::FuncId};

use super::CraneliftBackend;

pub fn add_external_func(
    backend: &mut CraneliftBackend, 
    info: FuncId, 
    func_type: FuncType, 
    ptr: *const usize
) {
    let mut ctx = backend.module.make_context();

    func_type_to_signature(&func_type, &mut ctx.func.signature);

    let sig = {
        // because of rust ABI rules, the rust function we're calling might have
        // a different return style than the equivalent cxc function
        let mut external_signature = backend.module.make_signature();
        func_type_to_signature(&func_type, &mut external_signature);
        external_signature
    };
    
    backend.external_functions.insert(info, ExternalFuncData {
        ptr,
        sig,
        typ: func_type,
    });
}

pub struct ExternalFuncData {
    pub ptr: *const usize,
    pub sig: Signature,
    typ: FuncType,
}
