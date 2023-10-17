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

    func_type_to_signature(&func_type, &mut ctx.func.signature, false);

    let sig = {
        // because of rust ABI rules, the rust function we're calling might have
        // a different return style than the equivalent cxc function
        let mut external_signature = backend.module.make_signature();
        func_type_to_signature(&func_type, &mut external_signature, true);
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

pub fn build_external_func_call(
    builder: &mut FunctionBuilder,
    mut args: Vec<CLValue>,
    func_data: &ExternalFuncData,
) -> Vec<CLValue> {
    let sigref = builder.func.import_signature(func_data.sig.clone());
    let func_val = builder.ins().iconst(cl_types::I64, func_data.ptr as i64);

    let ret_vals: Vec<CLValue> = if func_data.typ.ret.return_style() == ReturnStyle::SRet {
        builder.ins().call_indirect(sigref, func_val, &*args);

        vec![]
    } else if func_data.typ.ret.rust_return_style() == ReturnStyle::SRet {
        let stack_slot = builder.create_sized_stack_slot(StackSlotData {
            kind: StackSlotKind::ExplicitSlot,
            size: func_data.typ.ret.size() as u32,
        });

        let stack_addr = builder.ins().stack_addr(cl_types::I64, stack_slot, 0);

        args.insert(0, stack_addr);
        builder.ins().call_indirect(sigref, func_val, &*args);

        // copy val to stack so we can return it
        
        let first_ret = builder.ins().stack_load(cl_types::I64, stack_slot, 0);

        let second_ret_type = match func_data.typ.ret.return_style() {
            ReturnStyle::ThroughI64I32 => cl_types::I32,
            ReturnStyle::ThroughI64I64 => cl_types::I64,
            _ => unreachable!()
        };
        let second_ret = builder.ins().stack_load(second_ret_type, stack_slot, 8);

        vec![first_ret, second_ret]
    } else if func_data.typ.ret.return_style() != func_data.typ.ret.rust_return_style() {
        let stack_slot = builder.create_sized_stack_slot(StackSlotData {
            kind: StackSlotKind::ExplicitSlot,
            size: func_data.typ.ret.size() as u32,
        });

        let call = builder.ins().call_indirect(sigref, func_val, &*args);
        let call_ret_vals = builder.inst_results(call).to_vec();

        let mut stack_offset = 0;
        for (t, typ) in 
            func_data.typ.ret.rust_raw_return_type().to_cl_type().into_iter().enumerate() {
            let val = call_ret_vals[t];
            builder.ins().stack_store(val, stack_slot, stack_offset as i32);
            stack_offset += typ.bytes();
        }

        let mut ret_vals = Vec::new();
        let mut stack_offset = 0;

        for typ in func_data.typ.ret.raw_return_type().to_cl_type() {
            let val = builder.ins().stack_load(typ, stack_slot, stack_offset as i32);
            ret_vals.push(val);
            stack_offset += typ.bytes();
        }

        ret_vals
    } else {
        let call = builder.ins().call_indirect(sigref, func_val, &*args);
        builder.inst_results(call).to_vec()
    };

    ret_vals
}
