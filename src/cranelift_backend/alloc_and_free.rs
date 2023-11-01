use cranelift::prelude::Signature;
use cranelift_module::Module;
use cranelift_jit::JITModule;

use crate::{Type, FuncType, typ::ABI};

use super::to_cl_type::func_type_to_signature;

pub struct AllocAndFree {
    pub alloc_ptr: *const usize,
    pub alloc_sig: Signature,
    pub free_ptr: *const usize,
    pub free_sig: Signature,
}

impl AllocAndFree {
    pub fn new(module: &JITModule) -> Self {
        let mut alloc_sig = module.make_signature();
        let alloc_type = FuncType { 
            args: vec![Type::i(64)], 
            ret: Type::i(8).get_ref(), 
            abi: ABI::C 
        };
        func_type_to_signature(&alloc_type, &mut alloc_sig);

        let mut free_sig = module.make_signature();
        let free_type = FuncType { 
            args: vec![Type::i(8).get_ref()], 
            ret: Type::void(), 
            abi: ABI::C 
        };
        func_type_to_signature(&free_type, &mut free_sig);

        Self {
            alloc_ptr: alloc_x_bytes as *const usize, 
            free_ptr: free as *const usize,
            alloc_sig,
            free_sig,
        }
    }
}

unsafe extern "C" fn alloc_x_bytes(x: i64) -> *const u8 {
    use std::alloc::*;
    alloc(Layout::from_size_align_unchecked(x as usize, 4))
}

unsafe extern "C" fn free(ptr: *mut u8) {
    use std::alloc::*;
    // this is undefined behavior!! we're using a layout that is only the size of 
    // the type of the pointer. if the user allocates more than one of a type, 
    // then this layout is wrong, which is undefined behavior under the rust 
    // allocation API. because the cxc allocation api does not require that the 
    // user specify the size of their deallocation, the only way to fix this in 
    // this backend is to stash the size of allocation for every allocation.
    //
    // TODO: we could also use libc
    let possibly_wrong_layout = Layout::from_size_align(4 as usize, 4).unwrap();
    dealloc(ptr, possibly_wrong_layout)
}
