use super::prelude::*;
use inkwell::values::PointerValue;
use std::collections::HashMap;
use std::sync::Arc;

#[derive(Clone)]
pub struct ProgramInfo<'ctx> {
    pub types: TypeGroup,
    pub variables: HashMap<Arc<str>, PointerValue<'ctx>>,
}

impl<'ctx> Default for ProgramInfo<'ctx> {
    fn default() -> Self {
        ProgramInfo {
            types: TypeGroup::with_core_lib(),
            variables: HashMap::new(),
        }
    }
}
