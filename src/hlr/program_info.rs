use super::prelude::*;
use inkwell::basic_block::BasicBlock;
use inkwell::values::PointerValue;
use std::collections::HashMap;
use std::sync::Arc;

#[derive(Clone)]
pub struct ProgramInfo<'ctx> {
    pub types: TypeGroup,
    pub variables: HashMap<Arc<str>, PointerValue<'ctx>>,
    pub gotos: HashMap<Arc<str>, BasicBlock<'ctx>>,
}

impl<'ctx> Default for ProgramInfo<'ctx> {
    fn default() -> Self {
        ProgramInfo {
            types: TypeGroup::with_core_lib(),
            variables: HashMap::new(),
            gotos: HashMap::new(),
        }
    }
}

impl<'ctx> ProgramInfo<'ctx> {
    pub fn force_grab_goto(&self, name: &str) -> BasicBlock {
        let maybe_goto = self.gotos.get(name);

        match maybe_goto {
            Some(var) => var.clone(),
            None => panic!("could not find goto {name}"),
        }
    }
}
