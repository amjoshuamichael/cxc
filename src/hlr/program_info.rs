use super::prelude::*;
use inkwell::basic_block::BasicBlock;
use inkwell::values::PointerValue;
use std::collections::HashMap;

#[derive(Clone)]
pub struct ProgramInfo<'ctx> {
    pub types: TypeGroup,
    pub variables: HashMap<String, PointerValue<'ctx>>,
    pub gotos: HashMap<String, BasicBlock<'ctx>>,
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
    pub fn force_grab_var(&self, name: &String) -> PointerValue {
        let maybe_var = self.variables.get(name);

        match maybe_var {
            Some(var) => var.clone(),
            None => panic!("could not find var {name}"),
        }
    }

    pub fn force_grab_goto(&self, name: &String) -> BasicBlock {
        let maybe_goto = self.gotos.get(name);

        match maybe_goto {
            Some(var) => var.clone(),
            None => panic!("could not find goto {name}"),
        }
    }
}
