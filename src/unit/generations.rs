use std::collections::HashMap;

use super::{FuncQuery, FuncId};

#[derive(Clone, Default, Debug)]
pub struct Generations {
    func_to_gen: HashMap<FuncId, u32>,
}

impl Generations {
    pub fn update(&mut self, id: FuncId) -> bool {
        if let Some(gen) = self.func_to_gen.get_mut(&id) {
            *gen += 1;
            true
        } else {
            self.func_to_gen.insert(id, 0);
            false
        }
    }

    pub fn get_gen_of(&self, id: &FuncId) -> u32 {
        self.func_to_gen[id]
    }
}
