use std::collections::HashMap;

use super::UniqueFuncInfo;

#[derive(Clone, Default, Debug)]
pub struct Generations {
    func_to_gen: HashMap<UniqueFuncInfo, u32>,
}

impl Generations {
    pub fn update(&mut self, info: UniqueFuncInfo) -> bool {
        if let Some(gen) = self.func_to_gen.get_mut(&info) {
            *gen += 1;
            true
        } else {
            self.func_to_gen.insert(info, 0);
            false
        }
    }

    pub fn get_gen_of(&self, info: &UniqueFuncInfo) -> u32 {
        self.func_to_gen.get(info).copied().unwrap_or(0)
    }
}
