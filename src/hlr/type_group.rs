use super::prelude::*;
use std::sync::Arc;

#[derive(Default, Clone)]
pub struct TypeGroup(pub Vec<Arc<BaseType>>);

impl TypeGroup {
    pub fn add(&mut self, t: BaseType) {
        let arc = Arc::new(t);
        self.0.push(arc);
    }

    pub fn get(&self, name: &String) -> Option<Type> {
        for t in &self.0 {
            if t.name == *name {
                return Some(t.into());
            }
        }

        None
    }

    pub fn get_spec(&self, type_spec: &(u8, String)) -> Option<Type> {
        let mut base = self.get(&type_spec.1)?;
        base.ref_count = type_spec.0;
        Some(base)
    }

    pub fn with_core_lib() -> TypeGroup {
        let mut type_group = TypeGroup::default();
        type_group.add_types(&*crate::core_lib::CORE_LIB);

        type_group
    }

    pub fn add_types(&mut self, rhs: &TypeGroup) {
        for t in &rhs.0 {
            if self.get(&t.name).is_none() {
                self.add((**t).clone())
            }
        }
    }
}
