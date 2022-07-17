use super::prelude::*;
use crate::parse::*;
use std::sync::Arc;

#[derive(Default, Clone)]
pub struct TypeGroup(pub Vec<Arc<BaseType>>);

impl TypeGroup {
    pub fn add(&mut self, t: BaseType) {
        let arc = Arc::new(t);
        self.0.push(arc);
    }

    pub fn get_base(&self, name: &String) -> Option<Arc<BaseType>> {
        for t in &self.0 {
            if t.name == *name {
                return Some(t.clone());
            }
        }

        None
    }

    pub fn get_spec(&self, type_spec: &TypeSpec) -> Option<Type> {
        Some(Type {
            base: self.get_base(&type_spec.name)?,
            ref_count: type_spec.ref_count,
        })
    }

    pub fn with_core_lib() -> TypeGroup {
        let mut type_group = TypeGroup::default();
        type_group.add_types(&*crate::core_lib::CORE_LIB);

        type_group
    }

    pub fn add_types(&mut self, rhs: &TypeGroup) {
        for t in &rhs.0 {
            if self.get_base(&t.name).is_none() {
                self.add((**t).clone())
            }
        }
    }
}
