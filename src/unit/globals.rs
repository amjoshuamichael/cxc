use crate::hlr::prelude::*;
use inkwell::values::AnyValueEnum;
use std::collections::HashMap;
use std::sync::Arc;

#[derive(Default)]
pub struct Globals<'a>(HashMap<Arc<str>, Global<'a>>);

struct Global<'a> {
    typ: Type,
    value: AnyValueEnum<'a>,
}

impl<'a> Globals<'a> {
    pub fn insert(&mut self, key: Arc<str>, value: AnyValueEnum<'a>, typ: Type) {
        self.0.insert(key, Global { value, typ });
    }

    pub fn exists(&self, key: &Arc<str>) -> bool {
        self.0.contains_key(key)
    }

    pub fn get_value(&self, key: String) -> Option<AnyValueEnum<'a>> {
        Some(self.0.get(&Arc::from(&*key))?.value.clone())
    }

    pub fn get_type(&self, key: String) -> Option<Type> {
        Some(self.0.get(&Arc::from(&*key))?.typ.clone())
    }
}
