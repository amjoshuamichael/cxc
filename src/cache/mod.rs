use std::{sync::RwLock, cmp::Ordering};

use crate::XcReflect;

pub struct Cache<T> {
    calculated: RwLock<Option<T>>,
}

impl<T: XcReflect> XcReflect for Cache<T> {
    fn alias_code() -> String {
        let t = T::alias_code();
        format!("
        {{
            calculated: RwLock<Option<{t}>>,
        }}
        ")
    }
}

impl<T> Clone for Cache<T> {
    fn clone(&self) -> Self { Self::default() }
}

impl<T> Default for Cache<T> {
    fn default() -> Self {
        Cache { calculated: RwLock::new(None), }
    }
}

impl<T> std::hash::Hash for Cache<T> {
    fn hash<H: std::hash::Hasher>(&self, _: &mut H) { }
}

impl<T> PartialEq for Cache<T> {
    fn eq(&self, _: &Self) -> bool { true }
}

impl<T> PartialOrd for Cache<T> {
    fn partial_cmp(&self, _: &Self) -> Option<Ordering> { Some(Ordering::Equal) }
}

impl<T> Eq for Cache<T> { }
impl<T> Ord for Cache<T> { fn cmp(&self, _: &Self) -> Ordering { Ordering::Equal } }

impl<T: Copy> Cache<T> {
    pub fn retrieve_or_call(&self, call: impl FnOnce() -> T) -> T {
        if let Ok(lock) = self.calculated.try_read() &&
            let Some(cached) = *lock {
            cached
        } else {
            let calculated = call();

            if let Ok(mut lock) = self.calculated.try_write() {
                *lock = Some(calculated)
            }

            calculated
        }
    }
}
