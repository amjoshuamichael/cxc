use crate::{libraries::Library, XcReflect};
use std::alloc::Layout;

pub struct AllocLib;

impl XcReflect for Layout {
    fn alias_code() -> String { "Layout = { size: u64, align: Alignment }".into() }
}

impl Library for AllocLib {
    fn add_to_unit(&self, unit: &mut crate::Unit) {
        unit.push_script("Alignment = u64");
        unit.add_reflect_type::<Layout>();
        unit.push_script(include_str!("alloc.cxc"));
    }
}
