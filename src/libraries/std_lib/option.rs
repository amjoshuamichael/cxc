use crate::libraries::Library;

pub struct OptionLib;

impl Library for OptionLib {
    fn add_to_unit(&self, unit: &mut crate::Unit) {
        unit.push_script(include_str!("option.cxc"));
    }
}
