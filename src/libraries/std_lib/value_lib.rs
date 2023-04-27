use crate::{libraries::Library, Unit, Value};

pub struct ValueLib;

impl Library for ValueLib {
    fn add_to_unit(&self, unit: &mut Unit) {
        unit.add_reflect_type::<Value>();
        unit.push_script(include_str!("./value.cxc")).unwrap();
    }
}
