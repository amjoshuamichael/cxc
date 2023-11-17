use super::{Library, TypeInterfaceLib};

pub struct StdLib;

mod atomic_lib;
pub mod bit_array;
mod default;
pub mod hash;
mod print_lib;
pub mod string;
mod to_string;
mod type_helpers;
mod unit_lib;
mod value_lib;
mod array_helper_funcs_lib;
use atomic_lib::AtomicLib;
use bit_array::BitArrayLib;
use default::DefaultLib;
use print_lib::PrintLib;
use string::StringLib;
use to_string::ToStringLib;
use type_helpers::TypeHelperLib;
use unit_lib::UnitLib;
use value_lib::ValueLib;
use self::array_helper_funcs_lib::ArrayHelperLib;

impl Library for StdLib {
    fn add_to_unit(&self, unit: &mut crate::Unit) {
        unit.push_script(include_str!("ptr.cxc")).unwrap();

        unit.add_lib(ArrayHelperLib);

        unit.push_script(include_str!("vec.cxc")).unwrap();
        unit.push_script(include_str!("vecdeque.cxc")).unwrap();
        unit.push_script(include_str!("hashmap.cxc")).unwrap();
        unit.push_script(include_str!("rc.cxc")).unwrap();
        unit.push_script(include_str!("lucid.cxc")).unwrap();
        unit.push_script(include_str!("range.cxc")).unwrap();
        unit.push_script(include_str!("slice.cxc")).unwrap();
        unit.push_script(include_str!("alloc.cxc")).unwrap();
        unit.push_script(include_str!("iter.cxc")).unwrap();

        unit.add_lib(StringLib);
        unit.add_lib(ToStringLib);
        unit.add_lib(DefaultLib);
        unit.add_lib(TypeInterfaceLib);
        unit.add_lib(PrintLib);
        unit.add_lib(BitArrayLib);
        unit.add_lib(TypeHelperLib);

        unit.add_lib(AtomicLib);
        unit.push_script(include_str!("arc.cxc")).unwrap();
        unit.push_script(include_str!("new.cxc")).unwrap();

        unit.add_lib(UnitLib);
        unit.add_lib(ValueLib);

        unit.add_external_default::<bool>();
    }
}
