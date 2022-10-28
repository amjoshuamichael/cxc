mod test_lib;
mod type_interface_lib;
pub use test_lib::TestLib;
pub use type_interface_lib::TypeInterfaceLib;

use crate::Unit;

pub trait Library {
    fn add_to_unit(&self, unit: &mut Unit);
}
