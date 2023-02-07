mod crates;
pub mod std_lib;
mod test_lib;
mod type_interface_lib;
pub use crates::*;
pub use std_lib::StdLib;
pub use type_interface_lib::TypeInterfaceLib;

use crate::Unit;

pub trait Library {
    fn add_to_unit(&self, unit: &mut Unit);
}
