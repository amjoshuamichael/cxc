use crate::parse::TypeRelation;

use crate::Type;
use crate::{ExternalFuncAdd, TypeData, XcValue};

use crate::libraries::Library;

pub(super) struct ValueLib;

impl Library for ValueLib {
    fn add_to_unit(&self, unit: &mut crate::Unit) {
        let value_type = unit.add_reflect_type::<XcValue>().unwrap();

        unit.add_rust_func_explicit(
            "from",
            make_a_val as *const usize,
            ExternalFuncAdd {
                arg_types: vec![Type::void_ptr()],
                ret_type: value_type.clone(),
                relation: TypeRelation::Static(value_type),
                ..ExternalFuncAdd::empty()
            }
            .reflect_variable_types(),
        );
    }
}

unsafe fn make_a_val(type_ptr: *const TypeData, data: *const u8) -> XcValue {
    let the_type = Type::from_raw(type_ptr);
    XcValue::new_from_ptr(the_type, data)
}
