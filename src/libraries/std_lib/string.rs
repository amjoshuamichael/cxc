use crate::{ExternalFuncAdd, Type, TypeRelation};

use crate::libraries::Library;

pub(super) struct StringLib;

impl Library for StringLib {
    fn add_to_unit(&self, unit: &mut crate::Unit) {
        let string_type = unit.add_opaque_type::<String>();

        unit.add_external_default::<String>(string_type.clone());
        unit.add_external_clone::<String>(string_type.clone());

        unit.add_rust_func_explicit(
            "new",
            String::new as *const usize,
            ExternalFuncAdd {
                ret_type: string_type.clone(),
                ..ExternalFuncAdd::empty()
            },
        );

        unit.add_rust_func_explicit(
            "from_bytes",
            string_from_bytes as *const usize,
            ExternalFuncAdd {
                ret_type: string_type.clone(),
                arg_types: vec![Type::void_ptr(), Type::i(64)],
                relation: TypeRelation::Static(string_type.clone()),
                ..ExternalFuncAdd::empty()
            },
        );

        unit.add_rust_func_explicit(
            "push_string",
            push_string as *const usize,
            ExternalFuncAdd {
                arg_types: vec![string_type.clone(), string_type.get_ref()],
                relation: TypeRelation::MethodOf(string_type.get_ref()),
                ..ExternalFuncAdd::empty()
            },
        );
    }
}

fn string_from_bytes(buf: *mut u8, length: usize) -> String {
    let slice = unsafe { std::slice::from_raw_parts(buf, length) };
    String::from_utf8_lossy(&slice).to_string()
}

fn push_string(other: &String, this: &mut String) { *this += &**other; }
