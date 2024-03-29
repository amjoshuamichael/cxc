use crate::{ExternalFuncAdd, Type, TypeRelation};

use crate::libraries::Library;

pub struct StringLib;

impl crate::XcReflect for String {
    fn spec_code() -> String { "String = { Vec<u8>, }".to_string() }
}

impl Library for StringLib {
    fn add_to_unit(&self, unit: &mut crate::Unit) {
        if unit.comp_data.get_spec(&"Vec<u8>".into(), &()).is_err() {
            unit.push_script(include_str!("vec.cxc")).unwrap();
        }

        let string_type = unit.add_reflect_type::<String>().unwrap();

        unit.add_external_default::<String>();
        unit.add_external_clone::<String>();

        unit.add_rust_func_explicit(
            "new",
            String::new as *const usize,
            ExternalFuncAdd {
                ret_type: string_type.clone(),
                relation: TypeRelation::Static(string_type.clone()),
                ..ExternalFuncAdd::empty()
            },
        );

        unit.add_rust_func_explicit(
            "_from_bytes",
            string_from_bytes as *const usize,
            ExternalFuncAdd {
                ret_type: string_type.clone(),
                arg_types: vec![Type::u(8).get_ref(), Type::i(64)],
                relation: TypeRelation::Static(string_type.clone()),
                ..ExternalFuncAdd::empty()
            },
        );

        unit.add_rust_func_explicit(
            "push_string",
            push_string as *const usize,
            ExternalFuncAdd {
                arg_types: vec![string_type.get_ref(), string_type.get_ref()],
                relation: TypeRelation::MethodOf(string_type.get_ref()),
                ..ExternalFuncAdd::empty()
            },
        );

        unit.push_script(include_str!("string.cxc")).unwrap();
    }
}

fn string_from_bytes(buf: *mut u8, length: usize) -> String {
    let slice = unsafe { std::slice::from_raw_parts(buf, length) };
    let x = String::from_utf8_lossy(slice).to_string();
    x
}

fn push_string(this: &mut String, other: &String) { 
    *this += &**other; 
}
