use crate::{libraries::Library, ExternalFuncAdd, Unit, Type};

pub struct PrintLib;

impl Library for PrintLib {
    fn add_to_unit(&self, unit: &mut Unit) {
        unit.push_script(include_str!("print.cxc")).unwrap();
        let string = unit.comp_data.get_by_name(&"String".into()).unwrap();

        unit.add_rust_func_explicit(
            "external_print_string",
            external_print_string as *const usize,
            ExternalFuncAdd {
                arg_types: vec![string.get_ref()],
                ..ExternalFuncAdd::empty()
            },
        );

        unit.add_rust_func_explicit(
            "print_number",
            print_number as *const usize,
            ExternalFuncAdd {
                arg_types: vec![Type::i(64)],
                ..ExternalFuncAdd::empty()
            },
        );

        unit.add_rust_func_explicit(
            "panic",
            panic as *const usize,
            ExternalFuncAdd {
                arg_types: vec![string.get_ref()],
                ..ExternalFuncAdd::empty()
            },
        );
    }
}

fn external_print_string(string: &String) { println!("{string}") }
fn print_number(num: i64) { println!("{num}") }
fn panic(string: &String) { panic!("{string}") }
