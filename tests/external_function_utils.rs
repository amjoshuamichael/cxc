mod test_utils;
use cxc::ExternalFuncAdd;
use cxc::LLVMContext;
use cxc::Unit;
use test_utils::xc_test;

#[test]
fn default_util() {
    let context = LLVMContext::new();
    let mut unit = Unit::new(&context);

    let string_type = unit.add_opaque_type::<String>();

    unit.add_external_default::<String>(string_type);
    unit.push_script("main(): String { ; String:default() }");

    let default_string = unsafe { unit.get_fn_by_name::<(), String>("main")(()) };
    assert_eq!(default_string, String::default());
}

#[test]
fn clone_util() {
    let context = LLVMContext::new();
    let mut unit = Unit::new(&context);

    let string_type = unit.add_opaque_type::<String>();

    unit.add_external_clone::<String>(string_type.clone());

    unit.add_rust_func_explicit(
        "a_cool_string",
        a_cool_string as *const usize,
        ExternalFuncAdd {
            ret_type: string_type.clone(),
            ..ExternalFuncAdd::empty()
        },
    );

    unit.push_script(
        "
        main(): String { 
            cool_string: String = a_cool_string()
            ; cool_string.clone()
        }
    ",
    );

    let default_string = unsafe { unit.get_fn_by_name::<(), String>("main")(()) };
    assert_eq!(default_string, a_cool_string());
}

fn a_cool_string() -> String { String::from("coolman") }
