pub fn main() {
    let mut unit = cxc::Unit::new();

    unit.add_lib(cxc::library::StdLib);

    unit.push_script(
        r#"
        hello_world() {
            print("hello_world")
        }
        "#,
    )
    .unwrap();

    let hello_world = unit.get_fn_by_name::<(), ()>("hello_world");
    unsafe { hello_world(()) }
}
