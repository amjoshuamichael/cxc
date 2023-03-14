pub fn main() {
    let mut unit = cxc::Unit::new();

    unit.add_lib(cxc::library::StdLib);

    unit.push_script(
        r#"
        main() {
            print("Hello, World!")
        }
        "#,
    )
    .unwrap();

    let hello_world = unit.get_fn("main").unwrap().downcast::<(), ()>();
    hello_world()
}
