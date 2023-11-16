pub fn main() {
    let mut unit = cxc::Unit::new();

    unit.add_lib(cxc::library::StdLib);

    unit.push_script(
        r#"
        main() {
            numbers := [4, 90, 32, 9]

            for numbers as number {
                print(*number)
            }
        }
        "#,
    )
    .unwrap();

    let hello_world = unit.get_fn("main").unwrap().downcast::<(), ()>();

    //hello_world();
}
