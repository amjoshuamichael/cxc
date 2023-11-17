use crate::test_utils::{xc_test, Color};
use cxc::Unit;

#[test]
fn basic_enum() {
    xc_test!(
        r#"
            Color = { Red / Green / Blue }

            main(); [3]Color {
                ; [Color/Red, Color/Green, Color/Blue]
            }
        "#;
        [Color::Red, Color::Green, Color::Blue]
    )
}

macro_rules! enum_size_test {
    ($size:tt, $int:ty, $name:ident) => {
        #[test]
        fn $name() {
            let mut code = String::new();
            code += "MyEnum = { ";

            for n in 0..$size {
                code += &*format!("A{n} / ");
            }

            code += "} main(); [";
            code += stringify!($size);
            code += "]MyEnum { ; [";

            for n in 0..$size {
                code += &*format!("MyEnum/A{n}, ");
            }

            code += "] }";

            let mut unit = Unit::new();
            println!("{code}");
            unit.push_script(&*code).unwrap();
            let main = unit.get_fn("main").unwrap().downcast::<(), [$int; $size]>();
            let arr = main();

            for n in 0..$size {
                assert_eq!(n as $int, arr[n]);
            }
        }
    }
}

enum_size_test!(255, u8, enums_255);
enum_size_test!(256, u8, enums_256);
enum_size_test!(257, u16, enums_257);
enum_size_test!(6000, u16, enums_6000);
