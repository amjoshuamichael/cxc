use std::rc::Rc;

use crate::test_utils::Numbers5;
mod test_utils;

use cxc::{Unit, library::StdLib};

static mut GNUM: i32 = 0;

#[test]
fn basic_global() {
    let mut unit = Unit::new();

    unit.add_global("x".into(), unsafe { &mut GNUM as *mut _ });

    unit.push_script(
        r#"
        main() {
            x.write(50)
        }
        "#,
    )
    .unwrap();

    unit.get_fn("main").unwrap().downcast::<(), ()>()();

    assert_eq!(unsafe { GNUM }, 50);
}

#[test]
fn big_global() {
    let mut unit = Unit::new();

    let mut large_value = Numbers5::default();
    unit.add_global("large_value".into(), &mut large_value as *mut _);

    unit.push_script(
        r#"
        main() {
            large_value.d = 60
        }
        "#,
    )
    .unwrap();

    unit.get_fn("main").unwrap().downcast::<(), ()>()();

    assert_eq!(large_value.d, 60);
}

#[test]
fn global_rc() {
    let mut unit = Unit::new();
    unit.add_lib(StdLib);

    let mut large_value = Rc::new(Numbers5::default());
    unit.add_reflect_type::<Numbers5>();
    unit.add_global("large_value".into(), &mut large_value as *mut _);

    unit.push_script(
        r#"
        ---

        large_value.b = 89

        "#,
    )
    .unwrap();

    assert_eq!(large_value.b, 89);
}

#[test]
fn comp_script() {
    let mut unit = Unit::new();

    let mut large_value = Numbers5::default();
    unit.add_global("large_value".into(), &mut large_value as *mut _);

    unit.push_script(
        r#"

        double(x: i32); i32 { ; x * 2 }

        ---

        x = 30
        y = double(x)
        large_value.c = y

        "#,
    )
    .unwrap();

    assert_eq!(large_value.c, 60);
}
