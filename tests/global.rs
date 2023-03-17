use crate::test_utils::Numbers5;
use std::rc::Rc;
mod test_utils;

use cxc::Unit;

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
