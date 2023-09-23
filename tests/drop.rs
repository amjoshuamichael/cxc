use std::rc::Rc;

use cxc::{Unit, library::StdLib};

#[test]
fn basic_drop() {
    let mut unit = Unit::new();
    unit.add_lib(StdLib);

    unit.push_script(
        r#"
        clone_rc(the_rc: &Rc<i32>) {
           cloned := the_rc.clone()
        }
        "#,
    )
    .unwrap();

    let the_rc = Rc::new(49);

    unit.get_fn("clone_rc").unwrap().downcast::<(&Rc<i32>,), ()>()(&the_rc);

    assert_eq!(Rc::<i32>::strong_count(&the_rc), 1);
}
