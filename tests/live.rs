use cxc::Unit;
use serial_test::serial;

#[test]
#[serial]
fn hot_reload_one() {
    let mut unit = Unit::new();
    unit.push_script("the_best_num(); i32 { ; 41 } ").unwrap();
    let func = unit.get_fn("the_best_num").unwrap().downcast::<(), i32>();
    assert_eq!(func(), 41);

    unit.push_script("the_best_num(); i32 { ; 42 } ").unwrap();
    assert_eq!(func(), 42);
}

#[test]
#[serial]
fn hot_reload_many() {
    let mut unit = Unit::new();

    unit.push_script("the_best_num(); i32 { ; 41 }").unwrap();
    unit.push_script("the_worst_num(); i32 { ; 0 }").unwrap();

    let best = unit.get_fn("the_best_num").unwrap().downcast::<(), i32>();
    let worst = unit.get_fn("the_worst_num").unwrap().downcast::<(), i32>();
    assert_eq!(best(), 41);
    assert_eq!(worst(), 0);

    unit.push_script("the_best_num(); i32 { ; 42 } ").unwrap();
    assert_eq!(best(), 42);
    assert_eq!(worst(), 0);

    unit.push_script("the_worst_num(); i32 { ; 1 } ").unwrap();
    assert_eq!(best(), 42);
    assert_eq!(worst(), 1);

    unit.push_script("the_best_num(); i32 { ; 900 } ").unwrap();
    assert_eq!(best(), 900);
    assert_eq!(worst(), 1);
}
