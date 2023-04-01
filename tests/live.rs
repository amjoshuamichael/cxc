use cxc::{Unit, library::StdLib, FuncType, Func, Type, UniqueFuncInfo};
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

#[test]
#[serial]
fn depended_on() {
    let mut unit = Unit::new();

    unit.push_script("the_best_num(); i32 { ; 41 }").unwrap();
    unit.push_script("double_that(); i32 { ; the_best_num() * 2 }")
        .unwrap();
    let doubled = unit.get_fn("double_that").unwrap().downcast::<(), i32>();
    let best = unit.get_fn("the_best_num").unwrap().downcast::<(), i32>();
    assert_eq!(best(), 41);
    assert_eq!(doubled(), 82);

    unit.push_script("the_best_num(); i32 { ; 42 }").unwrap();
    assert_eq!(best(), 42);
    assert_eq!(doubled(), 84);
}

#[test]
#[serial]
fn depends_on() {
    let mut unit = Unit::new();

    unit.push_script("the_best_num(); i32 { ; 42 }").unwrap();
    unit.push_script("worse_num(); i32 { ; the_best_num() - 1 }")
        .unwrap();
    let worse = unit.get_fn("worse_num").unwrap().downcast::<(), i32>();
    let best = unit.get_fn("the_best_num").unwrap().downcast::<(), i32>();
    assert_eq!(best(), 42);
    assert_eq!(worse(), 41);

    unit.push_script("worse_num(); i32 { ; the_best_num() - 2 }")
        .unwrap();
    assert_eq!(best(), 42);
    assert_eq!(worse(), 40);
}

#[test]
#[serial]
fn get_fn_by_ptr_0() {
    let mut unit = Unit::new();
    unit.add_lib(StdLib);

    let mut functions = Vec::<Func>::new();
    unit.add_global("functions".into(), &mut functions as *mut _);

    unit.push_script(
        r#"
        add_two(x: i32); i32 { ; x + 2 }
        fifty_four(); i32 { ; 54 }

        ---

        functions.push(comp_data.get_fn_by_ptr(add_two).1)
        functions.push(comp_data.get_fn_by_ptr(fifty_four).1)
        "#
    ).unwrap();

    let [add_two, fifty_four] = &*functions else { panic!() };

    assert_eq!(add_two.typ(), FuncType { args: vec![ Type::i(32) ], ret: Type::i(32) });
    assert_eq!(fifty_four.typ(), FuncType { args: vec![], ret: Type::i(32) });

    let add_two = add_two.downcast::<(i32,), i32,>();
    let fifty_four = fifty_four.downcast::<(), i32>();

    assert_eq!(add_two(fifty_four()), 56);
}

//TODO: this doesn't work
#[test]
#[serial]
#[ignore]
fn get_fn_by_ptr_1() {
    let mut unit = Unit::new();
    unit.add_lib(StdLib);

    let mut functions = Vec::<UniqueFuncInfo>::new();
    unit.add_global("functions".into(), &mut functions as *mut _);

    unit.push_script(
        r#"
        add_two(x: i32); i32 { ; x + 2 }
        fifty_four(); i32 { ; 54 }

        ---

        functions.push(comp_data.get_fn_by_ptr(add_two).0)
        functions.push(comp_data.get_fn_by_ptr(fifty_four).0)
        "#
    ).unwrap();

    let [add_two, fifty_four] = &*functions else { panic!() };
    let add_two = unit.get_fn(add_two.clone()).unwrap();
    let fifty_four = unit.get_fn(fifty_four.clone()).unwrap();

    assert_eq!(add_two.typ(), FuncType { args: vec![ Type::i(32) ], ret: Type::i(32) });
    assert_eq!(fifty_four.typ(), FuncType { args: vec![], ret: Type::i(32) });

    let add_two = add_two.downcast::<(i32,), i32,>();
    let fifty_four = fifty_four.downcast::<(), i32>();

    assert_eq!(add_two(fifty_four()), 56);
}
