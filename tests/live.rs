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
fn get_fn_by_ptr() {
    let mut unit = Unit::new();
    unit.add_lib(StdLib);

    let mut functions = Vec::<usize>::new();
    unit.add_global("functions".into(), &mut functions as *mut _);

    unit.push_script(
        r#"
        add_two(x: i32); i32 { ; x + 2 }

        ---

        functions.push(add_two)
        "#
    ).unwrap();

    unit.push_script(
        r#"
        ninety(); i32 { ; 90 }

        ---

        functions.push(ninety)
        "#
    ).unwrap();

    let [add_two, ninety] = &*functions else { panic!() };

    let add_two = unit.get_fn_by_ptr(*add_two as _).unwrap().1;
    let ninety = unit.get_fn_by_ptr(*ninety as _).unwrap().1;

    assert_eq!(add_two.typ(), FuncType { args: vec![ Type::i(32) ], ret: Type::i(32) });
    assert_eq!(ninety.typ(), FuncType { args: vec![], ret: Type::i(32) });

    let add_two = add_two.downcast::<(i32,), i32,>();
    let ninety = ninety.downcast::<(), i32>();

    assert_eq!(add_two(20), 22);
    assert_eq!(ninety(), 90);
}

#[test]
#[serial]
fn hot_reload_many_times() {
    let mut unit = Unit::new();
    unit.push_script("the_best_num(); i32 { ; 200 } ").unwrap();
    let cached_func = unit.get_fn("the_best_num").unwrap().downcast::<(), i32>();
    assert_eq!(cached_func(), 200);

    for n in 0..20 {
        let new_code = format!("the_best_num(); i32 {{ ; {n} }}");
        unit.push_script(&*new_code).unwrap();

        // we should be able to get the func using get_fn, but the cached_func should still work as
        // well
        let reloaded_func = unit.get_fn("the_best_num").unwrap().downcast::<(), i32>();
        assert_eq!(reloaded_func(), n);
        assert_eq!(cached_func(), n);
    }
}

#[test]
#[serial]
fn get_previous() {
    let mut unit = Unit::new();
    unit.push_script("func_one(); i32 { ; 1 } ").unwrap();
    let func_one = unit.get_fn("func_one").unwrap().downcast::<(), i32>();

    unit.push_script("func_two(); i32 { ; 2 } ").unwrap();
    assert_eq!(func_one(), 1);

    let func_two = unit.get_fn("func_two").unwrap().downcast::<(), i32>();
    assert_eq!(func_two(), 2);
    assert_eq!(func_one(), 1);

    let func_one_again = unit.get_fn("func_one").unwrap().downcast::<(), i32>();
    assert_eq!(func_one_again(), 1);
}
