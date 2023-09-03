use cxc::{Unit, library::StdLib, FuncType, Type, ExternalFuncAdd};
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
#[ignore]
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

    unit.push_script("worse_num(); i32 { ; the_best_num() - 2 }").unwrap();
    assert_eq!(best(), 42);
    assert_eq!(worse(), 40);
}

#[test]
#[serial]
fn two_vecs() {
    let mut unit = Unit::new();

    unit.push_script(
        r#"
            zero(); i32 {
                ; 0
            }

            copy_num(val: i32); i32 {
                ; val
            }

            push_1(); i32 {
                x: i32 = zero()
                x = copy_num(x)
                ; 0
            }
        "#,
    ).unwrap();
    let push_1 = unit.get_fn("push_1").unwrap().downcast::<(), i32>();
    assert_eq!(push_1(), 0);

    unit.push_script(r#"push_2(); i32 { ; 1 }"#).unwrap();
    assert_eq!(push_1(), 0);
}

#[test]
#[serial]
fn get_fn_by_ptr() {
    let mut unit = Unit::new();
    unit.add_lib(StdLib);

    let mut functions = Vec::<usize>::new();
    unit.add_global("functions".into(), &mut functions as *mut _);
    //cxc::bytesof::print_binary(&functions);

    unit.push_script(
        r#"
        add_two(x: i32); i32 { ; x + 2 }

        ---

        functions.push(add_two)
        "#
    ).unwrap();

    let add_two = functions[0];
    let add_two = unit.get_fn_by_ptr(add_two as _).unwrap().1;

    functions.clear();

    //cxc::bytesof::print_binary(&functions);

    unit.push_script(
        r#"
        ninety(); i32 { ; 90 }

        ---

        functions.push(ninety)
        "#
    ).unwrap();

    let ninety = functions[0];
    let ninety = unit.get_fn_by_ptr(ninety as _).unwrap().1;

    functions.clear();

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

#[test]
#[serial]
fn call_previous() {
    let mut unit = Unit::new();
    unit.push_script(r#"
        fifty_four(); i32 { ; 54 }
        double_that(); i32 { ; fifty_four() * 3 }
    "#).unwrap();
    let double_that = unit.get_fn("double_that").unwrap().downcast::<(), i32>();
    assert_eq!(double_that(), 54 * 3);

    unit.push_script(r#"
        double_that(); i32 { ; fifty_four() * 2 }
    "#).unwrap();
    assert_eq!(double_that(), 54 * 2);
}

#[test]
#[serial]
fn call_2_previous() {
    let mut unit = Unit::new();
    unit.push_script(r#"fifty_four<T>(); T { ; T 54 }"#).unwrap();
    unit.push_script(r#"double_that<T>(); T { ; fifty_four<T>() * T 2 }"#).unwrap();
    unit.push_script(r#"sextuple_that(); i32 { ; double_that<i32>() * 2 }"#).unwrap();
    let sextuple_that = unit.get_fn("sextuple_that").unwrap().downcast::<(), i32>();
    assert_eq!(sextuple_that(), 54 * 2 * 2);

    unit.push_script(r#"
        sextuple_that(); i32 { ; double_that<i32>() * 3 }
    "#).unwrap();
    assert_eq!(sextuple_that(), 54 * 2 * 3);
}
