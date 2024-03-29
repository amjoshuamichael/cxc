use cxc::{Unit, library::StdLib, Type, ExternalFuncAdd};

#[test]
fn hot_reload_one() {
    let mut unit = Unit::new();
    unit.push_script("the_best_num(); i32 { ; 41 } ").unwrap();
    let func = unit.get_fn("the_best_num").unwrap().downcast::<(), i32>();
    assert_eq!(func(), 41);

    unit.push_script("the_best_num(); i32 { ; 42 } ").unwrap();
    assert_eq!(func(), 42);
}

#[test]
fn hot_reload_many_1() {
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
fn generic_depended_on() {
    let mut unit = Unit::new();

    unit.push_script("the_best_num<T>(); T { ; T 41 }").unwrap();
    unit.push_script("double_that(); i32 { ; the_best_num<i32>() * 2 }")
        .unwrap();
    let doubled = unit.get_fn("double_that").unwrap().downcast::<(), i32>();
    assert_eq!(doubled(), 82);

    unit.push_script("the_best_num<T>(); T { ; T 42 }").unwrap();
    assert_eq!(doubled(), 84);
}

#[test]
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
#[cfg_attr(feature = "backend-interpreter", ignore)]
fn get_fn_by_ptr() {
    let mut unit = Unit::new();
    unit.add_lib(StdLib);

    let mut functions = Vec::<usize>::new();
    unit.add_global("functions".into(), &mut functions as *mut _);

    unit.push_script(
        r#"
        add_two(x: i32); i32 { ; x + 2 }

        ---

        functions.push(cast<(i32); i32, u64>(add_two))
        "#
    ).unwrap();

    let add_two = functions[0];
    let add_two = unit.get_fn_by_ptr(add_two as _).unwrap().1;

    functions.clear();

    unit.push_script(
        r#"
        ninety(); i32 { ; 90 }

        ---

        functions.push(cast<(); i32, u64>(ninety))
        "#
    ).unwrap();

    let ninety = functions[0];
    let ninety = unit.get_fn_by_ptr(ninety as _).unwrap().1;

    functions.clear();

    assert_eq!(add_two.typ().args, vec![Type::i(32)]);
    assert_eq!(add_two.typ().ret, Type::i(32));
    assert_eq!(ninety.typ().args, vec![]);
    assert_eq!(ninety.typ().ret, Type::i(32));

    let add_two = add_two.downcast::<(i32,), i32,>();
    let ninety = ninety.downcast::<(), i32>();

    assert_eq!(add_two(20), 22);
    assert_eq!(ninety(), 90);
}

#[test]
fn hot_reload_many_2() {
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

#[test]
fn conversion_dependents() {
    let mut unit = Unit::new();

    fn print_number(num: i64) { println!("{num}") }
    unit.add_rust_func_explicit(
        "print_number",
        print_number as *const usize,
        ExternalFuncAdd {
            arg_types: vec![Type::i(64)],
            ..ExternalFuncAdd::empty()
        },
    );

    unit.push_script(r#"
        TwoNums = { i32, i32 }
        &TwoNums:.fifty(); i32 { ; 40 }
    "#).unwrap();

    unit.push_script(r#"
        main(); i32 {
            x := TwoNums { 0, 0 }
            ; x.fifty()
        }
    "#).unwrap();

    let main = unit.get_fn("main").unwrap().downcast::<(), i32>();
    assert_eq!(main(), 40);

    unit.push_script(r#"
        TwoNums:.fifty(); i32 { ; 50 }
    "#).unwrap();
    assert_eq!(main(), 50);
}

#[test]
fn change_block_count() {
    let mut unit = Unit::new();

    unit.push_script(r#"
        fill_arr(); [300]u64 {
            arr: [300]u64 = [--]
            index := 0

            @ index < 300 {
                arr[index] = index

                index = index + 1
            }

            ; arr
        }
    "#).unwrap();
    let fill_arr = unit.get_fn("fill_arr").unwrap().downcast::<(), [u64; 300]>();

    let my_arr: [u64; 300] = fill_arr();

    let expected = (0..300).collect::<Vec<_>>();
    assert_eq!(&my_arr, &*expected);

    unit.push_script(r#"
        fill_arr(); [300]u64 {
            arr: [300]u64 = [--]
            index := 0

            @ index < 300 {
                arr[index] = index

                ? index % 5 == 0 { arr[index] = 5 } 
                ? index % 3 == 0 { arr[index] = 3 }

                index = index + 1
            }

            ; arr
        }
    "#).unwrap();

    let my_arr: [u64; 300] = fill_arr();

    let expected = (0..300).map(|x| {
        if x % 3 == 0 { 3 }
        else if x % 5 == 0 { 5 }
        else { x }
    }).collect::<Vec<_>>();
    assert_eq!(&my_arr, &*expected);
}

#[test]
fn change_type_2i32() {
    let mut unit = Unit::new();
    unit.push_script(r#"fifty_four(); i32 { ; 54 } "#).unwrap();
    let fifty_four = unit.get_fn("fifty_four").unwrap().clone();

    assert_eq!(fifty_four.typ().args, Vec::new());
    assert_eq!(fifty_four.typ().ret, Type::i(32));
    assert_eq!(fifty_four.downcast::<(), i32>()(), 54);

    unit.push_script(r#"fifty_four(); {i32, i32} { ; {32, 33} }"#).unwrap();
    assert_eq!(fifty_four.typ().args, Vec::new());
    assert_eq!(fifty_four.typ().ret, Type::new_tuple(vec![Type::i(32), Type::i(32)]));
    assert_eq!(fifty_four.downcast::<(), (i32, i32)>()(), (32, 33));
}

#[test]
fn change_type_f32() {
    let mut unit = Unit::new();
    unit.push_script(r#"fifty_four(); i32 { ; 54 } "#).unwrap();
    let fifty_four = unit.get_fn("fifty_four").unwrap().clone();

    assert_eq!(fifty_four.typ().args, Vec::new());
    assert_eq!(fifty_four.typ().ret, Type::i(32));
    assert_eq!(fifty_four.downcast::<(), i32>()(), 54);

    unit.push_script(r#"fifty_four(); f32 { ; 54.0 }"#).unwrap();
    assert_eq!(fifty_four.typ().args, Vec::new());
    assert_eq!(fifty_four.typ().ret, Type::f(32));
    assert_eq!(fifty_four.downcast::<(), f32>()(), 54.0);
}

#[test]
fn change_type_2f32() {
    let mut unit = Unit::new();
    unit.push_script(r#"fifty_four(); i32 { ; 54 } "#).unwrap();
    let fifty_four = unit.get_fn("fifty_four").unwrap().clone();

    assert_eq!(fifty_four.typ().args, Vec::new());
    assert_eq!(fifty_four.typ().ret, Type::i(32));
    assert_eq!(fifty_four.downcast::<(), i32>()(), 54);

    unit.push_script(r#"fifty_four(); {f32, f32} { ; {54.0, 72.7} }"#).unwrap();
    assert_eq!(fifty_four.typ().args, Vec::new());
    assert_eq!(fifty_four.typ().ret, Type::new_tuple(vec![Type::f(32), Type::f(32)]));
    assert_eq!(fifty_four.downcast::<(), (f32, f32)>()(), (54.0, 72.7));
}
