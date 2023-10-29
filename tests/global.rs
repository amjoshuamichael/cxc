//use std::rc::Rc;
//
//use serial_test::serial;
//use crate::test_utils::Numbers5;
//mod test_utils;
//
//use cxc::{Unit, library::StdLib, Type};
//
//static mut GNUM: i32 = 90;
//
//#[test]
//#[serial]
//#[cfg_attr(feature = "backend-interpreter", ignore)]
//fn basic_global() {
//    let mut unit = Unit::new();
//
//    unsafe { GNUM = 50 };
//    unit.add_global("x".into(), unsafe { &mut GNUM as *mut _ });
//
//    unit.push_script(
//        r#"
//        main() {
//            *x = 50
//        }
//        "#,
//    )
//    .unwrap();
//
//    unit.get_fn("main").unwrap().downcast::<(), ()>()();
//
//    assert_eq!(unsafe { GNUM }, 50);
//}
//
//#[test]
//#[serial]
//#[cfg_attr(feature = "backend-interpreter", ignore)]
//fn read_basic_global() {
//    let mut unit = Unit::new();
//
//    unsafe { GNUM = 90 };
//    unit.add_global("x".into(), unsafe { &mut GNUM as *mut _ });
//
//    unit.push_script(
//        r#"
//        main(); i32 {
//            ; *x
//        }
//        "#,
//    )
//    .unwrap();
//
//    let read = unit.get_fn("main").unwrap().downcast::<(), i32>()();
//
//    assert_eq!(read, 90);
//}
//
//#[test]
//#[cfg_attr(feature = "backend-interpreter", ignore)]
//fn big_global() {
//    let mut unit = Unit::new();
//
//    let mut large_value = Numbers5::default();
//    unit.add_reflect_type::<Numbers5>();
//    unit.add_global("large_value".into(), &mut large_value as *mut _);
//
//    unit.push_script(
//        r#"
//        main() {
//            large_value.d = 60
//        }
//        "#,
//    )
//    .unwrap();
//
//    unit.get_fn("main").unwrap().downcast::<(), ()>()();
//
//    assert_eq!(large_value.d, 60);
//}
//
//#[test]
//#[cfg_attr(feature = "backend-interpreter", ignore)]
//fn read_big_global() {
//    let mut unit = Unit::new();
//
//    let mut large_value = Numbers5 { a: 5, b: 10, c: 15, d: 20, e: 25 };
//    unit.add_reflect_type::<Numbers5>();
//    unit.add_global("large_value".into(), &mut large_value as *mut _);
//
//    unit.push_script(
//        r#"
//        main(); Numbers5 {
//            ; *large_value
//        }
//        "#,
//    )
//    .unwrap();
//
//    let read = unit.get_fn("main").unwrap().downcast::<(), Numbers5>()();
//
//    assert_eq!(read, large_value);
//}
//
//#[test]
//#[cfg_attr(feature = "backend-interpreter", ignore)]
//fn global_rc() {
//    let mut unit = Unit::new();
//    unit.add_lib(StdLib);
//
//    let mut large_value = Rc::new(Numbers5 { b: 80, ..Default::default() });
//    unit.add_reflect_type::<Numbers5>();
//    unit.add_global("large_value".into(), &mut large_value as *mut _);
//
//    unit.push_script(
//        r#"
//        ---
//
//        large_value.b = 89
//
//        "#,
//    )
//    .unwrap();
//
//    assert_eq!(large_value.b, 89);
//}
//
//#[test]
//#[cfg_attr(feature = "backend-interpreter", ignore)]
//fn comp_script() {
//    let mut unit = Unit::new();
//
//    unit.add_lib(StdLib);
//    let mut large_value = Numbers5::default();
//    unit.add_reflect_type::<Numbers5>();
//    unit.add_global("large_value".into(), &mut large_value as *mut _);
//
//    unit.push_script(
//        r#"
//
//        double(x: i32); i32 { ; x * 2 }
//
//        ---
//
//        x := 30
//        y := double(x)
//        large_value.c = y
//
//        "#,
//    )
//    .unwrap();
//
//    assert_eq!(large_value.c, 60);
//}
//
//#[test]
//#[cfg_attr(feature = "backend-interpreter", ignore)]
//fn first_class_void() {
//    let mut unit = Unit::new();
//    unit.add_lib(StdLib);
//
//    let mut large_value = Numbers5::default();
//    large_value.a = 9;
//    unit.add_reflect_type::<Numbers5>();
//    unit.add_global("large_value".into(), &mut large_value as *mut _);
//
//    unit.push_script(
//        r#"
//        FnHolder = { (i32), }
//
//        multiply_a_by(x: i32) { 
//            large_value.a = large_value.a * x
//        }
//
//        get_that_fn(); FnHolder {
//            ; FnHolder { multiply_a_by }
//        }
//
//        main() {
//            fn: FnHolder = get_that_fn()
//            (fn.0)(3)
//            (fn.0)(9)
//        }
//        "#,
//    )
//    .unwrap();
//
//    unit.get_fn("main").unwrap().downcast::<(), ()>()();
//    assert_eq!(large_value.a, 243);
//}
//
//#[test]
//#[cfg_attr(feature = "backend-interpreter", ignore)]
//fn get_fn_by_ptr() {
//    let mut unit = Unit::new();
//    unit.add_lib(StdLib);
//
//    let mut functions = Vec::<usize>::new();
//    unit.add_reflect_type::<Numbers5>();
//    unit.add_global("functions".into(), &mut functions as *mut _);
//
//    unit.push_script(
//        r#"
//        add_two(x: i32); i32 { ; x + 2 }
//        fifty_four(); i32 { ; 54 }
//
//        ---
//
//        functions.push(cast<(i32); i32, u64>(add_two))
//        functions.push(cast<(); i32, u64>(fifty_four))
//        "#
//    ).unwrap();
//
//    let [add_two, fifty_four] = &*functions else { panic!() };
//    let add_two = unit.get_fn_by_ptr(*add_two as _).unwrap().1;
//    let fifty_four = unit.get_fn_by_ptr(*fifty_four as _).unwrap().1;
//
//    assert_eq!(add_two.typ().args, vec![Type::i(32)]);
//    assert_eq!(add_two.typ().ret, Type::i(32));
//    assert_eq!(fifty_four.typ().args, vec![]);
//    assert_eq!(fifty_four.typ().ret, Type::i(32));
//
//    let add_two = add_two.downcast::<(i32,), i32,>();
//    let fifty_four = fifty_four.downcast::<(), i32>();
//
//    assert_eq!(add_two(fifty_four()), 56);
//}
