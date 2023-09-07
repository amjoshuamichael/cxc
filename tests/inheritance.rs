//mod test_utils;
//
//use std::rc::Rc;
//
//use cxc::{Unit, library::StdLib};
//use test_utils::xc_test;
//
//#[test]
//fn auto_deref_member() {
//    xc_test!(
//        use StdLib;
//        "
//        MyPoint = { x: f32, y: f32 }
//
//        main(); f32 {
//            x: {&&&MyPoint}.x = 4.0
//            y: {&&Rc<MyPoint>}.x = 4.0
//            z: {&&Rc<&MyPoint>}.x = 4.0
//
//            ; x + y + z
//        }
//        ";
//        12.0f32
//    )
//}
//
//#[test]
//fn auto_deref_method_1() {
//    xc_test!(
//        use StdLib;
//        r#"
//        MyPoint = { x: f32, y: f32 }
//
//        &MyPoint:.sqr_hypotenuse(); f32 {
//            ; self.x * self.x + self.y * self.y
//        }
//
//        main(); f32 {
//            rc_point: Rc<MyPoint> = Rc<MyPoint>:new(MyPoint { x = 4.0, y = 3.0 })
//            sqr_hyp: f32 = rc_point.sqr_hypotenuse()
//            ; sqr_hyp
//        }
//        "#;
//        25.0f32
//    )
//}
//
//#[derive(Debug, PartialEq)]
//struct MyPoint {
//    x: f32,
//    y: f32,
//}
//
//#[test]
//fn auto_deref_method_2() {
//    let mut unit = Unit::new();
//    unit.add_lib(StdLib);
//
//    unit.push_script(
//        r#"
//        MyPoint = { x: f32, y: f32 }
//
//        &MyPoint:.sqr_hypotenuse(); f32 {
//            ; self.x * self.x + self.y * self.y
//        }
//
//        main(rc_point: Rc<MyPoint>); f32 {
//            sqr_hypotenuse: f32 = rc_point.sqr_hypotenuse()
//
//            ; sqr_hypotenuse
//        }
//        "#,
//    )
//    .unwrap();
//
//    let func = unit
//        .get_fn("main")
//        .unwrap()
//        .downcast::<(Rc<MyPoint>,), f32>();
//
//    let rc = Rc::new(MyPoint { x: 4.0, y: 3.0 });
//    assert_eq!(func(rc), 25.0);
//}
//
//#[test]
//fn big_rc_sum() {
//    xc_test!(
//        use StdLib;
//        "
//        &{i32, i32}:.sum(); i32 {
//            ; self.0 + self.1
//        }
//
//        main(); i32 {
//            rcx: Rc<{i32, i32}> = Rc<{i32, i32}>:new({ 90, 90 })
//
//            ; rcx.sum()
//        }
//        ";
//        180
//    )
//}
