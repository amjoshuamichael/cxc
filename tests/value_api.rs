#![allow(unused_must_use)]
mod test_utils;
use std::rc::Rc;

use cxc::{Unit, Value, library::StdLib};
use test_utils::{Numbers4, Numbers5, Point2D, Point3D, TestUtilsLib};

#[test]
fn value_4() {
    let mut unit = Unit::new();
    unit.push_script("double(x: i32); i32 { ; x * 2 }");

    let value = unit.get_value("double(4)");
    let out_point = unsafe { *value.unwrap().consume::<i32>() };
    assert_eq!(out_point, 8);
}

#[test]
fn value_8() {
    let mut unit = Unit::new();
    unit.add_lib(TestUtilsLib::new("Point2D"));
    unit.push_script("double(x: i32); i32 { ; x * 2 }");

    let value = unit.get_value("Point2D { x = double(4), y = 40 }");
    let out_point = unsafe { *value.unwrap().consume::<Point2D>() };
    assert_eq!(out_point, Point2D { x: 8, y: 40 });
}

#[test]
fn value_12() {
    let mut unit = Unit::new();
    unit.add_lib(TestUtilsLib::new("Point3D"));
    unit.push_script("double(x: i32); i32 { ; x * 2 }");

    let value = unit.get_value("Point3D { x = 84, y = double(4), z = 40 }");
    let out_point = unsafe { *value.unwrap().consume::<Point3D>() };
    assert_eq!(out_point, Point3D { x: 84, y: 8, z: 40 });
}

#[test]
fn value_16() {
    let mut unit = Unit::new();
    unit.add_lib(TestUtilsLib::new("Numbers4"));
    unit.push_script("double(x: i32); i32 { ; x * 2 }");

    let value = unit.get_value("Numbers4 { a = 1, b = double(3), c = 8, d = 4}");
    let out_point = unsafe { *value.unwrap().consume::<Numbers4>() };
    assert_eq!(
        out_point,
        Numbers4 {
            a: 1,
            b: 6,
            c: 8,
            d: 4,
        }
    );
}

#[test]
fn value_20() {
    let mut unit = Unit::new();
    unit.add_lib(TestUtilsLib::new("Numbers5"));
    unit.push_script("double(x: i32); i32 { ; x * 2 }");

    let value = unit.get_value("Numbers5 { a = 1, b = 2, c = double(3), d = 4, e = 5 }");
    let out_point = unsafe { *value.unwrap().consume::<Numbers5>() };
    assert_eq!(
        out_point,
        Numbers5 {
            a: 1,
            b: 2,
            c: 6,
            d: 4,
            e: 5
        }
    );
}

#[test]
fn value_24() {
    let mut unit = Unit::new();
    unit.push_script("double(x: i32); i32 { ; x * 2 }");

    let value = unit.get_value("{ 1, 2, double(3), 4, 5, 6 }");
    let out_point = unsafe { *value.unwrap().consume::<(i32, i32, i32, i32, i32, i32)>() };
    assert_eq!(out_point, (1, 2, 6, 4, 5, 6));
}

#[test]
fn value_4_no_depedency() {
    let mut unit = Unit::new();

    let value = unit.get_value("4");
    let out_point = unsafe { *value.unwrap().consume::<i32>() };
    assert_eq!(out_point, 4);
}

#[test]
fn basic_value_func() {
    let mut unit = Unit::new();
    unit.add_lib(StdLib);
    unit.push_script("sixty_three(); Value { ; value(u32 234) }").unwrap();

    let value = unit.get_fn("sixty_three").unwrap().downcast::<(), Value>()();
    assert_eq!(value.get_type(), &unit.comp_data.get_spec(&"u32".into(), &()).unwrap());
    let value_u32 = unsafe { *value.consume::<u32>() };
    assert_eq!(value_u32, 234);
}

#[test]
fn vec_value_func() {
    let mut unit = Unit::new();
    unit.add_lib(StdLib);
    unit.push_script(
        "
        many_nums(); Value { 
            some_nums = Vec<u32>:new()
            some_nums.push(4325)
            some_nums.push(6954)
            some_nums.push(50986)

            ; value(some_nums)
        }
        "
    ).unwrap();

    let value = unit.get_fn("many_nums").unwrap().downcast::<(), Value>()();
    assert_eq!(value.get_type(), &unit.comp_data.get_spec(&"Vec<u32>".into(), &()).unwrap());
    let vec_value = unsafe { *value.consume::<Vec<u32>>() };
    assert_eq!(vec_value, vec![4325, 6954, 50986]);
}

#[test]
fn rc_value_func() {
    let mut unit = Unit::new();
    unit.add_lib(StdLib);
    unit.push_script("sixty_three_rc(); Value { ; value(Rc<u32>:new(u32 63)) }").unwrap();

    let value = unit.get_fn("sixty_three_rc").unwrap().downcast::<(), Value>()();
    assert_eq!(value.get_type(), &unit.comp_data.get_spec(&"Rc<u32>".into(), &()).unwrap());
    let value_u32 = unsafe { *value.consume::<Rc<u32>>() };
    assert_eq!(*value_u32, 63);
}
