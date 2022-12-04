mod test_utils;
use cxc::{LLVMContext, Unit};
use test_utils::{Numbers4, Numbers5, Point2D, Point3D, TestUtilsLib};

#[test]
fn small_value() {
    let context = LLVMContext::new();
    let mut unit = Unit::new(&context);
    unit.push_script("double(x: i32): i32 { ; x * 2 }");

    let value = unit.get_value("double(4)");
    let out_point = unsafe { value.consume_as::<i32>() };
    assert_eq!(out_point, 8);
}

#[test]
fn medium_value() {
    let context = LLVMContext::new();
    let mut unit = Unit::new(&context);
    unit.add_lib(TestUtilsLib::new("Point2D"));
    unit.push_script("double(x: i32): i32 { ; x * 2 }");

    let value = unit.get_value("Point2D { x = double(4), y = 40 }");
    let out_point = unsafe { value.consume_as::<Point2D>() };
    assert_eq!(out_point, Point2D { x: 8, y: 40 });
}

#[test]
fn large_value() {
    let context = LLVMContext::new();
    let mut unit = Unit::new(&context);
    unit.add_lib(TestUtilsLib::new("Point3D"));
    unit.push_script("double(x: i32): i32 { ; x * 2 }");

    let value = unit.get_value("Point3D { x = 84, y = double(4), z = 40 }");
    let out_point = unsafe { value.consume_as::<Point3D>() };
    assert_eq!(out_point, Point3D { x: 84, y: 8, z: 40 });
}

#[test]
fn extra_large_value() {
    let context = LLVMContext::new();
    let mut unit = Unit::new(&context);
    unit.add_lib(TestUtilsLib::new("Numbers4"));
    unit.push_script("double(x: i32): i32 { ; x * 2 }");

    let value = unit.get_value("Numbers4 { a = 1, b = double(3), c = 8, d = 4}");
    let out_point = unsafe { value.consume_as::<Numbers4>() };
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
fn jumbo_value() {
    let context = LLVMContext::new();
    let mut unit = Unit::new(&context);
    unit.add_lib(TestUtilsLib::new("Numbers5"));
    unit.push_script("double(x: i32): i32 { ; x * 2 }");

    let value =
        unit.get_value("Numbers5 { a = 1, b = 2, c = double(3), d = 4, e = 5 }");
    let out_point = unsafe { value.consume_as::<Numbers5>() };
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