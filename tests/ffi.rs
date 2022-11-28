mod test_utils;
use cxc::library::{StdLib, TestLib};
use cxc::XcReflect;
use cxc::{LLVMContext, Type, Unit};
use test_utils::{xc_test, Numbers5, Point2D, Point3D};

#[test]
fn return_arg() {
    xc_test!(
        input, i32; => i32;
        "; input";
        32 => 32
    )
}

#[test]
fn multiple_args() {
    xc_test!(
        a, i32; b, i32; => i32;
        "; a + b";
        4, 5 => 9
    )
}

#[test]
fn pointer() {
    let context = LLVMContext::new();
    let mut unit = Unit::new(&context);

    unit.push_script(
        "
            square(num: &i32): i32 {
                num = *num * *num
                ; 0
            }
            ",
    );

    let mut num = 4;
    unsafe { unit.get_fn_by_name::<&mut i32, i32>("square")(&mut num) };
    assert_eq!(num, 16);
}

#[test]
fn one_el_array_out() {
    xc_test!(
        "main(): i32[1] { ; [1] }";
        [1]
    )
}

#[test]
fn two_el_array_out() {
    xc_test!(
        "main(): i32[2] { ; [1, 4] }";
        [1, 4]
    )
}

#[test]
fn three_el_array_out() {
    xc_test!(
        "main(): i32[3] { ; [1, 4, 9] }";
        [1, 4, 9]
    )
}

#[test]
fn four_el_array_out() {
    xc_test!(
        "main(): i32[4] { ; [1, 4, 9, 90] }";
        [1, 4, 9, 90]
    )
}

#[test]
fn five_el_array_out() {
    xc_test!(
        "main(): i32[5] { ; [1, 4, 9, 90, 129] }";
        [1, 4, 9, 90, 129]
    )
}

#[test]
fn struct_pointer() {
    let context = LLVMContext::new();
    let mut unit = Unit::new(&context);

    unit.push_script(
        "
        Point2D = {
            x: i32,
            y: i32
        }

        sqr_magnitude_of(in_ptr: &Point2D): i32 {
            in: Point2D = *in_ptr

            ; in.x * in.x + in.y * in.y
        }
        ",
    );

    let point = Point2D { x: 2, y: 3 };

    let sqr_mag: i32 = unsafe { unit.get_fn_by_name("sqr_magnitude_of")(&point) };
    assert_eq!(sqr_mag, 13);
}

#[test]
fn small_struct() {
    dbg!(test_utils::Point2D::alias_code());

    xc_test!(
        "; Point2D { x = 32, y = 43 }" => Point2D;
        Point2D { x: 32, y: 43 }
    )
}

#[test]
fn medium_struct() {
    xc_test!(
        "; Point3D { x = 32, y = 43, z = 13 }" => Point3D;
        Point3D { x: 32, y: 43, z: 13 }
    )
}

#[test]
fn large_struct() {
    xc_test!(
        "; Numbers5 { a = 1, b = 2, c = 3, d = 4, e = 5 }" => Numbers5;
        Numbers5 { a: 1, b: 2, c: 3, d: 4, e: 5 }
    )
}

#[test]
fn external_function() {
    pub fn print_num(input: i64) {
        println!("{input}");
    }

    let context = LLVMContext::new();
    let mut unit = Unit::new(&context);

    unit.add_rust_func_explicit(
        "print_num",
        print_num as *const usize,
        Type::never().func_with_args(vec![Type::i(64)]),
        None,
        Vec::new(),
    )
    .push_script(
        "
        call(): i64 {
            x: i64 = 0
            @ x < 100 {
                print_num(x)
                x = x + 1
            }

            ; x
        }
        ",
    );

    unsafe { unit.get_fn_by_name::<(), i64>("call")(()) };
}

#[test]
fn strings() {
    xc_test!(
        use TestLib, StdLib;
        r#"
        main(): String {
            x: String = "that's cray"
            ; x
        }
        "#;
        String::from("that's cray")
    )
}
