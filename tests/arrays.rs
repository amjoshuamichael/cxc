mod test_utils;

use cxc::{library::{StdLib, StringLib}, Unit};
use test_utils::xc_test;

#[test]
fn array_basic() {
    xc_test!(
        "
        main(); i32 {
            original: [7]i32 = [1, 4, 8, 15, 16, 23, 42]

            first_sum: i32 = original[3] + original[0] + original[6]

            index: u64 = 0
            @ index < 7 {
                original[index] = cast<i64, i32>(index) * 2

                index = index + 1
            }

            second_sum: i32 = original[0] + original[1] + original[3] + original[6]

            ; first_sum + second_sum
        }
        ";
        78
    )
}

#[test]
fn struct_arrays() {
    xc_test!(
        "
        Point2D = {
            x: i32,
            y: i32
        }

        main(); i32 {
            points: [3]Point2D = [
                Point2D { x = 43, y = 15 },
                Point2D { x = 327, y = 413 },
                Point2D { x = 1672, y = 2526 },
            ]

            output: i32 = points[i64 0].x + points[i64 0].y

            points[i64 0].x = 94

            output = output + points[i64 0].x + points[i64 0].y

            points[i64 1] = Point2D { x = 4, y = 6 }

            output = output + points[i64 0].x + points[i64 0].y

            ; output
        }
        ";
        276
    )
}

#[test]
fn active_initialize_array_ints() {
    xc_test!(
        use StdLib;
        "
        main(); [5]i32 {
            numbers: [5]i32 = [10, 20, 90, ++]

            ; numbers
        }
        ";
        [10, 20, 90, 0, 0]
    );
}

#[test]
#[cfg(not(feature = "backend-interpreter"))]
fn string_array() {
    xc_test!(
        use StdLib;
        r#"
        main(); [2]String {
            numbers: [2]String = ["one", "two"]
            ; numbers
        }
        "#;
        [String::from("one"), String::from("two")]
    );
}

#[test]
#[cfg(not(feature = "backend-interpreter"))]
fn active_initialize_array_strings() {
    xc_test!(
        use StringLib;
        r#"
        main(); [5]String {
            numbers: [5]String = ["one", "two", "three", ++ ]

            ; numbers
        }
        "#;
        [String::from("one"), String::from("two"), String::from("three"), String::default(), String::default()]
    );
}

#[test]
fn slice_basic() {
    xc_test!(
        use StdLib;
        r#"
        main(); { u64, u32 } {
            numbers: [5]u32 = [543, 60, 44, 222, 994]

            range: Range<u64> = Range<u64>:from(u64 2, u64 4)
            slice: { +ptr: &u32, +len: u64 } = numbers.slice(range)
            ; { slice.len, *slice.ptr }
        }
        "#;
        (2u64, 44u32)
    );
}

#[test]
#[cfg(not(feature = "backend-interpreter"))]
fn pass_in_slice() {
    let mut unit = Unit::new();
    unit.add_lib(StdLib);

    unit.push_script("
        section_of(array_ptr: &[6]u32); { +ptr: &u32, +len: u64 } {
            ; array_ptr.slice(Range<u64>:from(u64 1, u64 4))
        }
    ").unwrap();

    let section_of = unit.get_fn("section_of").unwrap().downcast::<(&[u32; 6],), &[u32]>();

    let x = [43, 543, 99, 5436, 99, 23];
    assert_eq!(section_of(&x), &[543, 99, 5436]);
}
