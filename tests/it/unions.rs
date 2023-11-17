use super::test_utils::xc_test;

#[derive(Debug, PartialEq, Copy, Clone)]
#[repr(C)]
struct TestOutput(i32, f32, [i32; 5]);

#[test]
fn basic_unions() {
    xc_test!(
        "
        Union = {
            int_value: i32 |
            float_value: f32 |
            array_value: [5]i32
        }

        main(); {i32, f32, [5]i32} {
            union: Union = {--}
            output := {--}
            
            union.int_value = 42
            output.0 = union.int_value

            union.float_value = 3.14
            output.1 = union.float_value

            union.array_value = [4, 90, 243, 49, 3]
            output.2 = union.array_value

            ; {42, 3.14, [4, 90, 243, 49, 3]}
        }
        ";
        TestOutput(42, 3.14, [4, 90, 243, 49, 3])
    )
}

#[test]
fn union_parameter() {
    xc_test!(
        "
        Union = {
            int_value: i32 |
            two_ints: {i32, i32} |
            three_ints: {i32, i32, i32}
        }

        sum(union: Union, tag: i32); i32 {
            ?  tag == 0 { ; union.int_value }
            :? tag == 1 { ; union.two_ints.0 + union.two_ints.1 }
            :? tag == 2 { ; union.three_ints.0 + union.three_ints.1 + union.three_ints.2 }
            ; 0
        }

        main(); i32 {
            union: Union = {--}
            sum := 0
            
            union.int_value = 1
            sum = sum + sum(union, 0)

            union.two_ints = { 10, 10 }
            sum = sum + sum(union, 1)

            union.three_ints = { 100, 100, 100 }
            sum = sum + sum(union, 2)

            ; sum
        }
        ";
        321
    )
}

#[test]
fn union_modify_bit() {
    xc_test!(
        "
        Union = {
            int_value: i32 |
            bytes: [4]u8 |
        }

        main(); {i32, i32} {
            union: Union = {--}    
            union.int_value = 42

            one := union.int_value

            union.bytes[3] = 0b10000000
            two := union.int_value

            ; {one, two}
        }
        ";
        (42, i32::MIN + 42)
    )
}

#[test]
fn union_array_in_struct() {
    xc_test!(
        "
        Point = {
            x: f32,
            y: f32,
        }

        Shape = {
            tag: u8,
            union: {
                line: {
                    start: Point,
                    end: Point,
                } |
                square: {
                    sides: [4]i32
                }
            }
        }

        main(); { Shape.union.square, Shape.union.line, } {
            square: Shape = {--}
            square.tag = 0
            square.union.square.sides = [0, 1, 2, 3]

            line: Shape = {--}
            line.tag = 1
            pline := &line.union.line
            line.union.line.start.x = 0.0
            line.union.line.start.y = 1.0
            pline.end = { x = 2.0, y = 3.0 }

            ; { square.union.square, line.union.line }
        }
        ";
        ([0, 1, 2, 3], (0.0f32, 1.0f32, 2.0f32, 3.0f32))
    )
}

#[test]
fn union_with_struct() {
    xc_test!(
        "
        Union = {
            int: i32 |
            float: f32 |
            point: { x: i64, y: i64 }
        }

        main(); { Union.int, Union.float, Union.point } {
            union: Union = { -- }

            union.int = 42
            one := union.int

            union.float = 3.14
            two := union.float

            union.point = { x = 1, y = 2}
            point := union.point

            ; {one, two, point}
        }
        ";
        (42, 3.14f32, (1i64, 2i64))
    )
}
