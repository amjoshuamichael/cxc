mod test_utils;
use test_utils::xc_test;

#[test]
fn call_test_1() {
    xc_test!(
        "
        mul_by_two(num: i32); i32 {
            output: i32 = num * 2
            ; output
        }

        main(); i32 {
            ; mul_by_two(6)
        }
        ";
        12
    )
}

#[test]
fn call_test_2() {
    xc_test!(
        "
        divide_by_two(num: f32); f32 {
            output: f32 = num / 2.0
            ; output
        }

        main(); f32 {
            x: f32 = divide_by_two(6.0)
            ; x
        }
        ";
        6.0_f32 / 2.0_f32
    )
}

#[test]
fn return_small_struct() {
    xc_test!(
        "
        somewhere(); Point2D {
            ; Point2D { x = 1, y = 10 }
        }

        main(); i32 {
            x: Point2D = somewhere()
            ; x.x + x.y
        }
        ";
        11
    )
}

#[test]
fn return_medium_struct() {
    xc_test!(
        "
        somewhere(); Point3D {
            ; Point3D { x = 1, y = 10, z = 100 }
        }

        main(); i32 {
            x: Point3D = somewhere()
            ; x.x + x.y + x.z
        }
        ";
        111
    )
}

#[test]
fn return_large_struct() {
    xc_test!(
        "
        some_numbers(); Numbers5 {
            ; Numbers5 { a = 1, b = 10, c = 100, d = 2000, e = 10000 }
        }

        main(); i32 {
            x: Numbers5 = some_numbers()
            ; x.a + x.b + x.c + x.d + x.e
        }
        ";
        12111
    )
}

#[test]
fn basic_first_class() {
    xc_test!(
        "
        double(in: i32, i_want_zero: bool); i32 {
            ? i_want_zero { ; 0 }

            ; in * 2
        }

        triple(in: i32, i_want_zero: bool); i32 {
            ? i_want_zero { ; 0 }

            ; in * 3
        }

        get_a_function(multiply_amount: i32); (i32, bool); i32 {
            ? multiply_amount == 2 { ; double }

            ; triple
        }

        run_with_5(function: (i32, bool); i32); i32 {
            ; (function)(5, false)
        }

        main(); i32 {
            double: (i32, bool); i32 = get_a_function(2)
            ten: i32 = run_with_5(double)

            triple: (i32, bool); i32 = get_a_function(3)
            fifteen: i32 = run_with_5(triple)

            ; ten + fifteen
        }
        ";
        25
    )
}

#[test]
fn store_first_class() {
    xc_test!(
        "
        Operation = {
            func: (i32); i32,
        }

        &Operation:.run(input: i32); i32 {
            ; (self.func)(input)
        }

        double(in: i32); i32 { ; in * 2 }

        triple(in: i32); i32 { ; in * 3 }

        get_operation(multiply_amount: i32); Operation {
            ? multiply_amount == 2 { 
                ; Operation { func = double } 
            }

            ; Operation { func = triple } 
        }

        main(); i32 {
            double_operation: Operation = get_operation(2)
            ten: i32 = double_operation.run(5)

            triple_operation: Operation = get_operation(3)
            sixty: i32 = triple_operation.run(20)

            ; ten + sixty
        }
        ";
        70
    )
}
