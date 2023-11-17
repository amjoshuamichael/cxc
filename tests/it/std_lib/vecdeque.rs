use crate::test_utils::xc_test;
use cxc::library::StdLib;
use std::collections::VecDeque;

#[test]
#[cfg(not(feature = "backend-interpreter"))]
fn new() {
    xc_test!(
        use StdLib;
        "
        main(); VecDeque<i32> {
            ; VecDeque<i32>:new()
        }
        ";
        VecDeque::<i32>::new()
    )
}

#[test]
#[cfg(not(feature = "backend-interpreter"))]
fn push_5() {
    xc_test!(
        use StdLib;
        "
        main(); VecDeque<i32> {
            vec := VecDeque<i32>:new()
            vec.push_back(30)
            vec.push_back(90)
            vec.push_back(89)
            vec.push_back(328)
            vec.push_back(904)
            ; vec 
        }
        ";
        {
            let mut vec = VecDeque::<i32>::new();
            vec.push_back(30);
            vec.push_back(90);
            vec.push_back(89);
            vec.push_back(328);
            vec.push_back(904);
            vec
        }
    )
}

#[test]
#[cfg(not(feature = "backend-interpreter"))]
fn configurations() {
    xc_test!(
        use StdLib;
        "
        main(); {VecDeque<i32>, VecDeque<i32>, VecDeque<i32>} {
            vec1 := VecDeque<i32>:new()
            push_back_x_times(&vec1, 30, 5) # expand case A
            pop_front_x_times(&vec1, 2)
            push_back_x_times(&vec1, 50, 4)

            vec2 := VecDeque<i32>:new() 
            push_back_x_times(&vec2, 30, 7)
            pop_front_x_times(&vec2, 2)
            push_back_x_times(&vec2, 50, 7) # expand case B

            vec3 := VecDeque<i32>:new() 
            push_back_x_times(&vec3, 30, 7)
            pop_front_x_times(&vec3, 6)
            push_back_x_times(&vec3, 50, 12) # expand case C

            ; {vec1, vec2, vec3}
        }

        push_back_x_times(vec: &VecDeque<i32>, val: i32, count: i32) {
            i := 0
            @ i < count {
                vec.push_back(val)

                i = i + 1
            }
        }

        pop_front_x_times(vec: &VecDeque<i32>, count: i32) {
            i := 0
            @ i < count {
                vec.pop_front()

                i = i + 1
            }
        }
        ";
        (
            [30, 30, 30, 50, 50, 50, 50]
                .into_iter().collect::<VecDeque<i32>>(),
            [30, 30, 30, 30, 30, 50, 50, 50, 50, 50, 50, 50]
                .into_iter().collect::<VecDeque<i32>>(),
            [30, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50]
                .into_iter().collect::<VecDeque<i32>>(),
        )
    )
}

// This test is here to check frees under the interpreter
#[test]
fn configurations_no_return() {
    xc_test!(
        use StdLib;
        "
        main() {
            vec1 := VecDeque<i32>:new()
            push_back_x_times(&vec1, 30, 5) # expand case A
            pop_front_x_times(&vec1, 2)
            push_back_x_times(&vec1, 50, 4)

            vec2 := VecDeque<i32>:new() 
            push_back_x_times(&vec2, 30, 7)
            pop_front_x_times(&vec2, 2)
            push_back_x_times(&vec2, 50, 7) # expand case B

            vec3 := VecDeque<i32>:new() 
            push_back_x_times(&vec3, 30, 7)
            pop_front_x_times(&vec3, 6)
            push_back_x_times(&vec3, 50, 12) # expand case C
        }

        push_back_x_times(vec: &VecDeque<i32>, val: i32, count: i32) {
            i := 0
            @ i < count {
                vec.push_back(val)

                i = i + 1
            }
        }

        pop_front_x_times(vec: &VecDeque<i32>, count: i32) {
            i := 0
            @ i < count {
                vec.pop_front()

                i = i + 1
            }
        }
        "
    )
}
