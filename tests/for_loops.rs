//mod test_utils;
//use test_utils::xc_test;
//use cxc::library::StdLib;
//
//#[test]
//fn sum_array() {
//    xc_test!(
//        use StdLib;
//        "
//        main(); i32 {
//            fibon = [1, 1, 2, 3, 5, 8, 13, 21]
//
//            sum = 0
//
//            for fibon {
//                sum += it    
//            }
//
//            ; sum
//        }
//        ";
//        54
//    )
//}
//
//#[test]
//fn sum_array_indices() {
//    xc_test!(
//        use StdLib;
//        "
//        main(); i32 {
//            fibon = [1, 2, 37, 30, 43, 77, 17, 20]
//
//            sum = 0
//
//            for fibon {
//                ? it % 2 == 0 { 
//                    sum = sum + it_index 
//                }
//            }
//
//            ; sum
//        }
//        ";
//        54
//    )
//}
