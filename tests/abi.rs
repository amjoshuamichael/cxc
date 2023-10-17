mod test_utils;

macro_rules! abi_test {
    ($testname:ident, $rtyp:ty; $($tokens:tt)+) => { mod $testname {
        use super::super::test_utils::consume;
        const TOKENS: &str = stringify! { $($tokens)+ };
        #[test]
        fn pass() {
            let mut unit = cxc::Unit::new();
            let [xctype, xcvalue, combine_func] = 
                &*TOKENS.split("as").collect::<Vec<_>>() else { unreachable!() };

            let out_type = std::any::type_name::<$rtyp>();

            unit.add_lib(cxc::library::StdLib);

            unit.push_script(&*format!(r#"
                combine(x: {xctype}); {out_type} {{
                    ; {combine_func}
                }}
            "#))
            .unwrap();

            unit.push_script(&*format!(r#"
                main(); bool {{
                    x: {xctype} = {xcvalue}
                    combined_by_function := combine(x)
                    combined_here := {combine_func}
                    ; combined_by_function == combined_here
                }}
            "#))
            .unwrap();

            let function = unit.get_fn("main").unwrap().downcast::<(), bool>();
            #[allow(unused_unsafe)]
            let output = unsafe { function() };
            assert_eq!(consume::<bool>(output), true);
        }

        #[test]
        fn ret() {
            let mut unit = cxc::Unit::new();
            let [xctype, xcvalue, combine_x] = 
                &*TOKENS.split("as").collect::<Vec<_>>() else { unreachable!() };

            let combine_y = combine_x.replace("x", "y");

            unit.push_script(&*format!(r#"
                generate(); {xctype} {{
                    ; {xcvalue}
                }}
            "#))
            .unwrap();

            unit.push_script(&*format!(
                "main(); bool {{
                    x: {xctype} = {xcvalue}
                    y := generate()
                    combined_from_function := {combine_y}
                    combined_here := {combine_x}
                    ; combined_from_function == combined_here
                }}",
            ))
            .unwrap();

            let function = unit.get_fn("main").unwrap().downcast::<(), bool>();
            #[allow(unused_unsafe)]
            let output = unsafe { function() };
            assert_eq!(consume::<bool>(output), true);
        }

        #[test]
        fn through() {
            let mut unit = cxc::Unit::new();
            let [xctype, xcvalue, combine_x] = 
                &*TOKENS.split("as").collect::<Vec<_>>() else { unreachable!() };

            let combine_y = combine_x.replace("x", "y");

            unit.push_script(&*format!(r#"
                through(val: {xctype}); {xctype} {{
                    ; val
                }}
            "#))
            .unwrap();

            unit.push_script(&*format!(
                "main(); bool {{
                    x: {xctype} = {xcvalue}
                    y := through(x)
                    combined_from_function := {combine_y}
                    combined_here := {combine_x}
                    ; combined_from_function == combined_here
                }}",
            ))
            .unwrap();

            let function = unit.get_fn("main").unwrap().downcast::<(), bool>();
            #[allow(unused_unsafe)]
            let output = unsafe { function() };
            assert_eq!(consume::<bool>(output), true);
        }

        #[test]
        fn all() {
            let mut unit = cxc::Unit::new();
            let [xctype, xcvalue, combine] = 
                &*TOKENS.split("as").collect::<Vec<_>>() else { unreachable!() };

            let out_type = std::any::type_name::<$rtyp>();

            unit.push_script(&*format!(r#"
                generate(); {xctype} {{
                    ; {xcvalue}
                }}

                through(val: {xctype}); {xctype} {{
                    ; val
                }}

                combine(x: {xctype}); {out_type} {{
                    ; {combine}
                }}
            "#))
            .unwrap();

            unit.push_script(&*format!(
                "main(); bool {{
                    x: {xctype} = {xcvalue}
                    generated := generate()
                    through := through(generated)
                    combined_from_function := combine(through)
                    combined_here := {combine}
                    ; combined_from_function == combined_here
                }}",
            ))
            .unwrap();

            let function = unit.get_fn("main").unwrap().downcast::<(), bool>();
            #[allow(unused_unsafe)]
            let output = unsafe { function() };
            assert_eq!(consume::<bool>(output), true);

        }
    }}
}

macro_rules! abi_tests_int {
    ($modname:ident, $on:tt) => { mod $modname {
        abi_test!(alone, $on; $on as 21 as x);
        abi_test!(one, $on; { $on, } as { 21 } as x.0);
        abi_test!(two, $on; { $on, $on } as { 21, 22 } as x.0 + x.1);
        abi_test!(three, $on; { $on, $on, $on } as { 21, 22, 23 } as x.0 + x.1 + x.2);
        abi_test!(four, $on; 
                  { $on, $on, $on, $on } 
                  as { 21, 22, 23, 24 } 
                  as x.0 + x.1 + x.2 + x.3);
        abi_test!(five, $on; 
                  { $on, $on, $on, $on, $on } as 
                  { 21, 22, 23, 24, 25 } as 
                  x.0 + x.1 + x.2 + x.3);
    }}
}

macro_rules! abi_tests_two {
    ($modname:ident, $a:tt, $b:tt, $aval:expr, $bval:expr) => { mod $modname {
        abi_test!(ab, bool; { $a, $b } as { $aval, $bval } as x.0 == $aval && x.1 == $bval);
        abi_test!(ba, bool; { $b, $a } as { $bval, $aval } as x.0 == $bval && x.1 == $aval);
        abi_test!(aab, bool; { $a, $a, $b } 
                  as { $aval, $aval, $bval } 
                  as x.0 == $aval && x.1 == $aval && x.2 == $bval);
        abi_test!(bba, bool; { $b, $b, $a } 
                  as { $bval, $bval, $aval } 
                  as x.0 == $bval && x.1 == $bval && x.2 == $aval);
        abi_test!(abb, bool; { $a, $b, $b } 
                  as { $aval, $bval, $bval } 
                  as x.0 == $aval && x.1 == $bval && x.2 == $bval);
        abi_test!(baa, bool; { $b, $a, $a } 
                  as { $bval, $aval, $aval } 
                  as x.0 == $bval && x.1 == $aval && x.2 == $aval);
        abi_test!(a_ab, bool; { $a, { $a, $b } } 
                  as { $aval, { $aval, $bval } } 
                  as x.0 == $aval && x.1.0 == $aval && x.1.1 == $bval);
        abi_test!(b_ba, bool; { $b, { $b, $a } } 
                  as { $bval, { $bval, $aval } } 
                  as x.0 == $bval && x.1.0 == $bval && x.1.1 == $aval);
        abi_test!(a_bb, bool; { $a, { $b, $b } } 
                  as { $aval, { $bval, $bval } } 
                  as x.0 == $aval && x.1.0 == $bval && x.1.1 == $bval);
        abi_test!(b_aa, bool; { $b, { $a, $a } } 
                  as { $bval, { $aval, $aval } } 
                  as x.0 == $bval && x.1.0 == $aval && x.1.1 == $aval);
        abi_test!(aa_b, bool; { { $a, $a }, $b } 
                  as { { $aval, $aval }, $bval } 
                  as x.0.0 == $aval && x.0.1 == $aval && x.1 == $bval);
        abi_test!(bb_a, bool; { { $b, $b }, $a } 
                  as { { $bval, $bval }, $aval } 
                  as x.0.0 == $bval && x.0.1 == $bval && x.1 == $aval);
        abi_test!(ab_b, bool; { { $a, $b }, $b } 
                  as { { $aval, $bval }, $bval } 
                  as x.0.0 == $aval && x.0.1 == $bval && x.1 == $bval);
        abi_test!(ba_a, bool; { { $b, $a }, $a } 
                  as { { $bval, $aval }, $aval } 
                  as x.0.0 == $bval && x.0.1 == $aval && x.1 == $aval);
    }}
}

macro_rules! abi_tests_float {
    ($modname:ident, $on:tt) => { mod $modname {
        abi_test!(alone, $on; $on as 21.3 as x);
        abi_test!(one, $on; { $on, } as { 21.3 } as x.0);
        abi_test!(two, $on; { $on, $on } as { 21.3, 22.4 } as x.0 + x.1);
        abi_test!(onetwo, $on; { $on, { $on, $on } } 
                  as { 21.3, { 22.4, 23.5 } } 
                  as x.0 + x.1.0 + x.1.1);
        abi_test!(twoone, $on; { { $on, $on }, $on } 
                  as { { 21.3, 22.4 }, 23.5 } 
                  as x.0.0 + x.0.1 + x.1);
        abi_test!(three, $on; { $on, $on, $on } as { 21.3, 22.4, 23.5 } as x.0 + x.1 + x.2);
        abi_test!(four, $on; 
                  { $on, $on, $on, $on } 
                  as { 21.3, 22.4, 23.5, 24.6 } 
                  as x.0 + x.1 + x.2 + x.3);
        abi_test!(five, $on; 
                  { $on, $on, $on, $on, $on } as 
                  { 21.3, 22.4, 23.5, 24.6, 25.7 } as 
                  x.0 + x.1 + x.2 + x.3);
    }}
}

abi_tests_int!(i8, i8);
abi_tests_int!(u8, u8);
abi_tests_int!(i16, i16);
abi_tests_int!(u16, u16);
abi_tests_int!(i32, i32);
abi_tests_int!(u32, u32);
abi_tests_int!(i64, i64);
abi_tests_int!(u64, u64);

abi_tests_float!(f32, f32);
abi_tests_float!(f64, f64);

// u8 max is 255
// u16 max is 65535
// u32 max is 4_294_967_295
// u64 max is 18_446_744_073_709_551_615
// f32 example is 1.23456
// f64 example is 1.234567890123456
abi_tests_two!(u8u64, u8, u64, 251, 18_446_744_073_709_551_611);
abi_tests_two!(u16u64, u16, u64, 65531, 18_446_744_073_709_551_611);
abi_tests_two!(u32u64, u32, u64, 4_294_967_291, 18_446_744_073_709_551_611);
abi_tests_two!(u8u32, u8, u32, 251, 4_294_967_291);
abi_tests_two!(u16u32, u16, u32, 65531, 4_294_967_291);
abi_tests_two!(u8u16, u8, u16, 251, 65531);
abi_tests_two!(f32f64, f32, f64, 1.23456, 1.234567890123456);

