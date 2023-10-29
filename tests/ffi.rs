#![allow(unused_must_use)]
#![allow(unused_imports)]
mod test_utils;

use cxc::{ExternalFuncAdd, TypeRelation, library::StdLib};
use cxc::{Type, Unit};
use test_utils::Point2D;
use std::rc::Rc;

#[test]
#[cfg(not(feature = "backend-interpreter"))]
fn basic_pointer() {
    let mut unit = Unit::new();

    unit.push_script(
        "
        square(num: &i32) {
            *num = *num * *num
        }
        ",
    );

    let mut num = 4;
    unit.get_fn("square")
        .unwrap()
        .downcast::<(&mut i32,), i32>()(&mut num);
    assert_eq!(num, 16);
}

#[test]
#[cfg_attr(feature = "backend-interpreter", ignore)]
fn struct_pointer() {
    let mut unit = Unit::new();

    unit.push_script(
        "
        Point2D = {
            x: i32,
            y: i32
        }

        sqr_magnitude_of(in: &Point2D); i32 {
            ; in.x * in.x + in.y * in.y
        }
        ",
    )
    .unwrap();

    let point = Point2D { x: 2, y: 3 };

    let sqr_mag = unit
        .get_fn("sqr_magnitude_of")
        .unwrap()
        .downcast::<(&Point2D,), i32>()(&point);
    assert_eq!(sqr_mag, 13);
}

#[test]
#[cfg(not(feature = "backend-interpreter"))]
fn struct_rc_pointer() {
    let mut unit = Unit::new();

    unit.add_lib(StdLib);
    unit.push_script(
        r#"
        Point2D = {
            x: i32,
            y: i32
        }

        sqr_magnitude_of(in: Rc<Point2D>); i32 {
            output := in.x * in.x + in.y * in.y
            ; output
        }
        "#,
    )
    .unwrap();

    let point = Point2D { x: 2, y: 3 };

    let sqr_mag = unit
        .get_fn("sqr_magnitude_of")
        .unwrap()
        .downcast::<(Rc<Point2D>,), i32>()(Rc::new(point));
    assert_eq!(sqr_mag, 13);
}

#[test]
#[cfg_attr(feature = "backend-interpreter", ignore)]
fn i32_and_ref() {
    let mut unit = Unit::new();

    unit.push_script(
        "
        main(pointer: &i32); {i32, &i32} {
            output: {i32, &i32} = {i32, &i32} { 10, pointer }
            ; output
        }
        ",
    )
    .unwrap();

    let mut thirty_two = 32;
    let func = unit
        .get_fn("main")
        .unwrap()
        .downcast::<(&i32,), (i32, &i32)>();
    let output: (i32, &i32);

    output = func(&mut thirty_two);
    assert_eq!(output, (10, &thirty_two));
}

#[test]
#[cfg_attr(feature = "backend-interpreter", ignore)]
fn method_on_struct_with_arg() {
    let mut unit = Unit::new();

    let point2d = unit.add_reflect_type::<Point2D>().unwrap();
    unit.add_rust_func_explicit(
        "magnify",
        Point2D::magnify as *const usize,
        ExternalFuncAdd {
            arg_types: vec![point2d.get_ref().clone(), Type::i(32)].clone(),
            ret_type: point2d.clone(),
            relation: TypeRelation::MethodOf(point2d.get_ref()),
            ..ExternalFuncAdd::empty()
        },
    );
    
    unit.push_script(
        "
        main(); i32 {
            point: Point2D = Point2D { x = 42, y = 43 }
            point = point.magnify(40)
            ; point.x
        }
        ",
    )
    .unwrap();

    let output = unit.get_fn("main").unwrap().downcast::<(), i32>()();
    assert_eq!(output, 1680);
}

macro_rules! ffi_test {
    ($testname:ident, $rtyp:ty, $rval:expr, $check:expr, $($xctypeandval:tt)+) => { 
        // TODO: make tests will work on ffi with the interpreter.
        #[cfg(not(feature = "backend-interpreter"))]
        mod $testname {
            #[allow(unused_imports)]
            use super::*;

            const CHECK: &str = $check;
            const XCTYPEANDVAL: &str = stringify! { $($xctypeandval)+ };

            #[test]
            fn pass_r2c() {
                let mut unit = cxc::Unit::new();

                let [xctype, _] = &*XCTYPEANDVAL.split("as").collect::<Vec<_>>()
                    else { unreachable!() };
                
                unit.push_script(
                    &*format!("check(x: {xctype}); bool {{ ; {CHECK} }}")
                ).unwrap();
                let check = unit.get_fn("check").unwrap().downcast::<($rtyp,), bool>();
                assert!(check($rval));
            }

            #[test]
            fn ret_c2r() {
                let mut unit = cxc::Unit::new();

                let [xctype, xcval] = &*XCTYPEANDVAL.split("as").collect::<Vec<_>>()
                    else { unreachable!() };

                unit.push_script(
                    &*format!("main(); {xctype} {{ ; {xcval} }}")
                ).unwrap();
                let main = unit.get_fn("main").unwrap().downcast::<(), $rtyp>();
                let x = main();

                #[cfg(feature = "show-bytes")]
                cxc::bytesof::print_binary_two(&$rval, &x);
                assert_eq!(x, $rval);
            }

            #[test]
            fn ret_r2c() {
                fn external() -> $rtyp { $rval }

                let mut unit = cxc::Unit::new();

                let [xctype, _] = &*XCTYPEANDVAL.split("as").collect::<Vec<_>>()
                    else { unreachable!() };

                unit.add_rust_func_explicit(
                    "external",
                    external as *const usize,
                    cxc::ExternalFuncAdd {
                        ret_type: unit.comp_data.ty(xctype),
                        ..cxc::ExternalFuncAdd::empty()
                    }
                );

                unit.push_script(&*format!("
                    main(); bool {{ 
                        x := external()
                        ; {CHECK}
                    }}
                ")).unwrap();
                let main = unit.get_fn("main").unwrap().downcast::<(), bool>();
                assert!(main());
            }

            #[test]
            fn pass_c2r() {
                fn external(x: $rtyp) -> bool { x == $rval }

                let mut unit = cxc::Unit::new();

                let [xctype, xcval] = &*XCTYPEANDVAL.split("as").collect::<Vec<_>>()
                    else { unreachable!() };

                unit.add_rust_func_explicit(
                    "external",
                    external as *const usize,
                    cxc::ExternalFuncAdd {
                        ret_type: cxc::Type::bool(),
                        arg_types: vec![unit.comp_data.ty(xctype)],
                        ..cxc::ExternalFuncAdd::empty()
                    }
                );

                unit.push_script(&*format!(
                        "main(); bool {{ ;external({xcval}) }}"
                )).unwrap();
                let main = unit.get_fn("main").unwrap().downcast::<(), bool>();
                assert!(main());
            }
        }
    }
}

macro_rules! ffi_tests_int {
    ($ty:tt) => { mod $ty {
        ffi_test!(alone, $ty, 41, "x == 41", $ty as 41);
        ffi_test!(one, ($ty,), (41,), "x.0 == 41", {$ty,} as {41,});
        ffi_test!(two, ($ty, $ty), 
                  (41,42), "x.0 == 41 && x.1 == 42", 
                  {$ty,$ty} as {41,42});
        ffi_test!(three, ($ty, $ty, $ty), 
                  (41,42,43), "x.0 == 41 && x.1 == 42 && x.2 == 43", 
                  {$ty,$ty,$ty} as {41,42,43});
        ffi_test!(four, ($ty, $ty, $ty, $ty), 
                  (41,42,43,44), "x.0 == 41 && x.1 == 42 && x.2 == 43 && x.3 == 44", 
                  {$ty,$ty,$ty,$ty} as {41,42,43,44});
        ffi_test!(five, ($ty, $ty, $ty, $ty, $ty), 
                  (41,42,43,44,45), "x.0 == 41 && x.1 == 42 && x.2 == 43 && x.3 == 44 && x.4 == 45", 
                  {$ty,$ty,$ty,$ty,$ty} as {41,42,43,44,45});
    }}
}

macro_rules! ffi_tests_float {
    ($ty:tt) => { mod $ty {
        ffi_test!(alone, $ty, 4.1, "x == 4.1", $ty as 4.1);
        ffi_test!(one, ($ty,), (4.1,), "x.0 == 4.1", {$ty,} as {4.1,});
        ffi_test!(two, ($ty, $ty), 
                  (4.1,4.2), "x.0 == 4.1 && x.1 == 4.2", 
                  {$ty,$ty} as {4.1,4.2});
        ffi_test!(three, ($ty, $ty, $ty), 
                  (4.1,4.2,4.3), "x.0 == 4.1 && x.1 == 4.2 && x.2 == 4.3", 
                  {$ty,$ty,$ty} as {4.1,4.2,4.3});
        ffi_test!(four, ($ty, $ty, $ty, $ty), 
                  (4.1,4.2,4.3,4.4), "x.0 == 4.1 && x.1 == 4.2 && x.2 == 4.3 && x.3 == 4.4", 
                  {$ty,$ty,$ty,$ty} as {4.1,4.2,4.3,4.4});
        ffi_test!(five, ($ty, $ty, $ty, $ty, $ty), 
                  (4.1,4.2,4.3,4.4,4.5), "x.0 == 4.1 && x.1 == 4.2 && x.2 == 4.3 && x.3 == 4.4 && x.4 == 4.5", 
                  {$ty,$ty,$ty,$ty,$ty} as {4.1,4.2,4.3,4.4,4.5});
    }}
}

macro_rules! ffi_tests_two {
    ($modname:ident, $a:tt, $b:tt, $aval:tt, $bval:tt) => { mod $modname {
        #![allow(non_camel_case_types)]
        #[derive(Clone, Copy, Debug, PartialEq)]
        #[repr(C)]
        struct AB($a,$b);
        #[derive(Clone, Copy, Debug, PartialEq)]
        #[repr(C)]
        struct BA($b,$a);
        #[derive(Clone, Copy, Debug, PartialEq)]
        #[repr(C)]
        struct AA($a,$a);
        #[derive(Clone, Copy, Debug, PartialEq)]
        #[repr(C)]
        struct BB($b,$b);

        ffi_test!(ab, AB, AB($aval,$bval), 
                  stringify! { x.0 == $aval && x.1 == $bval }, 
                  {$a,$b} as {$aval,$bval});
        ffi_test!(ba, BA, BA($bval,$aval), 
                  stringify! { x.0 == $bval && x.1 == $aval }, 
                  {$b,$a} as {$bval,$aval});

        #[derive(Clone, Copy, Debug, PartialEq)]
        #[repr(C)]
        struct AAB($a,$a,$b);
        ffi_test!(aab, AAB, AAB($aval,$aval,$bval), 
                  stringify! { x.0 == $aval && x.1 == $aval && x.2 == $bval },
                  {$a,$a,$b} as {$aval,$aval,$bval});
        #[derive(Clone, Copy, Debug, PartialEq)]
        #[repr(C)]
        struct BBA($b,$b,$a);
        ffi_test!(bba, BBA, BBA($bval,$bval,$aval), 
                  stringify! { x.0 == $bval && x.1 == $bval && x.2 == $aval },
                  {$b,$b,$a} as {$bval,$bval,$aval});
        #[derive(Clone, Copy, Debug, PartialEq)]
        #[repr(C)]
        struct ABB($a,$b,$b);
        ffi_test!(abb, ABB, ABB($aval,$bval,$bval), 
                  stringify! { x.0 == $aval && x.1 == $bval && x.2 == $bval },
                  {$a,$b,$b} as {$aval,$bval,$bval});
        #[derive(Clone, Copy, Debug, PartialEq)]
        #[repr(C)]
        struct BAA($b,$a,$a);
        ffi_test!(baa, BAA, BAA($bval,$aval,$aval), 
                  stringify! { x.0 == $bval && x.1 == $aval && x.2 == $aval },
                  {$b,$a,$a} as {$bval,$aval,$aval});

        #[derive(Clone, Copy, Debug, PartialEq)]
        #[repr(C)]
        struct AA_B(AA,$b);
        ffi_test!(aa_b, AA_B, AA_B(AA($aval,$aval),$bval), 
                  stringify! { x.0.0 == $aval && x.0.1 == $aval && x.1 == $bval },
                  {{$a,$a},$b} as {{$aval,$aval},$bval});
        #[derive(Clone, Copy, Debug, PartialEq)]
        #[repr(C)]
        struct BB_A(BB,$a);
        ffi_test!(bb_a, BB_A, BB_A(BB($bval,$bval),$aval), 
                  stringify! { x.0.0 == $bval && x.0.1 == $bval && x.1 == $aval },
                  {{$b,$b},$a} as {{$bval,$bval},$aval});
        #[derive(Clone, Copy, Debug, PartialEq)]
        #[repr(C)]
        struct AB_B(AB,$b);
        ffi_test!(ab_b, AB_B, AB_B(AB($aval,$bval),$bval), 
                  stringify! { x.0.0 == $aval && x.0.1 == $bval && x.1 == $bval },
                  {{$a,$b},$b} as {{$aval,$bval},$bval});
        #[derive(Clone, Copy, Debug, PartialEq)]
        #[repr(C)]
        struct BA_A(BA,$a);
        ffi_test!(ba_a, BA_A, BA_A(BA($bval,$aval),$aval), 
                  stringify! { x.0.0 == $bval && x.0.1 == $aval && x.1 == $aval },
                  {{$b,$a},$a} as {{$bval,$aval},$aval});

        #[derive(Clone, Copy, Debug, PartialEq)]
        #[repr(C)]
        struct A_AB($a,AB);
        ffi_test!(a_ab, A_AB, A_AB($aval,AB($aval,$bval)), 
                  stringify! { x.0 == $aval && x.1.0 == $aval && x.1.1 == $bval },
                  {$a,{$a,$b}} as {$aval,{$aval,$bval}});
        #[derive(Clone, Copy, Debug, PartialEq)]
        #[repr(C)]
        struct B_BA($b,BA);
        ffi_test!(b_ba, B_BA, B_BA($bval,BA($bval,$aval)), 
                  stringify! { x.0 == $bval && x.1.0 == $bval && x.1.1 == $aval },
                  {$b,{$b,$a}} as {$bval,{$bval,$aval}});
        #[derive(Clone, Copy, Debug, PartialEq)]
        #[repr(C)]
        struct A_BB($a,BB);
        ffi_test!(a_bb, A_BB, A_BB($aval,BB($bval,$bval)), 
                  stringify! { x.0 == $aval && x.1.0 == $bval && x.1.1 == $bval },
                  {$a,{$b,$b}} as {$aval,{$bval,$bval}});
        #[derive(Clone, Copy, Debug, PartialEq)]
        #[repr(C)]
        struct B_AA($b,AA);
        ffi_test!(b_aa, B_AA, B_AA($bval,AA($aval,$aval)), 
                  stringify! { x.0 == $bval && x.1.0 == $aval && x.1.1 == $aval },
                  {$b,{$a,$a}} as {$bval,{$aval,$aval}});

    }}
}

ffi_tests_int!(i8);
ffi_tests_int!(u8);
ffi_tests_int!(i16);
ffi_tests_int!(u16);
ffi_tests_int!(i32);
ffi_tests_int!(u32);
ffi_tests_int!(i64);
ffi_tests_int!(u64);
ffi_tests_float!(f32);
ffi_tests_float!(f64);

// u8 max is 255
// u16 max is 65535
// u32 max is 4_294_967_295
// u64 max is 18_446_744_073_709_551_615
// f32 example is 1.23456
// f64 example is 1.234567890123456
ffi_tests_two!(u8u64, u8, u64, 251, 18_446_744_073_709_551_611);
ffi_tests_two!(u16u64, u16, u64, 65531, 18_446_744_073_709_551_611);
ffi_tests_two!(u32u64, u32, u64, 4_294_967_291, 18_446_744_073_709_551_611);
ffi_tests_two!(u8u32, u8, u32, 251, 4_294_967_291);
ffi_tests_two!(u16u32, u16, u32, 65531, 4_294_967_291);
ffi_tests_two!(u8u16, u8, u16, 251, 65531);
ffi_tests_two!(f32f64, f32, f64, 2.23456, 1.234567890123456);
