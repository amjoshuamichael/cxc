// these functions are used by other modules in the test directory
#![allow(unused_macros)]
#![allow(unused_imports)]
#![allow(dead_code)]
use cxc::XcReflect;

macro_rules! xc_test {
    ($code:expr) => {
        xc_test! (use cxc::library::StdLib; $code; ())
    };

    ($(use $($lib:path),+;)? $code:expr; $expected_output:expr) => {{
        let mut unit = cxc::Unit::new();

        $( $( unit.add_lib($lib); )* )?

        unit.add_lib(test_utils::TestUtilsLib::new($code));

        let code = $code;
        if code.contains("main()") {
            unit.push_script($code).unwrap();
        } else {
            unit.push_script(&*format!("main() {code}")).unwrap();
        }


        #[allow(unused_assignments)]
        // this is just so output uses the expected output's type
        let mut output = $expected_output;

        #[cfg(feature = "llvm-debug")]
        println!("--getting function--");

        let function = unit.get_fn("main").unwrap().downcast::<(), _>();

        #[cfg(feature = "llvm-debug")]
        println!("--running function--");

        output = function();

        #[cfg(feature = "show-bytes")]
        cxc::bytesof::print_binary_two(&$expected_output, &output);

        assert_eq!(output, $expected_output);
    }};
}

pub struct TestUtilsLib<'a> {
    code: &'a str,
}

#[derive(Clone, Copy, Debug, XcReflect, PartialEq, Eq)]
pub struct Point2D {
    pub x: i32,
    pub y: i32,
}

impl Point2D {
    pub fn sqr_magnitude(&self) -> i32 { self.x * self.x + self.y * self.y }
    pub fn magnify(&self, by: i32) -> Point2D {
        Point2D {
            x: self.x * by,
            y: self.y * by,
        }
    }
}

#[derive(Default, Clone, Copy, Debug, PartialEq, Eq)]
pub struct TwoOf<T: 'static> {
    pub one: T,
    pub two: T,
}

impl<T> XcReflect for TwoOf<T> {
    fn alias_code() -> String { "TwoOf<T> = { one: T, two: T }".into() }
}

#[derive(Clone, Copy, Debug, XcReflect, PartialEq, Eq)]
pub struct Point3D {
    pub x: i32,
    pub y: i32,
    pub z: i32,
}

#[derive(Clone, Copy, Debug, XcReflect, PartialEq, Eq)]
pub struct Numbers4 {
    pub a: i32,
    pub b: i32,
    pub c: i32,
    pub d: i32,
}

#[derive(Default, Clone, Copy, Debug, XcReflect, PartialEq, Eq)]
pub struct Numbers5 {
    pub a: i32,
    pub b: i32,
    pub c: i32,
    pub d: i32,
    pub e: i32,
}

#[derive(Default, Clone, Debug, XcReflect, PartialEq, Eq)]
pub struct Strings4 {
    pub a: String,
    pub b: String,
    pub c: String,
    pub d: String,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BoolMore {
    Bool(bool),
    V2,
    V3,
    V4,
    V5,
}

impl XcReflect for BoolMore {
    fn alias_code() -> String {
        "BoolMore = { Bool: bool / V2: {} / V3: {} / V4: {} / V5: {} }".into()
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BoolMM {
    BoolM(BoolMore),
    V6,
    V7,
    V8,
    V9,
}

impl XcReflect for BoolMM {
    fn alias_code() -> String {
        "BoolMM = { BoolM: BoolMore / V6: {} / V7: {} / V8: {} / V9: {} }".into()
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BoolMMM {
    BoolMM(BoolMM),
    V10,
    V11,
    V12,
    V13,
}

impl XcReflect for BoolMMM {
    fn alias_code() -> String {
        "BoolMMM = { BoolMM: BoolMM / V10: {} / V11: {} / V12: {} / V13: {} }".into()
    }
}

impl<'a> TestUtilsLib<'a> {
    pub(crate) fn new(code: &'a str) -> Self { TestUtilsLib { code } }
}

impl<'a> Library for TestUtilsLib<'a> {
    fn add_to_unit(&self, unit: &mut Unit) {
        if self.code.contains("TwoOf<") {
            unit.add_reflect_type::<TwoOf<()>>();
        }

        if self.code.contains("Point2D") {
            unit.add_reflect_type::<Point2D>();
        }

        if self.code.contains("Point3D") {
            unit.add_reflect_type::<Point3D>();
        }

        if self.code.contains("Numbers4") {
            unit.add_reflect_type::<Numbers4>();
        }

        if self.code.contains("Numbers5") {
            unit.add_reflect_type::<Numbers5>();
        }

        if self.code.contains("Strings4") {
            unit.add_reflect_type::<Strings4>();
        }

        if self.code.contains("BoolMore") {
            unit.add_reflect_type::<BoolMore>();
        }

        if self.code.contains("BoolMM") {
            unit.add_reflect_type::<BoolMM>();
        }

        if self.code.contains("BoolMMM") {
            unit.add_reflect_type::<BoolMMM>();
        }
    }
}

use cxc::{library::Library, Unit};
pub(crate) use xc_test;
