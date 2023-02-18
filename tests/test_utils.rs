// these functions are used by other modules in the test directory
#![allow(unused_macros)]
#![allow(unused_imports)]
#![allow(dead_code)]
use cxc::XcReflect;

macro_rules! xc_test {
    ($code:expr => $ret:ty; $result:expr) => {{
        let mut unit = cxc::Unit::new();

        unit.add_lib(test_utils::TestUtilsLib::new($code));

        unit.push_script(
            concat!("test() ; ", stringify!($ret), "{ ", $code, " }"),
        ).unwrap();

        let output = unsafe { unit.get_fn_by_name::<(), $ret>("test")(()) };
        assert_eq!(output, $result);
    }};

    (
        $($arg:ident, $arg_ty:ty $(,aka $cxc_type:expr)?; )+ => $ret:ty;
        $code:expr;
        $($arg_val:expr),+ => $result:expr
    ) => {{
        let mut unit = cxc::Unit::new();

        unit.add_lib(test_utils::TestUtilsLib::new($code));

        let mut args = String::new();
        $(
            args += stringify!($arg);

            args += ": ";

            #[allow(unused_mut)]
            #[allow(unused_assignments)]
            let mut arg_type = stringify!($arg_ty);
            $(arg_type = $cxc_type;)?
            args += arg_type;

            args += ", ";
        )*

        unit.push_script(
            &*(
                String::from("test(") +
                &*args +
                concat!(")", "; ", stringify!($ret), "{ ", $code, " }")
            )
        ).unwrap();

        #[allow(unused_parens)]
        let output: $ret = unsafe {
            unit.get_fn_by_name("test")($($arg_val),*)
        };
        assert_eq!(output, $result);
    }};

    ($code:expr) => {{
        let mut unit = cxc::Unit::new();

        unit.add_lib(cxc::library::StdLib);

        unit.add_lib(test_utils::TestUtilsLib::new($code));

        unit.push_script($code).unwrap();

        unsafe { unit.get_fn_by_name::<(), ()>("main")(()) };
    }};

    ($(use $($lib:ident),+;)? $code:expr; $expected_output:expr) => {{
        let mut unit = cxc::Unit::new();

        $( $( unit.add_lib($lib); )* )?

        unit.add_lib(test_utils::TestUtilsLib::new($code));

        unit.push_script($code).unwrap();

        #[allow(unused_assignments)]
        // this is just so output uses the expected output's type
        let mut output = $expected_output;

        output = unsafe { unit.get_fn_by_name::<(), _>("main")(()) };

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
pub struct TwoOf<T> {
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
    }
}

use cxc::{library::Library, Unit};
pub(crate) use xc_test;
