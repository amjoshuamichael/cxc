mod test_utils;

use std::rc::Rc;

use cxc::{library::StdLib, Unit};
use test_utils::{xc_test, Point3D};

#[test]
#[ignore]
fn basic_union() {
    xc_test!(
        "
        Vec2 = { x: i32, y: i32 }
        Vec3 = Vec2 + { z: i32 }

        main(); i32 {
            x: Vec3 = Vec3 {
                x = 3,
                y = 90,
                z = 43,
            }
        }
        ";
        Point3D { x: 3, y: 90, z: 43 }
    )
}
