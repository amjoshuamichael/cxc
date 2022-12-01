use crate::library::Library;
use crate::XcReflect;
use glam::{Mat2, Mat3, Mat4, Vec2, Vec3};

pub struct GlamLib;

impl Library for GlamLib {
    fn add_to_unit(&self, unit: &mut crate::Unit) {
        unit.add_reflect_type::<Vec3>();
        unit.add_reflect_type::<Vec2>();

        unit.add_opaque_type::<Mat4>();
        unit.add_opaque_type::<Mat3>();
        unit.add_opaque_type::<Mat2>();
    }
}

impl XcReflect for Vec3 {
    fn alias_code<'a>() -> &'a str {
        "Vec3 = {
            x: f32,
            y: f32,
            z: f32
        }"
    }
}

impl XcReflect for Vec2 {
    fn alias_code<'a>() -> &'a str {
        "Vec2 = {
            x: f32,
            y: f32
        }"
    }
}
