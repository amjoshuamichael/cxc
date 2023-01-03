use crate::XcReflect;
use crate::{library::Library, ExternalFuncAdd, Type, TypeRelation};
use glam::{Mat2, Mat3, Mat4, Vec2, Vec3};

pub struct GlamLib;

impl Library for GlamLib {
    fn add_to_unit(&self, unit: &mut crate::Unit) {
        let vec3 = unit.add_reflect_type::<Vec3>().unwrap();
        dbg!(&vec3.size());
        let mut vec = Vec3::default();
        unit.add_rust_func_explicit(
            "new",
            new_vec3 as *const usize,
            ExternalFuncAdd {
                arg_types: vec![Type::f32(); 3],
                ret_type: vec3.clone(),
                relation: TypeRelation::Static(vec3),
                ..ExternalFuncAdd::empty()
            },
        );
        let vec2 = unit.add_reflect_type::<Vec2>().unwrap();
        unit.add_rust_func_explicit(
            "new",
            new_vec2 as *const usize,
            ExternalFuncAdd {
                arg_types: vec![Type::f32(); 2],
                ret_type: vec2.clone(),
                relation: TypeRelation::Static(vec2),
                ..ExternalFuncAdd::empty()
            },
        );

        unit.add_opaque_type::<Mat4>();
        unit.add_opaque_type::<Mat3>();
        unit.add_opaque_type::<Mat2>();
    }
}

pub fn new_vec3(x: f32, y: f32, z: f32) -> Vec3 {
    let mut vec = Vec3::default();
    vec.x = x;
    vec.y = y;
    vec.z = z;
    vec
}
pub fn new_vec2(x: f32, y: f32) -> Vec2 { Vec2::new(x, y) }

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
