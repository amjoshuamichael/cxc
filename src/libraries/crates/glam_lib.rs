use crate::XcReflect;
use crate::{library::Library, ExternalFuncAdd, Type, TypeRelation};
use glam::{Mat3, Mat4, Vec2, Vec3, Vec4};

pub struct GlamLib;

impl Library for GlamLib {
    fn add_to_unit(&self, unit: &mut crate::Unit) {
        //unit.add_reflect_type::<Vec4>().unwrap();

        let vec3 = unit.add_reflect_type::<Vec3>().unwrap();
        unit.add_external_default::<Vec3>();
        unit.add_external_to_string::<Vec3>();
        unit.add_rust_func_explicit(
            "new",
            Vec3::new as *const usize,
            ExternalFuncAdd {
                arg_types: vec![Type::f32(); 3],
                ret_type: vec3.clone(),
                relation: TypeRelation::Static(vec3),
                ..ExternalFuncAdd::empty()
            },
        );

        let vec2 = unit.add_reflect_type::<Vec2>().unwrap();
        unit.add_external_default::<Vec2>();
        unit.add_external_to_string::<Vec2>();
        unit.add_rust_func_explicit(
            "new",
            Vec2::new as *const usize,
            ExternalFuncAdd {
                arg_types: vec![Type::f32(); 2],
                ret_type: vec2.clone(),
                relation: TypeRelation::Static(vec2),
                ..ExternalFuncAdd::empty()
            },
        );

        unit.add_reflect_type::<Mat3>();
        //unit.add_reflect_type::<Mat4>();
    }
}

impl XcReflect for Vec4 {
    fn alias_code() -> String {
        "Vec4 = {
            inner: {f32, f32, f32, f32}
        }"
        .into()
    }
}

impl XcReflect for Vec3 {
    fn alias_code() -> String {
        "Vec3 = {
            x: f32,
            y: f32,
            z: f32
        }"
        .into()
    }
}

impl XcReflect for Vec2 {
    fn alias_code() -> String {
        "Vec2 = C({
            x: f32,
            y: f32
        })"
        .into()
    }
}

impl XcReflect for Mat3 {
    fn alias_code() -> String {
        "Mat3 = {
            x_axis: Vec3,
            y_axis: Vec3,
            z_axis: Vec3
        }"
        .into()
    }
}

impl XcReflect for Mat4 {
    fn alias_code() -> String {
        "Mat4 = {
            x_axis: Vec4,
            y_axis: Vec4,
            z_axis: Vec4,
            w_axis: Vec4
        }"
        .into()
    }
}
