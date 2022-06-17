use crate::hlr::prelude::*;

lazy_static! {
    pub static ref CORE_LIB: TypeGroup = {
        let mut core_lib = TypeGroup::default();

        // The primitive library is a library of types that make up the edges of all other types.
        // No struct can exist without these at the edges.
        core_lib.add(Type::new_prim("u8"));
        core_lib.add(Type::new_prim("u16"));
        core_lib.add(Type::new_prim("u32"));
        core_lib.add(Type::new_prim("u64"));

        core_lib.add(Type::new_prim("i8"));
        core_lib.add(Type::new_prim("i16"));
        core_lib.add(Type::new_prim("i32"));
        core_lib.add(Type::new_prim("i64"));

        core_lib.add(Type::new_prim("bool"));

        // These are types that should not be used by the programmer. These include things like
        // goto statements and goto markers.
        core_lib.add(Type::new_under("goto"));
        core_lib.add(Type::new_under("gotomarker"));

        core_lib
    };
}
