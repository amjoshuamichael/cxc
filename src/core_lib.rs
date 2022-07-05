use crate::hlr::prelude::*;

lazy_static! {
    pub static ref CORE_LIB: TypeGroup = {
        let mut core_lib = TypeGroup::default();

        // The primitive library is a library of types that make up the edges of all other types.
        core_lib.add(BaseType::new_prim("i32"));
        core_lib.add(BaseType::new_prim("f32"));

        core_lib.add(BaseType::new_prim("bool"));

        // The under library is a library of types that should not be used by the programmer.
        core_lib.add(BaseType::new_under("goto"));
        core_lib.add(BaseType::new_under("gotomarker"));

        // Used as a placeholder before type inference.
        core_lib.add(BaseType::new_under("none"));

        core_lib
    };
}
