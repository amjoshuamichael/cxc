use crate::hlr::prelude::*;

lazy_static! {
    pub static ref CORE_LIB: TypeGroup = {
        let mut core_lib = TypeGroup::default();

        // The primitive library is a library of types that make up the edges of all other types.
        core_lib.add(
            Type::new_prim("u8")
                .with_impl(Impl::FromIntLiteral { min: u8::MIN.into(), max: u8::MAX.into() })
                .with_impl(Impl::AddToIntLiteral)
        );
        core_lib.add(
            Type::new_prim("u16")
                .with_impl(Impl::FromIntLiteral { min: u16::MIN.into(), max: u16::MAX.into() })
                .with_impl(Impl::AddToIntLiteral)
        );
        core_lib.add(
            Type::new_prim("u32")
                .with_impl(Impl::FromIntLiteral { min: u32::MIN.into(), max: u32::MAX.into() })
                .with_impl(Impl::AddToIntLiteral)
        );
        core_lib.add(
            Type::new_prim("u64")
                .with_impl(Impl::FromIntLiteral { min: u64::MIN.into(), max: u64::MAX.into() })
                .with_impl(Impl::AddToIntLiteral)
        );

        core_lib.add(
            Type::new_prim("i8")
                .with_impl(Impl::FromIntLiteral { min: i8::MIN.into(), max: i8::MAX.into() })
                .with_impl(Impl::AddToIntLiteral)
        );
        core_lib.add(
            Type::new_prim("i16")
                .with_impl(Impl::FromIntLiteral { min: i16::MIN.into(), max: i16::MAX.into() })
                .with_impl(Impl::AddToIntLiteral)
        );
        core_lib.add(
            Type::new_prim("i32")
                .with_impl(Impl::FromIntLiteral { min: i32::MIN.into(), max: i32::MAX.into() })
                .with_impl(Impl::AddToIntLiteral)
        );
        core_lib.add(
            Type::new_prim("i64")
                .with_impl(Impl::FromIntLiteral { min: i64::MIN.into(), max: i64::MAX.into() })
                .with_impl(Impl::AddToIntLiteral)
        );

        core_lib.add(Type::new_prim("bool"));

        // The under library is a library of types that should not be used by the programmer.
        core_lib.add(Type::new_under("goto"));
        core_lib.add(Type::new_under("gotomarker"));

        // Used as a placeholder before type inference.
        core_lib.add(Type::new_under("none"));

        core_lib
    };
}
