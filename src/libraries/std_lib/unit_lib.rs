use crate::{libraries::Library, Func, unit::backends::function::FuncCodePtr, FuncType, Type, Value, XcReflect, IntType, FloatType, StructType, RefType, BoolType, ArrayType, typ::TypeData, VarName, TypeName};

pub struct UnitLib;

impl Library for UnitLib {
    fn add_to_unit(&self, unit: &mut crate::Unit) {
        // TODO: redo this
        unit.add_many_reflect_types(
            &[
                Type::type_decl(),
                TypeData::type_decl(),
                VarName::type_decl(),
                TypeName::type_decl(),
                IntType::type_decl(),
                StructType::type_decl(),
                RefType::type_decl(),
                FuncType::type_decl(),
                ArrayType::type_decl(),
                BoolType::type_decl(),
                Value::type_decl(),
                Func::type_decl(),
                VarName::type_decl(),
            ]
        );

        #[cfg(feature = "ffi-assertions")]
        {
            unit.assert_size_of::<VarName>().unwrap();
            //unit.assert_size_of_with_name::<TypeRelation>("TypeRelation").unwrap();
            //unit.assert_size_of::<UniqueFuncInfo>().unwrap();
            unit.assert_size_of::<Func>().unwrap();
        }
    }
}
