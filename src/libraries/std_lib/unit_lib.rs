use crate::{CompData, libraries::Library, Func, unit::backends::function::FuncCodePtr, FuncType, Type, Value, XcReflect, IntType, FloatType, StructType, typ::{SumType, VariantType}, RefType, BoolType, ArrayType, TypeData, TypeEnum, VarName, TypeName, TypeRelation, UniqueFuncInfo};

pub struct UnitLib;

impl Library for UnitLib {
    fn add_to_unit(&self, unit: &mut crate::Unit) {
        unit.add_many_reflect_types(
            &[
                Type::type_decl(),
                TypeData::type_decl(),
                TypeEnum::type_decl(),
                VarName::type_decl(),
                TypeName::type_decl(),
                IntType::type_decl(),
                FloatType::type_decl(),
                StructType::type_decl(),
                SumType::type_decl(),
                VariantType::type_decl(),
                RefType::type_decl(),
                FuncType::type_decl(),
                ArrayType::type_decl(),
                BoolType::type_decl(),
                FuncCodePtr::type_decl(),
                UniqueFuncInfo::type_decl(),
                TypeRelation::type_decl(),
                Value::type_decl(),
                Func::type_decl(),
                VarName::type_decl(),
            ]
        );

        unit.assert_size_of::<VarName>().unwrap();
        unit.assert_size_of_with_name::<TypeRelation>("TypeRelation").unwrap();
        unit.assert_size_of::<UniqueFuncInfo>().unwrap();
        unit.assert_size_of::<Func>().unwrap();

        let func = unit.get_reflect_type::<Func>().unwrap();

        let unique_func_info = unit.get_reflect_type::<UniqueFuncInfo>().unwrap();
        let comp_data = unit.add_reflect_type::<CompData>().unwrap();
    }
}
