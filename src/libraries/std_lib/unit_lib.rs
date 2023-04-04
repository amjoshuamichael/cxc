use crate::{CompData, libraries::Library, ExternalFuncAdd, Func, unit::{FuncCodeInfo, Gen}, FuncType, Type, XcValue, XcReflect, IntType, FloatType, StructType, typ::{SumType, VariantType}, RefType, BoolType, ArrayType, TypeData, TypeEnum, VarName, TypeName, TypeRelation, UniqueFuncInfo};

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
                FuncCodeInfo::type_decl(),
                UniqueFuncInfo::type_decl(),
                TypeRelation::type_decl(),
                XcValue::type_decl(),
                Func::type_decl(),
                Gen::type_decl(),
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

        unit.add_rust_func_explicit(
            "get_fn_by_ptr", 
            CompData::get_fn_by_ptr as *const usize, 
            ExternalFuncAdd { 
                arg_types: vec![ Type::u(64) ], 
                ret_type: Type::new_tuple(vec![unique_func_info, func]),
                ..ExternalFuncAdd::empty() 
            }
            .method_of(comp_data.get_ref()),
       );
    }
}
