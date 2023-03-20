use crate::{CompData, libraries::Library, ExternalFuncAdd, Func, unit::FuncCodeInfo, FuncType, Type};

pub struct UnitLib;

impl Library for UnitLib {
    fn add_to_unit(&self, unit: &mut crate::Unit) {
        unit.add_reflect_type::<FuncType>();
        unit.add_reflect_type::<FuncCodeInfo>();
        let func = unit.add_reflect_type::<Func>().unwrap();
        let comp_data = unit.add_reflect_type::<CompData>().unwrap();

        unit.add_rust_func_explicit(
            "get_fn_by_ptr", 
            CompData::get_fn_by_ptr as *const usize, 
            ExternalFuncAdd { 
                arg_types: vec![ Type::u(64) ], 
                ret_type: func,
                ..ExternalFuncAdd::empty() 
            }
            .method_of(comp_data.get_ref()),
       );
    }
}
