use std::collections::HashMap;

use crate::{
    FuncType, Type, TypeEnum,
    TypeRelation, FuncQuery, Unit, XcReflect,
};

use super::{backends::IsBackend, ProcessedFuncInfo};

pub struct ExternalFuncAdd {
    pub ret_type: Type,
    pub arg_types: Vec<Type>,
    pub relation: TypeRelation,
    pub generics: Vec<Type>,
}

impl ExternalFuncAdd {
    pub fn empty() -> Self {
        ExternalFuncAdd {
            ret_type: Type::void(),
            arg_types: Vec::new(),
            relation: TypeRelation::Unrelated,
            generics: Vec::new(),
        }
    }

    pub fn method_of(mut self, typ: Type) -> Self {
        self.relation = TypeRelation::MethodOf(typ.clone());
        self.arg_types.insert(0, typ);
        self
    }
}

impl Unit {
    pub fn add_rust_func<A, R>(&mut self, name: &str, function: [fn(A) -> R; 1]) {
        let func_type = self.comp_data.type_of(&function[0]);
        let TypeEnum::Func(FuncType { args, ret: ret_type }) = 
            func_type.as_type_enum() else { panic!() };

        let ext_add = ExternalFuncAdd {
            arg_types: args.clone(),
            ret_type: ret_type.clone(),
            ..ExternalFuncAdd::empty()
        };

        let function_ptr: *const usize = function[0] as *const usize;

        self.add_rust_func_explicit(name, function_ptr, ext_add)
    }

    pub fn add_external_default<T: Default + XcReflect>(&mut self) {
        let typ = self.get_reflect_type::<T>().unwrap();
        self.add_rust_func_explicit(
            "default",
            T::default as *const usize,
            ExternalFuncAdd {
                ret_type: typ.clone(),
                relation: TypeRelation::Static(typ),
                ..ExternalFuncAdd::empty()
            },
        );
    }

    pub fn add_external_clone<T: Clone + XcReflect>(&mut self) {
        let typ = self.get_reflect_type::<T>().unwrap();
        self.add_rust_func_explicit(
            "clone",
            T::clone as *const usize,
            ExternalFuncAdd {
                arg_types: vec![typ.get_ref()],
                ret_type: typ.clone(),
                relation: TypeRelation::MethodOf(typ.get_ref()),
                ..ExternalFuncAdd::empty()
            },
        );
    }

    pub fn add_external_to_string<T: ToString + XcReflect>(&mut self) {
        let string_type = self.comp_data.get_by_name(&"String".into()).unwrap();
        let typ = self.get_reflect_type::<T>().unwrap();

        self.add_rust_func_explicit(
            "to_string",
            T::to_string as *const usize,
            ExternalFuncAdd {
                ret_type: string_type,
                ..ExternalFuncAdd::empty()
            }
            .method_of(typ.get_ref()),
        );
    }

    pub fn add_rust_func_explicit(
        &mut self,
        name: &str,
        function_ptr: *const usize,
        ext_add: ExternalFuncAdd,
    ) {
        let func_type = FuncType {
            ret: ext_add.ret_type,
            args: ext_add.arg_types,
        };

        let func_id = self.comp_data.processed.insert(ProcessedFuncInfo {
            dependencies: HashMap::new(),
            name: name.into(),
            relation: ext_add.relation,
            generics: ext_add.generics,
            typ: func_type.clone(),
        });

        self.comp_data.generations.update(func_id);

        self.backend.add_external_func(
            func_id, 
            func_type,
            function_ptr, 
        );
    }
}
