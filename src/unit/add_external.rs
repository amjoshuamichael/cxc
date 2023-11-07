use std::{iter::once};

use indexmap::IndexMap;

use crate::{
    FuncType, Type, TypeEnum,
    TypeRelation, Unit, XcReflect, parse::{FuncCode, VarDecl, Expr}, VarName, typ::{spec_from_type::type_to_type_spec, ABI},
};

use super::{backends::IsBackend, ProcessedFuncInfo};

#[derive(Debug)]
pub struct ExternalFuncAdd {
    pub ret_type: Type,
    pub arg_types: Vec<Type>,
    pub abi: ABI,
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
            abi: ABI::Rust,
        }
    }

    pub fn method_of(mut self, typ: Type) -> Self {
        self.relation = TypeRelation::MethodOf(typ.clone());
        self.arg_types.insert(0, typ);
        self
    }
}

impl Unit {
    pub fn add_rust_func<A: XcReflect, R: XcReflect>(
        &mut self, 
        name: &str, 
        function: [fn(A) -> R; 1],
    ) {
        let func_type = self.comp_data.type_of_val(&function[0]);
        let TypeEnum::Func(FuncType { args, ret: ret_type, abi }) = 
            func_type.as_type_enum() else { panic!() };

        let ext_add = ExternalFuncAdd {
            arg_types: args.clone(),
            ret_type: ret_type.clone(),
            abi: *abi,
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
            abi: ext_add.abi,
        };

        let func_code_id = self.comp_data.func_code.insert(FuncCode {
            name: name.into(),
            relation: ext_add.relation.clone().map_inner_type(type_to_type_spec),
            args: func_type.args.iter().map(|a| VarDecl {
                name: VarName::None,
                type_spec: type_to_type_spec(a.clone()),
            }).collect(),
            ret_type: type_to_type_spec(func_type.ret.clone()),
            generic_count: ext_add.generics.len(),
            is_external: true,
            abi: ABI::Rust,
            ..FuncCode::empty()
        });

        let func_id = self.comp_data.processed.insert(ProcessedFuncInfo {
            specified_dependencies: IndexMap::new(),
            name: name.into(),
            relation: ext_add.relation,
            generics: ext_add.generics,
            typ: func_type.clone(),
        });

        self.comp_data.realizations.insert(func_code_id, once(func_id).collect());

        self.backend.add_external_func(
            func_id, 
            func_type,
            &self.comp_data.processed[func_id],
            function_ptr, 
        );
    }
}
