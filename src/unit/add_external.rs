use std::{mem::transmute, rc::Rc};

use inkwell::values::{BasicMetadataValueEnum, CallableValue};

use crate::{
    typ::ReturnStyle, FuncType, Kind, Type, TypeEnum, TypeRelation, UniqueFuncInfo,
    Unit,
};

pub struct ExternalFuncAdd {
    pub ret_type: Type,
    pub arg_types: Vec<Type>,
    pub relation: TypeRelation,
    pub generics: Vec<Type>,
    pub type_mask: Vec<bool>,
}

impl ExternalFuncAdd {
    pub fn empty() -> Self {
        ExternalFuncAdd {
            ret_type: Type::never(), // TODO: void type
            arg_types: Vec::new(),
            relation: TypeRelation::Unrelated,
            generics: Vec::new(),
            type_mask: Vec::new(),
        }
    }

    pub fn reflect_variable_types(mut self) -> Self {
        self.type_mask = vec![true; self.arg_types.len()];
        self
    }
}

impl<'u> Unit<'u> {
    pub fn add_rust_func<A, R>(
        &mut self,
        name: &str,
        function: [fn(A) -> R; 1],
    ) -> &mut Self {
        let func_type = self.comp_data.type_of(&function[0]);
        let TypeEnum::Func(FuncType { args, ret_type }) = 
            func_type.as_type_enum() else { panic!() };

        let ext_add = ExternalFuncAdd {
            arg_types: args.clone(),
            ret_type: ret_type.clone(),
            ..ExternalFuncAdd::empty()
        };

        let function_ptr: *const usize = unsafe { transmute(function[0]) };

        self.add_rust_func_explicit(name, function_ptr, ext_add)
    }

    pub fn add_external_default<T: Default>(&mut self, typ: Type) {
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

    pub fn add_external_clone<T: Clone>(&mut self, typ: Type) {
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

    pub fn add_rust_func_explicit(
        &mut self,
        name: &str,
        function_ptr: *const usize,
        mut ext_add: ExternalFuncAdd,
    ) -> &mut Self {
        {
            let mut added_so_far = 0;
            for (index, should_reflect) in ext_add.type_mask.iter().enumerate() {
                if *should_reflect {
                    ext_add.arg_types.insert(index + added_so_far, Type::i(64));
                    added_so_far += 1;
                }
            }
        }

        let func_type = ext_add
            .ret_type
            .clone()
            .func_with_args(ext_add.arg_types.clone());
        let TypeEnum::Func(func_type_inner) = 
            func_type.as_type_enum() else { panic!() };
        let ret_type = func_type_inner.ret_type();

        let ink_func_ptr_type =
            func_type.to_any_type(&self.context).into_pointer_type();

        let ink_func_type = func_type_inner.llvm_func_type(&self.context);

        let func_info = UniqueFuncInfo {
            name: name.into(),
            relation: ext_add.relation,
            generics: ext_add.generics,
        };

        let function =
            self.module
                .add_function(&*func_info.to_string(), ink_func_type, None);

        let builder = self.context.create_builder();

        {
            let block = self.context.append_basic_block(function, "link");
            builder.position_at_end(block);
        }

        let callable = {
            let function_pointer = self
                .context
                .i64_type()
                .const_int(function_ptr as u64, false)
                .const_to_pointer(ink_func_ptr_type);

            CallableValue::try_from(function_pointer).unwrap()
        };

        if ret_type.return_style() != ReturnStyle::Pointer {
            let arg_vals: Vec<BasicMetadataValueEnum> = function
                .get_params()
                .iter()
                .map(|p| (*p).try_into().unwrap())
                .collect();
            let out = builder.build_call(callable, &*arg_vals, "call");
            let out = out.try_as_basic_value().unwrap_left();
            builder.build_return(Some(&out));
        } else {
            let arg_vals: Vec<BasicMetadataValueEnum> = function
                .get_params()
                .iter()
                .map(|p| (*p).try_into().unwrap())
                .collect();
            builder.build_call(callable, &*arg_vals, "call");
            builder.build_return(None);
        }

        let function = self.module.get_function(&*func_info.to_string()).unwrap();
        let comp_data = Rc::get_mut(&mut self.comp_data).unwrap();

        comp_data.compiled.insert(func_info.clone(), function);
        comp_data.func_types.insert(func_info.clone(), func_type);

        if ext_add.type_mask.len() > 0 {
            comp_data.reflect_arg_types(&func_info, ext_add.type_mask.clone());
        }

        self
    }
}
