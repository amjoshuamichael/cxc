use std::rc::Rc;

use inkwell::values::BasicMetadataValueEnum;

use crate::to_llvm::add_sret_attribute_to_call_site;
use crate::{
    to_llvm::add_sret_attribute_to_func, typ::ReturnStyle, FuncType, Kind, Type, TypeEnum,
    TypeRelation, UniqueFuncInfo, Unit, XcReflect,
};

use super::functions::Func;

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
            ret_type: Type::void(),
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
        let typ = self.get_reflect_type::<T>().unwrap();
        self.add_rust_func_explicit(
            "to_string",
            T::to_string as *const usize,
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
    ) {
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
        let ret_type = func_type_inner.ret.clone();

        let ink_func_ptr_type = func_type.to_any_type(self.context).into_pointer_type();

        let ink_func_type = func_type_inner.llvm_func_type(self.context);

        // TODO: Check if function already exists, and update gen if nescssary
        let func_info = UniqueFuncInfo {
            name: name.into(),
            relation: ext_add.relation.clone(),
            generics: ext_add.generics,
            ..Default::default()
        };

        let mut function =
            self.module
                .add_function(&func_info.to_string(), ink_func_type, None);

        add_sret_attribute_to_func(&mut function, self.context, &ret_type);

        let builder = self.context.create_builder();

        {
            let block = self.context.append_basic_block(function, "link");
            builder.position_at_end(block);
        }

        let llvm_func_ptr = self
            .context
            .i64_type()
            .const_int(function_ptr as u64, false)
            .const_to_pointer(ink_func_ptr_type);

        let arg_vals: Vec<BasicMetadataValueEnum> =
            function.get_params().iter().map(|p| (*p).into()).collect();

        if ret_type.return_style() == ReturnStyle::Sret {
            let mut out =
                builder.build_indirect_call(ink_func_type, llvm_func_ptr, &arg_vals, "call");
            add_sret_attribute_to_call_site(&mut out, self.context, &ret_type);
            builder.build_return(None);
        } else if ret_type.is_void() {
            builder.build_indirect_call(ink_func_type, llvm_func_ptr, &arg_vals, "call");
            builder.build_return(None);
        } else {
            let out =
                builder.build_indirect_call(ink_func_type, llvm_func_ptr, &arg_vals, "call");
            let out = out.try_as_basic_value().unwrap_left();
            builder.build_return(Some(&out));
        }

        let comp_data = Rc::get_mut(&mut self.comp_data).unwrap();

        comp_data.compiled.insert(
            func_info.clone(),
            Func::new_external(
                func_info,
                func_type_inner.clone(),
                function_ptr,
                ext_add.type_mask.clone(),
            ),
        );
    }
}
