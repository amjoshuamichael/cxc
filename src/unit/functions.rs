use crate::{FuncType, Type, TypeRelation};

pub type DeriverFunc = fn(&CompData, Type) -> Option<FuncCode>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DeriverInfo {
    func_name: VarName,
    is_static: bool,
}

impl TryFrom<UniqueFuncInfo> for DeriverInfo {
    type Error = ();

    fn try_from(info: UniqueFuncInfo) -> Result<Self, ()> {
        let is_static = match info.relation {
            TypeRelation::MethodOf(_) => false,
            TypeRelation::Static(_) => true,
            TypeRelation::Unrelated => return Err(()),
        };

        Ok(DeriverInfo {
            func_name: info.name,
            is_static,
        })
    }
}

impl<'a> CompData<'a> {
    pub fn new() -> Self {
        let mut out = Self::default();

        out.insert_intrinsic(FuncCode {
            name: VarName::from("alloc"),
            ret_type: TypeSpec::Ref(box TypeSpec::GenParam(0)),
            args: Vec::new(),
            generic_count: 1,
            code: Expr::Block(Vec::new()),
            relation: TypeSpecRelation::Unrelated,
        });

        out.insert_intrinsic(FuncCode {
            name: VarName::from("free"),
            ret_type: Type::i(32).into(),
            args: vec![VarDecl {
                name: VarName::temp(),
                type_spec: Some(TypeSpec::Ref(box TypeSpec::GenParam(0)).into()),
            }],
            generic_count: 1,
            code: Expr::Block(Vec::new()),
            relation: TypeSpecRelation::Unrelated,
        });

        out.insert_intrinsic(FuncCode {
            name: VarName::from("memmove"),
            ret_type: Type::i(32).into(),
            args: vec![
                VarDecl {
                    name: VarName::temp(),
                    type_spec: Some(TypeSpec::Ref(box TypeSpec::GenParam(0)).into()),
                },
                VarDecl {
                    name: VarName::temp(),
                    type_spec: Some(TypeSpec::Ref(box TypeSpec::GenParam(0)).into()),
                },
                VarDecl {
                    name: VarName::temp(),
                    type_spec: Some(Type::i(64).into()),
                },
            ],
            generic_count: 1,
            code: Expr::Block(Vec::new()),
            relation: TypeSpecRelation::Unrelated,
        });

        out.insert_intrinsic(FuncCode {
            name: VarName::from("memcpy"),
            ret_type: Type::i(32).into(),
            args: vec![
                VarDecl {
                    name: VarName::temp(),
                    type_spec: Some(TypeSpec::Ref(box TypeSpec::GenParam(0)).into()),
                },
                VarDecl {
                    name: VarName::temp(),
                    type_spec: Some(TypeSpec::Ref(box TypeSpec::GenParam(0)).into()),
                },
                VarDecl {
                    name: VarName::temp(),
                    type_spec: Some(Type::i(64).into()),
                },
            ],
            generic_count: 1,
            code: Expr::Block(Vec::new()),
            relation: TypeSpecRelation::Unrelated,
        });

        out.insert_intrinsic(FuncCode {
            name: VarName::from("size_of"),
            ret_type: Type::i(64).into(),
            args: Vec::new(),
            generic_count: 1,
            code: Expr::Block(Vec::new()),
            relation: TypeSpecRelation::Unrelated,
        });

        out.insert_intrinsic(FuncCode {
            name: VarName::from("write"),
            ret_type: Type::i(32).into(),
            args: vec![
                VarDecl {
                    name: VarName::temp(),
                    type_spec: Some(TypeSpec::GenParam(0).get_ref().get_ref().into()),
                },
                VarDecl {
                    name: VarName::temp(),
                    type_spec: Some(TypeSpec::GenParam(0).get_ref().into()),
                },
            ],
            generic_count: 1,
            code: Expr::Block(Vec::new()),
            relation: TypeSpecRelation::MethodOf(
                TypeSpec::GenParam(0).get_ref().get_ref().into(),
            ),
        });

        out
    }

    fn insert_intrinsic(&mut self, code: FuncCode) {
        let decl_info = self.insert_code(code);
        self.intrinsics.insert(decl_info);
    }

    pub fn insert_code(&mut self, code: FuncCode) -> FuncDeclInfo {
        let decl_info = code.decl_info();
        self.func_code.insert(decl_info.clone(), code);
        self.decl_names
            .entry(decl_info.name.clone())
            .and_modify(|set| set.push(decl_info.clone()))
            .or_insert(vec![decl_info.clone()]);

        decl_info
    }

    pub fn to_unique_func_info(
        &mut self,
        decl_info: FuncDeclInfo,
        generics: Vec<Type>,
    ) -> UniqueFuncInfo {
        let relation = decl_info
            .relation
            .clone()
            .map(|spec| self.get_spec(&spec, &generics).unwrap());

        let unique_func_info = UniqueFuncInfo {
            name: decl_info.name.clone(),
            relation,
            generics,
        };

        unique_func_info
    }

    pub fn insert_temp_function(&mut self, code: FuncCode) -> UniqueFuncInfo {
        let name = VarName::from("T_temp");
        let decl_info = FuncDeclInfo {
            name: name.clone(),
            ..Default::default()
        };

        self.func_code.insert(decl_info, code);

        UniqueFuncInfo {
            name,
            ..Default::default()
        }
    }

    pub fn remove_function(&mut self, unique_func_info: UniqueFuncInfo) {
        self.compiled.remove(&unique_func_info);
        self.func_types.remove(&unique_func_info);
        let decl_info = self.get_declaration_of(&unique_func_info).unwrap();
        self.func_code.remove(&decl_info);
    }

    pub fn unique_func_info_iter(&self) -> impl Iterator<Item = &UniqueFuncInfo> {
        self.compiled.keys()
    }

    pub fn func_exists(&self, info: &FuncDeclInfo) -> bool { self.func_code.contains_key(info) }

    pub fn reflect_arg_types(&mut self, info: &UniqueFuncInfo, mask: Vec<bool>) {
        assert!({
            let func_type = self.get_type(info).unwrap();
            let TypeEnum::Func(FuncType { args, .. }) = 
                func_type.as_type_enum() else { panic!() };

            let arg_count = mask.iter().filter(|refl| **refl).count() + mask.len();
            args.len() == arg_count
        });

        self.reflect_arg_types.insert(info.clone(), mask);
    }

    pub fn get_reflect_type_masks(&self, info: &UniqueFuncInfo) -> Vec<bool> {
        self.reflect_arg_types
            .get(info)
            .cloned()
            .unwrap_or_default()
    }

    pub fn has_been_compiled(&self, info: &UniqueFuncInfo) -> bool {
        let is_intrinsic = if let Some(decl) = self.get_declaration_of(info) {
            self.intrinsics.contains(&decl)
        } else {
            false
        };
        is_intrinsic || self.compiled.contains_key(info)
    }

    pub fn get_declaration_of(&self, info: &UniqueFuncInfo) -> Option<FuncDeclInfo> {
        let possible_decls = self.decl_names.get(&info.name)?;

        for decl in possible_decls {
            let decl_method_of = decl
                .relation
                .clone()
                .map(|spec| self.get_spec(&spec, &info.generics).unwrap());

            if decl_method_of == info.relation {
                return Some(decl.clone());
            }
        }

        None
    }

    pub fn get_func_value(&'a self, info: &UniqueFuncInfo) -> Option<FunctionValue<'a>> {
        self.compiled.get(info).cloned()
    }

    pub fn create_func_placeholder(
        &mut self,
        info: &UniqueFuncInfo,
        context: &'a Context,
        module: &Module<'a>,
    ) {
        let function_type = self.get_type(info).unwrap();
        let TypeEnum::Func(llvm_function_type) = function_type.as_type_enum()
            else { panic!() };

        let empty_function = module.add_function(
            &*info.to_string(),
            llvm_function_type.llvm_func_type(context),
            None,
        );

        self.compiled.insert(info.clone(), empty_function);
        self.func_types.insert(info.clone(), function_type.clone());
        self.globals
            .insert(info.name.clone(), (function_type, empty_function.as_global_value()));
    }

    pub fn get_type(&self, info: &UniqueFuncInfo) -> Option<Type> {
        let cached_type = self.func_types.get(info);
        if cached_type.is_some() {
            return cached_type.cloned();
        }

        let code = self.get_code(info.clone())?;

        let ret_type = &self.get_spec(&code.ret_type, &info.generics)?;

        let arg_types = code
            .args
            .iter()
            .map(|d| {
                self.get_spec(d.type_spec.as_ref().unwrap(), &info.generics)
                    .unwrap()
            })
            .collect();

        let func_type = ret_type.clone().func_with_args(arg_types);

        Some(func_type)
    }

    pub fn get_derived_code(&self, info: &UniqueFuncInfo) -> Option<FuncCode> {
        let deriver_info = DeriverInfo::try_from(info.clone()).unwrap();
        let deriver = self.derivers.get(&deriver_info)?;
        deriver(self, info.relation.inner()?)
    }

    pub fn get_code(&self, info: UniqueFuncInfo) -> Option<FuncCode> {
        let decl_info = self.get_declaration_of(&info);
        if let Some(decl_info) = decl_info {
            return self.func_code.get(&decl_info).cloned();
        };

        self.get_derived_code(&info)
    }

    pub fn add_method_deriver(&mut self, func_name: VarName, func: DeriverFunc) {
        self.derivers.insert(
            DeriverInfo {
                func_name,
                is_static: false,
            },
            func,
        );
    }

    pub fn add_static_deriver(&mut self, func_name: VarName, func: DeriverFunc) {
        self.derivers.insert(
            DeriverInfo {
                func_name,
                is_static: true,
            },
            func,
        );
    }
}

use std::hash::Hash;

use super::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct FuncDeclInfo {
    pub name: VarName,
    pub relation: TypeSpecRelation,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct UniqueFuncInfo {
    pub name: VarName,
    pub relation: TypeRelation,
    pub generics: Vec<Type>,
}

impl ToString for UniqueFuncInfo {
    fn to_string(&self) -> String {
        match &self.relation {
            TypeRelation::Static(typ) => {
                format!("{:?}::{:?}{:?}", typ, self.name, self.generics)
            },
            TypeRelation::MethodOf(typ) => {
                format!("M_{:?}{:?}{:?}", typ, self.name, self.generics)
            },
            TypeRelation::Unrelated => format!("{:?}{:?}", self.name, self.generics),
        }
        .chars()
        .filter(|c| c.is_alphanumeric() || *c == '_')
        .collect()
    }
}

impl UniqueFuncInfo {
    pub fn new(
        og_name: &VarName,
        relation: &TypeRelation,
        generics: Vec<Type>,
    ) -> UniqueFuncInfo {
        UniqueFuncInfo {
            name: og_name.clone(),
            relation: relation.clone(),
            generics,
        }
    }

    pub fn og_name(&self) -> VarName { self.name.clone() }
    pub fn is_method(&self) -> bool { matches!(self.relation, TypeRelation::MethodOf(_)) }
    pub fn is_static(&self) -> bool { matches!(self.relation, TypeRelation::Static(_)) }
    pub fn has_generics(&self) -> bool { self.generics.len() > 0 }
}
