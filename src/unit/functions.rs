use crate::{typ::TypeOrAlias, Type};

pub type DeriverFunc = fn(&CompData, Type) -> Option<FuncCode>;

impl<'a> CompData<'a> {
    pub fn new() -> Self {
        let mut out = Self::default();

        out.insert_intrinsic(FuncCode {
            name: VarName::from("alloc"),
            ret_type: TypeAlias::Ref(box TypeAlias::GenParam(0)),
            args: Vec::new(),
            generic_count: 1,
            code: Expr::Block(Vec::new()),
            method_of: None,
        });

        out.insert_intrinsic(FuncCode {
            name: VarName::from("free"),
            ret_type: TypeAlias::Int(32),
            args: vec![VarDecl {
                name: VarName::temp(),
                typ: Some(TypeAlias::Ref(box TypeAlias::GenParam(0)).into()),
            }],
            generic_count: 1,
            code: Expr::Block(Vec::new()),
            method_of: None,
        });

        out.insert_intrinsic(FuncCode {
            name: VarName::from("memmove"),
            ret_type: TypeAlias::Int(32),
            args: vec![
                VarDecl {
                    name: VarName::temp(),
                    typ: Some(TypeAlias::Ref(box TypeAlias::GenParam(0)).into()),
                },
                VarDecl {
                    name: VarName::temp(),
                    typ: Some(TypeAlias::Ref(box TypeAlias::GenParam(0)).into()),
                },
                VarDecl {
                    name: VarName::temp(),
                    typ: Some(Type::i(64).into()),
                },
            ],
            generic_count: 1,
            code: Expr::Block(Vec::new()),
            method_of: None,
        });

        out.insert_intrinsic(FuncCode {
            name: VarName::from("memcpy"),
            ret_type: TypeAlias::Int(32),
            args: vec![
                VarDecl {
                    name: VarName::temp(),
                    typ: Some(TypeAlias::Ref(box TypeAlias::GenParam(0)).into()),
                },
                VarDecl {
                    name: VarName::temp(),
                    typ: Some(TypeAlias::Ref(box TypeAlias::GenParam(0)).into()),
                },
                VarDecl {
                    name: VarName::temp(),
                    typ: Some(Type::i(64).into()),
                },
            ],
            generic_count: 1,
            code: Expr::Block(Vec::new()),
            method_of: None,
        });

        out.insert_intrinsic(FuncCode {
            name: VarName::from("size_of"),
            ret_type: TypeAlias::Int(64),
            args: Vec::new(),
            generic_count: 1,
            code: Expr::Block(Vec::new()),
            method_of: None,
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
        let method_of = decl_info
            .method_of
            .clone()
            .map(|toa| toa.into_type_with_generics(self, &generics).unwrap());

        let unique_func_info = UniqueFuncInfo {
            name: decl_info.name.clone(),
            method_of,
            generics,
        };

        unique_func_info
    }

    pub fn insert_temp_function(&mut self, code: FuncCode) -> UniqueFuncInfo {
        let name = VarName::from("T_temp");
        let decl_info = FuncDeclInfo {
            name: name.clone(),
            method_of: None,
        };
        self.func_code.insert(decl_info, code);

        UniqueFuncInfo {
            name,
            method_of: None,
            generics: Vec::new(),
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

    pub fn func_exists(&self, info: &FuncDeclInfo) -> bool {
        self.func_code.contains_key(info)
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
            let decl_method_of = decl.method_of.clone().map(|toa| {
                toa.into_type_with_generics(self, &info.generics).unwrap()
            });

            if decl_method_of == info.method_of {
                return Some(decl.clone());
            }
        }

        None
    }

    pub fn get_func_value(
        &'a self,
        info: &UniqueFuncInfo,
    ) -> Option<FunctionValue<'a>> {
        self.compiled.get(info).cloned()
    }

    pub fn create_func_placeholder(
        &mut self,
        info: &UniqueFuncInfo,
        context: &'a Context,
        module: &Module<'a>,
    ) {
        let function_type = self.get_type(info).unwrap();

        let empty_function = module.add_function(
            &*info.to_string(),
            function_type.to_any_type(context).into_function_type(),
            None,
        );

        self.compiled.insert(info.clone(), empty_function.into());
        self.func_types.insert(info.clone(), function_type);
    }

    pub fn get_type(&self, info: &UniqueFuncInfo) -> Option<Type> {
        let cached_type = self.func_types.get(info);
        if cached_type.is_some() {
            return cached_type.cloned();
        }

        let code = self.get_code(info.clone())?;

        let ret_type = self.get_spec(&code.ret_type, &info.generics).unwrap();

        let arg_types = code
            .args
            .iter()
            .map(|d| {
                d.typ
                    .clone()
                    .unwrap()
                    .into_type_with_generics(self, &info.generics)
                    .unwrap()
            })
            .collect();

        let func_type = ret_type.func_with_args(arg_types);

        Some(func_type)
    }

    pub fn get_derived_code(&self, info: &UniqueFuncInfo) -> Option<FuncCode> {
        let deriver = self.derivers.get(&info.name)?;
        deriver(self, info.method_of.as_ref()?.clone())
    }

    pub fn get_code(&self, info: UniqueFuncInfo) -> Option<FuncCode> {
        let decl_info = self.get_declaration_of(&info);
        if let Some(decl_info) = decl_info {
            return self.func_code.get(&decl_info).cloned();
        };

        self.get_derived_code(&info)
    }

    pub fn add_deriver(&mut self, func_name: VarName, func: DeriverFunc) {
        self.derivers.insert(func_name, func);
    }
}

use std::hash::Hash;

use super::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FuncDeclInfo {
    pub name: VarName,
    pub method_of: Option<TypeOrAlias>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UniqueFuncInfo {
    pub name: VarName,
    pub method_of: Option<Type>,
    pub generics: Vec<Type>,
}

impl ToString for UniqueFuncInfo {
    fn to_string(&self) -> String {
        match &self.method_of {
            Some(typ) => format!("M_{:?}{:?}{:?}", typ, self.name, self.generics),
            None => format!("{:?}{:?}", self.name, self.generics),
        }
        .chars()
        .filter(|c| c.is_alphanumeric() || *c == '_')
        .collect()
    }
}

impl UniqueFuncInfo {
    pub fn new(
        og_name: &VarName,
        method_of: &Option<Type>,
        generics: Vec<Type>,
    ) -> UniqueFuncInfo {
        UniqueFuncInfo {
            name: og_name.clone(),
            method_of: method_of.clone(),
            generics,
        }
    }

    pub fn og_name(&self) -> VarName { self.name.clone() }
    pub fn is_method(&self) -> bool { self.method_of.is_some() }
    pub fn has_generics(&self) -> bool { self.generics.len() > 0 }
}
