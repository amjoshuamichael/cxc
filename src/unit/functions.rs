use crate::Type;

impl<'a> CompData<'a> {
    pub fn new() -> Self {
        let mut out = Self {
            types: HashMap::new(),
            aliases: HashMap::new(),
            compiled: HashMap::new(),
            func_types: HashMap::new(),
            func_code: HashMap::new(),
        };

        out.insert_code(FuncCode {
            name: VarName::from("alloc"),
            ret_type: TypeAlias::Ref(box TypeAlias::GenParam(0)),
            args: Vec::new(),
            generic_count: 1,
            code: Expr::Block(Vec::new()),
            method_of: None,
            dependencies: Vec::new(),
        });

        out.insert_code(FuncCode {
            name: VarName::from("free"),
            ret_type: TypeAlias::Int(32),
            args: vec![VarDecl {
                name: VarName::temp(),
                typ: Some(TypeAlias::Ref(box TypeAlias::GenParam(0))),
            }],
            generic_count: 1,
            code: Expr::Block(Vec::new()),
            method_of: None,
            dependencies: Vec::new(),
        });

        out.insert_code(FuncCode {
            name: VarName::from("memmove"),
            ret_type: TypeAlias::Int(32),
            args: vec![
                VarDecl {
                    name: VarName::temp(),
                    typ: Some(TypeAlias::Ref(box TypeAlias::GenParam(0))),
                },
                VarDecl {
                    name: VarName::temp(),
                    typ: Some(TypeAlias::Ref(box TypeAlias::GenParam(0))),
                },
                VarDecl {
                    name: VarName::temp(),
                    typ: Some(TypeAlias::Int(64)),
                },
            ],
            generic_count: 1,
            code: Expr::Block(Vec::new()),
            method_of: None,
            dependencies: Vec::new(),
        });

        out.insert_code(FuncCode {
            name: VarName::from("size_of"),
            ret_type: TypeAlias::Int(64),
            args: Vec::new(),
            generic_count: 1,
            code: Expr::Block(Vec::new()),
            method_of: None,
            dependencies: Vec::new(),
        });

        out
    }

    pub fn insert_code(&mut self, code: FuncCode) {
        self.func_code.insert(code.decl_info(), code);
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
        let decl_info = unique_func_info.into();
        self.func_code.remove(&decl_info);
    }

    pub fn unique_func_info_iter(&self) -> impl Iterator<Item = &UniqueFuncInfo> {
        self.compiled.keys()
    }

    pub fn func_exists(&self, info: &FuncDeclInfo) -> bool {
        self.func_code.contains_key(info)
    }

    pub fn has_been_compiled(&self, info: &UniqueFuncInfo) -> bool {
        self.compiled.contains_key(info)
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

        let code = self.func_code.get(&info.clone().into())?;

        let ret_type = self.get_spec(&code.ret_type, &info.generics).unwrap();

        let arg_types = code
            .args
            .iter()
            .map(|d| {
                self.get_spec(d.typ.as_ref().unwrap(), &info.generics)
                    .unwrap()
            })
            .collect();

        let func_type = ret_type.func_with_args(arg_types);

        Some(func_type)
    }

    pub fn get_code(&self, info: UniqueFuncInfo) -> Option<FuncCode> {
        self.func_code.get(&info.into()).cloned()
    }

    pub fn is_extern(&self, info: &FuncDeclInfo) -> bool {
        self.func_code.contains_key(info)
    }
}

use std::hash::Hash;

use super::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FuncDeclInfo {
    pub name: VarName,
    pub method_of: Option<TypeName>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UniqueFuncInfo {
    pub name: VarName,
    pub method_of: Option<TypeName>,
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
        method_of: &Option<TypeName>,
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
}

impl Into<FuncDeclInfo> for UniqueFuncInfo {
    fn into(self) -> FuncDeclInfo {
        FuncDeclInfo {
            name: self.name,
            method_of: self.method_of,
        }
    }
}
