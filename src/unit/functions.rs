use crate::{
    errors::{CResult, FErr, TErr, TResult},
    FuncType, Type, TypeRelation,
};

pub type DeriverFunc = fn(&CompData, Type) -> Option<FuncCode>;
pub type TypeLevelFunc = fn(Vec<Type>, &CompData) -> Type;

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct DeriverInfo {
    pub func_name: VarName,
    pub is_static: bool,
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

impl CompData {
    pub fn new() -> Self {
        let mut out = Self::default();

        out.insert_intrinsic(FuncCode {
            name: VarName::from("alloc"),
            args: vec![VarDecl {
                name: VarName::temp(),
                type_spec: TypeSpec::Int(32),
            }],
            ret_type: TypeSpec::Ref(box TypeSpec::GenParam(0)),
            generic_count: 1,
            code: Expr::Block(Vec::new()),
            relation: TypeSpecRelation::Unrelated,
        });

        out.insert_intrinsic(FuncCode {
            name: VarName::from("free"),
            ret_type: TypeSpec::Void,
            args: vec![VarDecl {
                name: VarName::temp(),
                type_spec: TypeSpec::Ref(box TypeSpec::GenParam(0)).into(),
            }],

            generic_count: 1,
            code: Expr::Block(Vec::new()),
            relation: TypeSpecRelation::Unrelated,
        });

        out.insert_intrinsic(FuncCode {
            name: VarName::from("memmove"),
            ret_type: TypeSpec::Void,
            args: vec![
                VarDecl {
                    name: VarName::temp(),
                    type_spec: TypeSpec::Ref(box TypeSpec::GenParam(0)).into(),
                },
                VarDecl {
                    name: VarName::temp(),
                    type_spec: TypeSpec::Ref(box TypeSpec::GenParam(0)).into(),
                },
                VarDecl {
                    name: VarName::temp(),
                    type_spec: Type::i(64).into(),
                },
            ],

            generic_count: 1,
            code: Expr::Block(Vec::new()),
            relation: TypeSpecRelation::Unrelated,
        });

        out.insert_intrinsic(FuncCode {
            name: VarName::from("memcpy"),
            ret_type: TypeSpec::Void,
            args: vec![
                VarDecl {
                    name: VarName::temp(),
                    type_spec: TypeSpec::Ref(box TypeSpec::GenParam(0)).into(),
                },
                VarDecl {
                    name: VarName::temp(),
                    type_spec: TypeSpec::Ref(box TypeSpec::GenParam(0)).into(),
                },
                VarDecl {
                    name: VarName::temp(),
                    type_spec: Type::i(64).into(),
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
            name: VarName::from("alignment_of"),
            ret_type: Type::i(64).into(),
            args: Vec::new(),

            generic_count: 1,
            code: Expr::Block(Vec::new()),
            relation: TypeSpecRelation::Unrelated,
        });

        out.insert_intrinsic(FuncCode {
            name: VarName::from("write"),
            ret_type: TypeSpec::Void,
            args: vec![
                VarDecl {
                    name: VarName::temp(),
                    type_spec: "&&T".into(),
                },
                VarDecl {
                    name: VarName::temp(),
                    type_spec: "&T".into(),
                },
            ],
            generic_count: 1,
            relation: TypeSpecRelation::MethodOf("&&T".into()),
            ..Default::default()
        });

        out.insert_intrinsic(FuncCode {
            name: VarName::from("cast"),
            args: vec![VarDecl {
                name: VarName::temp(),
                type_spec: "T".into(),
            }],
            ret_type: "U".into(),
            generic_count: 2,
            ..Default::default()
        });

        out.types.insert("Type".into(), Type::i(64));

        out
    }

    fn insert_intrinsic(&mut self, code: FuncCode) {
        let decl_info = self.insert_code(code);
        self.intrinsics.insert(decl_info);
    }

    pub fn insert_code(&mut self, code: FuncCode) -> FuncDeclInfo {
        let decl_info = code.decl_info();
        self.func_code.insert(decl_info.clone(), code);
        self.append_type_for_name(&decl_info);

        decl_info
    }

    pub(crate) fn append_type_for_name(&mut self, info: &FuncDeclInfo) {
        self.decl_names
            .entry(info.name.clone())
            .and_modify(|set| set.push(info.clone()))
            .or_insert(vec![info.clone()]);
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

    pub fn func_exists(&self, info: &FuncDeclInfo) -> bool { self.func_code.contains_key(info) }

    pub fn reflect_arg_types(&mut self, info: &UniqueFuncInfo, mask: Vec<bool>) {
        assert!({
            let func_type = self.get_func_type(info).unwrap();
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

    pub fn unique_func_info_iter(&self) -> impl Iterator<Item = &UniqueFuncInfo> {
        self.compiled.iter()
    }

    pub fn has_been_compiled(&self, info: &UniqueFuncInfo) -> bool {
        let is_intrinsic = if let Some(decl) = self.get_declaration_of(info) {
            self.intrinsics.contains(&decl)
        } else {
            false
        };
        is_intrinsic || self.compiled.contains(info)
    }

    pub fn name_is_intrinsic(&self, name: &VarName) -> bool {
        self.intrinsics.iter().any(|decl| &decl.name == name)
    }

    pub fn get_declaration_of(&self, info: &UniqueFuncInfo) -> Option<FuncDeclInfo> {
        let possible_decls = self.decl_names.get(&info.name)?;

        for decl in possible_decls {
            if let Some(found_relation) = decl.relation.inner_type() && 
                let Some(looking_for_relation) = info.relation.inner_type() {
                if looking_for_relation.could_come_from(found_relation, self) 
                {
                    return Some(decl.clone());
                }
            } else if decl.relation.inner_type().is_none() && info.relation.inner_type().is_none() {
                return Some(decl.clone());
            };
        }

        None
    }

    pub fn create_func_placeholder<'a>(
        &mut self,
        info: &UniqueFuncInfo,
        context: &'static Context,
        module: &Module<'static>,
    ) -> CResult<()> {
        let function_type = self.get_func_type(info)?;
        let TypeEnum::Func(llvm_function_type) = function_type.as_type_enum()
            else { panic!() };

        let empty_function = module.add_function(
            &*info.to_string(),
            llvm_function_type.llvm_func_type(&context),
            None,
        );

        self.compiled.insert(info.clone());
        self.func_types.insert(info.clone(), function_type.clone());
        self.globals
            .insert(info.name.clone(), (function_type, empty_function.as_global_value()));

        Ok(())
    }

    pub fn get_func_type(&self, info: &UniqueFuncInfo) -> CResult<Type> {
        if let Some(cached_type) = self.func_types.get(info) {
            return Ok(cached_type.clone());
        }

        let code = self.get_code(info.clone())?;

        let ret_type = &self.get_spec(&code.ret_type, info)?;

        let arg_types = code
            .args
            .iter()
            .map(|arg| self.get_spec(&arg.type_spec, info))
            .collect::<TResult<Vec<_>>>()?;

        let func_type = ret_type.clone().func_with_args(arg_types);

        Ok(func_type)
    }

    pub fn get_derived_code(&self, info: &UniqueFuncInfo) -> Option<FuncCode> {
        if let Ok(deriver_info) = DeriverInfo::try_from(info.clone()) && 
            info.relation.inner_type()?.is_known() {
            let deriver = self.derivers.get(&deriver_info)?;
            deriver(self, info.relation.inner_type()?)
        } else {
            None
        }
    }

    // TODO: return pointer from this function
    pub fn get_code(&self, info: UniqueFuncInfo) -> CResult<FuncCode> {
        let decl_info = self.get_declaration_of(&info);
        if let Some(decl_info) = decl_info {
            return Ok(self.func_code.get(&decl_info).unwrap().clone());
        };

        self.get_derived_code(&info)
            .ok_or(FErr::NotFound(info.clone()).into())
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

    pub fn add_type_alias(&mut self, name: TypeName, a: TypeSpec) {
        self.aliases.insert(name, a);
    }

    pub fn contains(&self, key: &TypeName) -> bool { self.aliases.contains_key(key) }

    pub fn get_by_name(&self, name: &TypeName) -> TResult<Type> {
        {
            let cached_type = self.types.get(name);
            if let Some(cached_type) = cached_type {
                return Ok(cached_type.clone());
            }
        }

        let alias = self.get_alias_for(name)?;
        let realized_type = self.get_spec(alias, &vec![])?;
        Ok(realized_type.with_name(name.clone()))
    }

    pub fn get_alias_for(&self, name: &TypeName) -> TResult<&TypeSpec> {
        self.aliases.get(name).ok_or(TErr::Unknown(name.clone()))
    }
}

use std::hash::Hash;

use super::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default, PartialOrd, Ord)]
pub struct FuncDeclInfo {
    pub name: VarName,
    pub relation: TypeSpecRelation,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default, PartialOrd, Ord)]
pub struct UniqueFuncInfo {
    pub name: VarName,
    pub relation: TypeRelation,
    pub own_generics: Vec<Type>,
}

impl ToString for UniqueFuncInfo {
    fn to_string(&self) -> String {
        match &self.relation {
            TypeRelation::Static(typ) => {
                format!("{:?}::{:?}{:?}", typ, self.name, self.own_generics)
            },
            TypeRelation::MethodOf(typ) => {
                format!("M_{:?}{:?}{:?}", typ, self.name, self.own_generics)
            },
            TypeRelation::Unrelated => format!("{:?}{:?}", self.name, self.own_generics),
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
            own_generics: generics,
        }
    }

    pub fn generics(&self) -> Vec<Type> {
        let mut some_generics = self.own_generics.clone();

        if let Some(typ) = self.relation.inner_type() {
            let typ_generics = typ.generics().clone();
            some_generics.extend(typ_generics);
        }

        some_generics
    }

    pub fn og_name(&self) -> VarName { self.name.clone() }
    pub fn is_method(&self) -> bool { matches!(self.relation, TypeRelation::MethodOf(_)) }
    pub fn is_static(&self) -> bool { matches!(self.relation, TypeRelation::Static(_)) }
    pub fn has_generics(&self) -> bool { self.generics().len() > 0 }
}
