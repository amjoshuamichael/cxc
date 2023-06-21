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
                name: VarName::None,
                type_spec: TypeSpec::Int(64),
            }],
            ret_type: TypeSpec::GenParam(0).get_ref(),
            generic_count: 1,
            code: Expr::Block(Vec::new()),
            relation: TypeSpecRelation::Unrelated,
        });

        out.insert_intrinsic(FuncCode {
            name: VarName::from("free"),
            ret_type: TypeSpec::Void,
            args: vec![VarDecl {
                name: VarName::None,
                type_spec: TypeSpec::GenParam(0).get_ref(),
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
                    name: VarName::None,
                    type_spec: TypeSpec::GenParam(0).get_ref(),
                },
                VarDecl {
                    name: VarName::None,
                    type_spec: TypeSpec::GenParam(0).get_ref(),
                },
                VarDecl {
                    name: VarName::None,
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
                    name: VarName::None,
                    type_spec: TypeSpec::GenParam(0).get_ref(),
                },
                VarDecl {
                    name: VarName::None,
                    type_spec: TypeSpec::GenParam(0).get_ref(),
                },
                VarDecl {
                    name: VarName::None,
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
            name: VarName::from("cast"),
            args: vec![VarDecl {
                name: VarName::None,
                type_spec: "T".into(),
            }],
            ret_type: "U".into(),
            generic_count: 2,
            ..Default::default()
        });

        out.insert_intrinsic(FuncCode {
            name: VarName::from("typeobj"),
            args: vec![],
            ret_type: "Type".into(),
            generic_count: 1,
            ..Default::default()
        });

        out.aliases.insert("Type".into(), TypeSpec::Int(64));

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

    pub fn name_is_intrinsic(&self, name: &VarName) -> bool {
        self.intrinsics.iter().any(|decl| &decl.name == name)
    }

    pub fn get_declaration_of(&self, info: &UniqueFuncInfo) -> Option<FuncDeclInfo> {
        let possible_decls = self.decl_names.get(&info.name)?;

        for decl in possible_decls {
            if let Some(found_relation) = decl.relation.inner_type() && 
                let Some(looking_for_relation) = info.relation.inner_type() {
                if looking_for_relation.could_come_from(found_relation, self).unwrap() {
                    return Some(decl.clone());
                }
            } else if decl.relation.inner_type().is_none() && info.relation.inner_type().is_none() {
                return Some(decl.clone());
            };
        }

        None
    }

    pub fn get_func_type(&self, info: &UniqueFuncInfo) -> CResult<FuncType> {
        if let Some(func_type) = self.func_types.get(info) {
            return Ok(func_type.clone());
        }

        let code = self.get_code(info.clone())?;

        let ret_type = self.get_spec(&code.ret_type, info)?;

        let arg_types = code
            .args
            .iter()
            .map(|arg| self.get_spec(&arg.type_spec, info))
            .collect::<TResult<Vec<_>>>()?;

        Ok(FuncType {
            ret: ret_type,
            args: arg_types,
        })
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
        let alias = self.get_alias_for(name)?;
        let realized_type = self.get_spec(alias, &())?;
        Ok(realized_type.with_name(name.clone()).with_generics(&Vec::new()))
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

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default, PartialOrd, Ord, XcReflectMac)]
#[repr(C)] // TODO: this shouldn't have to be here
pub struct UniqueFuncInfo {
    pub name: VarName,
    pub relation: TypeRelation,
    pub generics: Vec<Type>,
}

impl UniqueFuncInfo {
    pub fn to_string(&self, generations: &Generations) -> String {
        let gen = generations.get_gen_of(self);
        let gen_suffix = format!("{gen:x}");

        match &self.relation {
            TypeRelation::Static(typ) => {
                format!("{:?}::{:?}{:?}", typ, self.name, self.generics)
            },
            TypeRelation::MethodOf(typ) => {
                format!("M_{:?}{:?}{:?}", typ, self.name, self.generics)
            },
            TypeRelation::Unrelated => format!("{:?}{:?}", self.name, self.generics),
        }
        .replace("&", "ref")
        .chars()
        .filter(|c| c.is_alphanumeric() || *c == '_')
        .collect::<String>()
            + &*gen_suffix
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

    pub fn generics(&self) -> Vec<Type> {
        let mut some_generics = self.generics.clone();

        if let Some(typ) = self.relation.inner_type() {
            let typ_generics = typ.generics().clone();
            some_generics.extend(typ_generics);
        }

        some_generics
    }

    pub fn og_name(&self) -> VarName { self.name.clone() }
    pub fn is_method(&self) -> bool { matches!(self.relation, TypeRelation::MethodOf(_)) }
    pub fn is_static(&self) -> bool { matches!(self.relation, TypeRelation::Static(_)) }
    pub fn has_generics(&self) -> bool { !self.generics().is_empty() }
}

impl From<&str> for UniqueFuncInfo {
    fn from(name: &str) -> UniqueFuncInfo {
        UniqueFuncInfo {
            name: name.into(),
            ..Default::default()
        }
    }
}

impl From<VarName> for UniqueFuncInfo {
    fn from(name: VarName) -> UniqueFuncInfo {
        UniqueFuncInfo {
            name,
            ..Default::default()
        }
    }
}
