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

impl TryFrom<FuncQuery> for DeriverInfo {
    type Error = ();

    fn try_from(info: FuncQuery) -> Result<Self, ()> {
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
            name: VarName::from("intrinsic_alloc"),
            args: vec![VarDecl {
                name: VarName::None,
                type_spec: TypeSpec::Int(64),
            }],
            ret_type: TypeSpec::UInt(8).get_ref(),
            generic_count: 0,
            code: Expr::Block(Vec::new()),
            relation: TypeSpecRelation::Unrelated,
            is_external: true,
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
            is_external: true,
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
            is_external: true,
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
            is_external: true,
        });

        out.insert_intrinsic(FuncCode {
            name: VarName::from("size_of"),
            ret_type: Type::i(64).into(),
            args: Vec::new(),
            generic_count: 1,
            code: Expr::Block(Vec::new()),
            relation: TypeSpecRelation::Unrelated,
            is_external: true,
        });

        out.insert_intrinsic(FuncCode {
            name: VarName::from("alignment_of"),
            ret_type: Type::i(64).into(),
            args: Vec::new(),
            generic_count: 1,
            code: Expr::Block(Vec::new()),
            relation: TypeSpecRelation::Unrelated,
            is_external: true,
        });

        out.insert_intrinsic(FuncCode {
            name: VarName::from("cast"),
            args: vec![VarDecl {
                name: VarName::None,
                type_spec: "T".into(),
            }],
            ret_type: "U".into(),
            generic_count: 2,
            is_external: true,
            ..Default::default()
        });

        out.insert_intrinsic(FuncCode {
            name: VarName::from("typeobj"),
            args: vec![],
            ret_type: "Type".into(),
            generic_count: 1,
            is_external: true,
            ..Default::default()
        });

        out.typedefs.insert("Type".into(), TypeSpec::Int(64));

        out
    }

    fn insert_intrinsic(&mut self, code: FuncCode) {
        let decl_info = self.insert_code(code);
        self.intrinsics.insert(decl_info);
    }

    pub fn insert_code(&mut self, code: FuncCode) -> FuncCodeId {
        for (code_id, old_code) in &self.func_code {
            if old_code.name == code.name && old_code.relation == code.relation {
                self.func_code.remove(code_id);
                break;
            }
        }

        self.func_code.insert(code)
    }

    pub fn name_is_intrinsic(&self, name: &VarName) -> bool {
        self.intrinsics.iter().any(|id| {
            &self.func_code[*id].name == name
        })
    }

    pub fn query_for_code(&self, info: &FuncQuery) -> Option<FuncCodeId> {
        use std::mem::discriminant;

        for (id, code) in &self.func_code {
            if code.name != info.name {
                continue;
            }

            if code.relation.inner_type().is_none() && info.relation.inner_type().is_none()  {
                return Some(id);
            } else if let Some(looking_at_relation) = code.relation.inner_type() && 
                let Some(looking_for_relation) = info.relation.inner_type() &&
                discriminant(&looking_at_relation) == discriminant(&looking_at_relation) &&
                looking_for_relation.works_as_method_on(looking_at_relation, self) {
                return Some(id);
            }
        }

        None
    }

    pub fn query_for_id(&self, query: &FuncQuery) -> Option<FuncId> {
        if let Some(code_id) = self.query_for_code(query) &&
            let Some(realizations) = self.realizations.get(code_id) {
            for realization in realizations {
                if self.processed[*realization].generics == query.generics {
                    return Some(*realization);
                }
            }
        } else {
            for (func_id, info) in &self.processed {
                if info.name == query.name && 
                    info.relation == query.relation && 
                    info.generics == query.generics {
                    return Some(func_id);
                }
            }
        }

        return None;
    }


    pub fn get_func_type(&self, query: &FuncQuery) -> CResult<FuncType> {
        if let Some(func_id) = self.query_for_id(query) {
            return Ok(self.processed[func_id].typ.clone());
        }

        let code = self.get_code(query.clone())?;

        let ret_type = self.get_spec(&code.ret_type, query)?;

        let arg_types = code
            .args
            .iter()
            .map(|arg| self.get_spec(&arg.type_spec, query))
            .collect::<TResult<Vec<_>>>()?;

        Ok(FuncType {
            ret: ret_type,
            args: arg_types,
        })
    }

    pub fn get_derived_code(&self, info: &FuncQuery) -> Option<FuncCode> {
        if let Ok(deriver_info) = DeriverInfo::try_from(info.clone()) && 
            info.relation.inner_type()?.is_known() {
            let deriver = self.derivers.get(&deriver_info)?;
            deriver(self, info.relation.inner_type()?)
        } else {
            None
        }
    }

    // TODO: completely redo the way that derivations are handled, because re-running
    // the deriver every time seems like a bad way to do things. In rust, they know what
    // the type signature is going to be because everything is done through traits.
    // Maybe alongside each deriver function there should be a shorter function that
    // just produces the type.
    pub fn get_code(&self, query: FuncQuery) -> CResult<Cow<FuncCode>> {
        if let Some(decl_info) = self.query_for_code(&query) {
            return Ok(Cow::Borrowed(&self.func_code[decl_info]));
        };

        self.get_derived_code(&query)
            .map(Cow::Owned)
            .ok_or(FErr::NotFound(query.clone()).into())
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
        self.typedefs.insert(name, a);
    }

    pub fn contains(&self, key: &TypeName) -> bool { self.typedefs.contains_key(key) }

    pub fn get_by_name(&self, name: &TypeName) -> TResult<Type> {
        let alias = self.get_typedef_of(name)?;
        let realized_type = self.get_spec(alias, &())?;
        Ok(realized_type.with_name(name.clone()).with_generics(&Vec::new()))
    }

    pub fn get_typedef_of(&self, name: &TypeName) -> TResult<&TypeSpec> {
        self.typedefs.get(name).ok_or(TErr::Unknown(name.clone()))
    }
}

use std::{hash::Hash, borrow::Cow};

use super::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default, PartialOrd, Ord, XcReflectMac)]
#[repr(C)] // TODO: this shouldn't have to be here
// TODO: maybe make a function query that uses a func_code_id with some generics?
pub struct FuncQuery {
    pub name: VarName,
    pub relation: TypeRelation,
    pub generics: Vec<Type>,
}

impl FuncQuery {
    pub fn new(
        og_name: &VarName,
        relation: &TypeRelation,
        generics: Vec<Type>,
    ) -> FuncQuery {
        FuncQuery {
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

impl From<&str> for FuncQuery {
    fn from(name: &str) -> FuncQuery {
        FuncQuery {
            name: name.into(),
            ..Default::default()
        }
    }
}

impl From<VarName> for FuncQuery {
    fn from(name: VarName) -> FuncQuery {
        FuncQuery {
            name,
            ..Default::default()
        }
    }
}
