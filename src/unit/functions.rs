use crate::{
    errors::{CResult, FErr, TErr, TResult},
    FuncType, Type, TypeRelation, typ::can_transform::{transformation_steps_dist, Transformation},
};

pub type DeriverFunc = fn(&CompData, Type) -> Option<FuncCode>;
pub type TypeLevelFunc = fn(Vec<Type>, &CompData) -> Type;

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct DeriverInfo {
    pub func_name: VarName,
    pub is_static: bool,
}

impl<'a> TryFrom<FuncCodeQuery<'a>> for DeriverInfo {
    type Error = ();

    fn try_from(info: FuncCodeQuery) -> Result<Self, ()> {
        let is_static = match info.relation {
            TypeRelation::MethodOf(_) => false,
            TypeRelation::Static(_) => true,
            TypeRelation::Unrelated => return Err(()),
        };

        Ok(DeriverInfo {
            func_name: info.name.clone(),
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
                    type_spec: TypeSpec::UInt(64),
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
                    type_spec: TypeSpec::UInt(64),
                },
            ],
            generic_count: 1,
            code: Expr::Block(Vec::new()),
            relation: TypeSpecRelation::Unrelated,
            is_external: true,
        });

        out.insert_intrinsic(FuncCode {
            name: VarName::from("size_of"),
            ret_type: TypeSpec::UInt(64),
            args: Vec::new(),
            generic_count: 1,
            code: Expr::Block(Vec::new()),
            relation: TypeSpecRelation::Unrelated,
            is_external: true,
        });

        out.insert_intrinsic(FuncCode {
            name: VarName::from("alignment_of"),
            ret_type: TypeSpec::UInt(64),
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

    pub fn insert_code(&mut self, code: FuncCode, backend: Option<&mut Backend>) -> FuncCodeId {
        for (code_id, old_code) in &self.func_code {
            if old_code.name == code.name && old_code.relation == code.relation {
                self.func_code.remove(code_id);

                if let Some(backend) = backend &&
                    let Some(realizations) = self.realizations.remove(code_id) {
                    for realization in realizations {
                        backend.mark_to_recompile(realization);
                    }
                }

                break;
            }
        }

        self.func_code.insert(code)
    }

    fn insert_intrinsic(&mut self, code: FuncCode) {
        let decl_info = self.insert_code(code, None);
        self.intrinsics.insert(decl_info);
    }

    pub fn name_is_intrinsic(&self, name: &VarName) -> bool {
        self.intrinsics.iter().any(|id| {
            &self.func_code[*id].name == name
        })
    }

    pub fn query_for_code_with_transformation(
        &self, 
        query: FuncCodeQuery
    ) -> Option<(FuncCodeId, Option<Transformation>)> {
        if let Some(looking_for_relation) = query.relation.inner_type() {
            let mut closest_function: Option<(FuncCodeId, Option<Transformation>)> = None;
            let mut closest_function_dist = u32::MAX;

            for (id, code) in &self.func_code {
                if &code.name != query.name {
                    continue;
                }

                let Some(looking_at_relation) = code.relation.inner_type() else { continue };

                if let Some(result) = 
                    looking_for_relation.can_transform_to(looking_at_relation.clone()) {
                    let result_dist = transformation_steps_dist(&result.steps);
                    if closest_function_dist > result_dist {
                        closest_function_dist = result_dist;
                        closest_function = Some((id, Some(result)));
                    }
                }
            }

            return closest_function;
        } else {
            for (id, code) in &self.func_code {
                if &code.name != query.name {
                    continue;
                }

                return Some((id, None));
            }

            None
        }
    }

    pub fn query_for_code(
        &self, 
        info: FuncCodeQuery,
    ) -> Option<FuncCodeId> {
        self.query_for_code_with_transformation(info).map(|f| f.0)        
    }

    pub fn query_for_id(&self, query: &FuncQuery) -> Option<FuncId> {
        if let Some((code_id, transformation)) = 
            self.query_for_code_with_transformation(query.code_query()) &&
            let Some(realizations) = self.realizations.get(code_id) {

            let generics = transformation.map(|t| t.generics).unwrap_or_default();

            for realization in realizations {
                let realization_info = &self.processed[*realization];
                if realization_info.generics == generics {
                    return Some(*realization);
                }
            }
        }

        for (func_id, info) in &self.processed {
            if info.name == query.name && 
                info.relation == query.relation && 
                info.generics == query.generics {
                return Some(func_id);
            }
        }

        return None;
    }


    pub fn get_func_type(&self, query: &FuncQuery) -> CResult<FuncType> {
        if let Some(func_id) = self.query_for_id(query) {
            return Ok(self.processed[func_id].typ.clone());
        }

        let (code, _) = self.get_code(query.code_query())?;

        let ret_type = self.get_spec(&code.ret_type, &query.generics)?;

        let arg_types = code
            .args
            .iter()
            .map(|arg| self.get_spec(&arg.type_spec, &query.generics))
            .collect::<TResult<Vec<_>>>()?;

        Ok(FuncType {
            ret: ret_type,
            args: arg_types,
        })
    }

    pub fn get_derived_code(&self, info: FuncCodeQuery) -> Option<FuncCode> {
        if let Ok(deriver_info) = DeriverInfo::try_from(info.clone()) && 
            info.relation.inner_type()?.is_known() {
            let deriver = self.derivers.get(&deriver_info)?;
            deriver(self, info.relation.inner_type().cloned()?)
        } else {
            None
        }
    }

    // TODO: completely redo the way that derivations are handled, because re-running
    // the deriver every time seems like a bad way to do things. In rust, they know what
    // the type signature is going to be because everything is done through traits.
    // Maybe alongside each deriver function there should be a shorter function that
    // just produces the type.
    //
    // also, just based on some debug statistics, this function is called way too much.
    pub fn get_code(&self, query: FuncCodeQuery) -> CResult<(Cow<FuncCode>, FuncQuery)> {
        if let Some((decl_info, trans)) = self.query_for_code_with_transformation(query) {
            return Ok((
                Cow::Borrowed(&self.func_code[decl_info]), 
                FuncQuery {
                    name: query.name.clone(),
                    relation: query.relation.clone(),
                    generics: trans.map(|t| t.generics).unwrap_or_default(),
                },
            ));
        };

        self.get_derived_code(query)
            .map(|code| (
                    Cow::Owned(code), 
                    FuncQuery {
                        name: query.name.clone(),
                        relation: query.relation.clone(),
                        generics: Vec::new(),
                    },
            ))
            .ok_or(FErr::NotFound(query.name.clone(), query.relation.clone()).into())
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
pub struct FuncQuery {
    pub name: VarName,
    pub relation: TypeRelation,
    pub generics: Vec<Type>,
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

impl FuncQuery {
    pub fn code_query<'a>(&'a self) -> FuncCodeQuery<'a> {
        FuncCodeQuery {
            name: &self.name,
            relation: &self.relation,
        }
    }
}

#[derive(Copy, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(C)]
pub struct FuncCodeQuery<'a> {
    pub name: &'a VarName,
    pub relation: &'a TypeRelation,
}

impl<'a> FuncCodeQuery<'a> {
    pub fn to_owned_fcq(&self) -> OwnedFuncCodeQuery {
        OwnedFuncCodeQuery {
            name: self.name.clone(),
            relation: self.relation.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(C)]
// TODO: rename this to code query?? idk why it's called function code query. 
// where else would code be. where else did i think code was.
pub struct OwnedFuncCodeQuery {
    pub name: VarName,
    pub relation: TypeRelation,
}

impl OwnedFuncCodeQuery {
    pub fn to_borrowed_fcq<'a>(&'a self) -> FuncCodeQuery<'a> {
        FuncCodeQuery {
            name: &self.name,
            relation: &self.relation,
        }
    }
}
