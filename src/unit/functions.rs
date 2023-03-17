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
                type_spec: TypeSpec::Ref(box TypeSpec::GenParam(0)),
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
                    type_spec: TypeSpec::Ref(box TypeSpec::GenParam(0)),
                },
                VarDecl {
                    name: VarName::temp(),
                    type_spec: TypeSpec::Ref(box TypeSpec::GenParam(0)),
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
                    type_spec: TypeSpec::Ref(box TypeSpec::GenParam(0)),
                },
                VarDecl {
                    name: VarName::temp(),
                    type_spec: TypeSpec::Ref(box TypeSpec::GenParam(0)),
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
                    type_spec: "T".into(),
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

    pub fn get_reflect_type_masks(&self, info: &UniqueFuncInfo) -> Option<Vec<bool>> {
        let FuncCodeInfo::External { ref reflect_types_of, .. } = &self.compiled.get(info)?.code() else { return None };
        Some(reflect_types_of.clone())
    }

    pub fn unique_func_info_iter(&self) -> impl Iterator<Item = &UniqueFuncInfo> {
        self.compiled.keys()
    }

    pub fn has_been_compiled(&self, info: &UniqueFuncInfo) -> bool {
        let is_intrinsic = if let Some(decl) = self.get_declaration_of(info) {
            self.intrinsics.contains(&decl)
        } else {
            false
        };

        if let Some(func) = self.compiled.get(info) && 
            func.code().pointer().is_some() {
            return true;
        }

        is_intrinsic
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

    pub fn update_func_info(&mut self, info: UniqueFuncInfo, module: &Module<'static>) -> bool {
        assert_eq!(info.gen, Gen::Latest);

        let info_made_old = UniqueFuncInfo {
            gen: *self.generations.last().unwrap(),
            ..info.clone()
        };

        if !self.compiled.contains_key(&info_made_old) && 
            let Some(v) = self.compiled.remove(&info) {
            module
                .get_function(&info.to_string())
                .unwrap()
                .as_global_value()
                .set_name(&info_made_old.to_string());

            v.disable_pointer();
            self.compiled.insert(info.clone(), v.clone());

            // TODO: set fn pointer
            self.compiled.insert(info_made_old.clone(), Func::new_compiled(info_made_old, v.typ()));

            true
        } else { false }
    }

    pub fn new_generation(&mut self) { self.generations.push(Gen::random()) }

    pub fn create_func_placeholder(
        &mut self,
        info: &UniqueFuncInfo,
        context: &'static Context,
        module: &Module<'static>,
    ) -> CResult<()> {
        let function_type = self.get_func_type(info)?;

        let mut empty_function =
            module.add_function(&info.to_string(), function_type.llvm_func_type(context), None);

        add_sret_attribute_to_func(&mut empty_function, context, &function_type.ret);

        self.compiled
            .entry(info.clone())
            .or_insert_with(|| Func::new_compiled(info.clone(), function_type.clone()));

        self.globals.insert(
            info.name.clone(),
            (
                Type::new(TypeEnum::Func(function_type)),
                empty_function.as_global_value().as_pointer_value(),
            ),
        );

        Ok(())
    }

    pub fn get_func_type(&self, info: &UniqueFuncInfo) -> CResult<FuncType> {
        if let Some(compiled) = self.compiled.get(info) {
            return Ok(compiled.typ());
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
        let realized_type = self.get_spec(alias, &vec![])?;
        Ok(realized_type.with_name(name.clone()))
    }

    pub fn get_alias_for(&self, name: &TypeName) -> TResult<&TypeSpec> {
        self.aliases.get(name).ok_or(TErr::Unknown(name.clone()))
    }
}

use std::{
    hash::Hash,
    num::NonZeroU32,
    sync::{Arc, RwLock},
};

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
    pub generics: Vec<Type>,
    pub gen: Gen,
}

impl ToString for UniqueFuncInfo {
    fn to_string(&self) -> String {
        let gen_suffix = match self.gen {
            Gen::Latest => String::new(),
            Gen::Specific(n) => format!("{n:x}"),
        };

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
            gen: Gen::Latest,
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

#[derive(Clone, Debug)]
pub struct Func {
    inner: Arc<RwLock<FuncInner>>,
}

pub struct FuncDowncasted<A, R> {
    pub inner: Func,
    pub _phantoms: (std::marker::PhantomData<A>, std::marker::PhantomData<R>),
}

impl<A, R> Debug for FuncDowncasted<A, R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.inner.code().pointer())
    }
}

impl<A, R> Clone for FuncDowncasted<A, R> {
    fn clone(&self) -> Self {
        FuncDowncasted {
            inner: self.inner.clone(),
            _phantoms: Default::default(),
        }
    }
}

// TODO: include name, relation, and generics in FuncInner
impl Func {
    fn new(inner: FuncInner) -> Func {
        Self {
            inner: Arc::new(RwLock::new(inner)),
        }
    }

    pub fn new_compiled(_: UniqueFuncInfo, typ: FuncType) -> Func {
        Self::new(FuncInner {
            typ,
            code: FuncCodeInfo::Compiled { pointer: None },
        })
    }

    pub fn new_external(
        _: UniqueFuncInfo,
        typ: FuncType,
        pointer: *const usize,
        reflect_types_of: Vec<bool>,
    ) -> Func {
        Self::new(FuncInner {
            typ,
            code: FuncCodeInfo::External {
                pointer,
                reflect_types_of,
            },
        })
    }

    pub fn typ(&self) -> FuncType { self.inner.read().unwrap().typ.clone() }
    pub fn code(&self) -> FuncCodeInfo {
        let inner = self.inner.read().unwrap();
        inner.code.clone()
    }

    pub(super) fn set_pointer(&self, pointer: *const usize) {
        let mut inner = self.inner.write().unwrap();
        inner.code = FuncCodeInfo::Compiled {
            pointer: Some(pointer),
        };
    }

    pub(super) fn disable_pointer(&self) {
        let mut inner = self.inner.write().unwrap();
        inner.code = FuncCodeInfo::Compiled { pointer: None };
    }
}

#[derive(Debug)]
struct FuncInner {
    typ: FuncType,
    code: FuncCodeInfo,
}

#[derive(Clone, Debug)]
pub enum FuncCodeInfo {
    // TODO: use NonNull here
    Compiled {
        pointer: Option<*const usize>,
    },
    External {
        pointer: *const usize,
        // TODO: convert to a slice so that FuncCodeInfo can be Copy
        reflect_types_of: Vec<bool>,
    },
}

impl FuncCodeInfo {
    pub fn pointer(&self) -> Option<*const usize> {
        match self {
            FuncCodeInfo::Compiled { pointer } => *pointer,
            FuncCodeInfo::External { pointer, .. } => Some(*pointer),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, Default, PartialOrd, Ord)]
pub enum Gen {
    #[default]
    Latest,

    Specific(NonZeroU32),
}

impl Gen {
    pub fn random() -> Gen { Gen::Specific(NonZeroU32::new(rand::random::<u32>()).unwrap()) }
}
