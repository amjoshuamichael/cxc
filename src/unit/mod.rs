use self::backends::IsBackend;
use self::functions::DeriverFunc;
use self::functions::DeriverInfo;
use self::functions::TypeLevelFunc;
use self::precalcs::Caches;
pub use self::value_api::Value;
use crate::FuncType;

use crate::TypeEnum;
use crate::errors::CErr;
use crate::errors::CResult;
use crate::errors::CResultMany;
use crate::hlr::hlr_data_output::HLR;
use crate::hlr::prelude::*;
use crate::lex::*;
use crate::libraries::Library;
use crate::libraries::core_lib::CoreLib;
use crate::mir::MIR;
use crate::mir::mir;
use crate::parse;
use crate::parse::*;
use crate::unit::precalcs::cache_type_specs;
use crate::{XcReflect as XcReflectMac, xc_opaque};
use crate::Type;
pub use add_external::ExternalFuncAdd;
pub use functions::{FuncQuery, FuncCodeQuery, OwnedFuncCodeQuery};
use indexmap::IndexMap;
pub use reflect::XcReflect;
use slotmap::SecondaryMap;
use slotmap::SlotMap;
use slotmap::new_key_type;
use std::any::TypeId;
use std::borrow::Cow;
use std::collections::BTreeMap;
use std::collections::HashSet;
use std::fmt::Debug;
use std::iter::once;
use std::pin::Pin;
use anyhow::Error;
use crate::backend::Backend;
pub use backends::function::*;
pub(crate) use precalcs::*;

mod add_external;
pub mod callable;
mod functions;
pub mod get_type_spec;
mod reflect;
mod value_api;
pub mod backends;
mod precalcs;

use crate as cxc;

/// One of the most important structs in the compiler, `Unit` is a God object that contains
/// everything. Most of the important interactions with Cxc as a scripting language are
/// done through the `Unit`, including, but not limited to [`Unit::push_script`], which
/// adds code to the Cxc module.
pub struct Unit {
    pub comp_data: Pin<Box<CompData>>,
    pub(crate) backend: Backend,
}

#[derive(Default, XcReflectMac)]
#[xc_opaque]
pub struct CompData {
    typedefs: BTreeMap<TypeName, TypeSpec>,
    pub(crate) type_level_funcs: BTreeMap<TypeName, TypeLevelFunc>,
    reflected_types: BTreeMap<TypeId, Type>,
    pub(crate) func_code: SlotMap<FuncCodeId, FuncCode>,
    pub(crate) realizations: SecondaryMap<FuncCodeId, HashSet<FuncId>>, // TODO: use secondary set
    derivers: BTreeMap<DeriverInfo, DeriverFunc>,
    intrinsics: HashSet<FuncCodeId>,
    processed: SlotMap<FuncId, ProcessedFuncInfo>,
    pub globals: BTreeMap<VarName, Global>,
    pub(crate) caches: Caches,
}

#[derive(Hash, PartialEq, Clone, Eq, Debug)]
pub enum Global {
    Value {
        name: VarName,
        typ: Type,
    },
    Func(FuncQuery),
}

#[derive(Debug, Clone)]
pub struct ProcessedFuncInfo {
    pub specified_dependencies: IndexMap<OwnedFuncCodeQuery, Option<FuncCodeId>>,
    pub name: VarName,
    pub relation: TypeRelation,
    pub generics: Vec<Type>,
    pub typ: FuncType,
}

impl ProcessedFuncInfo {
    pub fn to_string(&self, func_id: FuncId) -> String {
        let id_suffix = format!("{func_id:?}");

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
            + &*id_suffix
    }
}

new_key_type! {
    pub struct FuncCodeId; 
    pub struct FuncId; 
}

impl Debug for CompData {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "CompData {{ ... }}")
    }
}

impl Default for Unit {
    fn default() -> Self { Self::new() }
}

impl Unit {
    pub fn new() -> Self {
        let mut new = Self {
            comp_data: Pin::new(Box::new(CompData::new())),
            backend: Backend::create(),
        };

        let comp_data_ptr = &mut *new.comp_data as *mut _;
        new.add_global("comp_data".into(), comp_data_ptr);

        new.add_lib(CoreLib);

        new
    }

    pub fn push_script(&mut self, script: &str) -> Result<Vec<FuncQuery>, Error> {
        let lexed = lex(script);

        let parsed = parse::parse(lexed)?;

        let has_comp_script = parsed.comp_script.is_some();

        let funcs_to_process = {
            for decl in parsed.types.iter().cloned() {
                self.comp_data.add_type_spec(decl.name, decl.typ);
            }

            let mut declarations = Vec::<FuncCodeId>::new();

            for code in parsed.funcs.iter() {
                let is_generic = code.has_generics();

                let code_id = self.comp_data.insert_code(code.clone(), Some(&mut self.backend));

                if !is_generic {
                    declarations.push(code_id);
                }
            }

            if let Some(comp_script) = parsed.comp_script {
                declarations.push(self.comp_data.insert_code(comp_script, Some(&mut self.backend)));
            }

            let funcs_to_compile: Vec<FuncQuery> = { declarations }
                .drain(..)
                .map(|code_id| {
                    let code = &self.comp_data.func_code[code_id];

                    let relation = code
                        .relation
                        .clone()
                        .map_inner_type(|spec| self.comp_data.get_spec(&spec, &()).unwrap());

                    FuncQuery {
                        name: code.name.clone(),
                        relation,
                        generics: Vec::new(),
                    }
                })
                .collect();

            funcs_to_compile
        };

        self.compile_func_set(funcs_to_process.iter().cloned().collect()).expect("failure");

        if has_comp_script {
            self.run_comp_script();
        }

        Ok(funcs_to_process)
    }

    pub fn compile_func_set(&mut self, mut set: HashSet<FuncQuery>) -> Result<(), ()> {
        self.comp_data.caches.clear();

        let mut all_func_ids = HashSet::<FuncId>::new();

        let mut hlrs = SecondaryMap::<FuncId, HLR>::new();

        loop {
            for query in &set {
                if query.generics.is_empty() && query.relation.inner_type().is_none() {
                    self.comp_data.globals.insert(
                        query.name.clone(),
                        Global::Func(query.clone()),
                    );
                }
            }

            for query in set.drain() {
                let code_id = self.comp_data.query_for_code(query.code_query());

                // TODO: this is a derivation hack and will be changed when derivations are
                // redone
                let code = if let Some(code_id) = code_id {
                    cache_type_specs(&mut *self.comp_data, code_id, &query);
                    Cow::Borrowed(&self.comp_data.func_code[code_id])
                } else {
                    self.comp_data.get_code(query.code_query()).unwrap().0
                };

                let (hlr, processed_func_info) = 
                    hlr(query.clone(), &self.comp_data, code.as_ref()).expect("");

                let possibly_existing_id = self.comp_data.query_for_id(&FuncQuery {
                    name: processed_func_info.name.clone(),
                    relation: processed_func_info.relation.clone(),
                    generics: processed_func_info.generics.clone(),
                });

                let func_id = if let Some(id) = possibly_existing_id {
                    self.comp_data.processed[id] = processed_func_info;
                    id
                } else {
                    self.comp_data.processed.insert(processed_func_info)
                };

                if let Some(code_id) = code_id {
                    if let Some(realizations) = self.comp_data.realizations.get_mut(code_id) {
                        realizations.insert(func_id);
                    } else {
                        self.comp_data.realizations.insert(
                            code_id, 
                            once(func_id).collect(),
                        );
                    }
                }

                hlrs.insert(func_id, hlr);
                all_func_ids.insert(func_id);
            }

            let uncompiled_dependencies = hlrs
                .iter()
                .flat_map(|(_, hlr)| { hlr.dependencies.iter() })
                .filter(|f| {
                    if let Some(func_id) = self.comp_data.query_for_id(f) &&
                        (all_func_ids.contains(&func_id) || self.backend.has_been_compiled(func_id)) {
                        // function has already been compiled, either in this round or a 
                        // previous round
                        false
                    } else { match self.comp_data.query_for_code(f.code_query()) {
                        Some(code_id) => {
                            if self.comp_data.intrinsics.contains(&code_id)  {
                                false
                            } else if self.comp_data.func_code[code_id].is_external {
                                false
                            } else {
                                // function is not intrinsic, external, or compiled yet
                                true
                            }
                        },
                        None => {
                            if self.comp_data.get_code(f.code_query()).is_ok() {
                                // TODO: deriver hack
                                true
                            } else {
                                // function is external and has no code
                                false
                            }
                        }
                    }}
                })
                .cloned()
                .collect::<HashSet<FuncQuery>>();

            let mut dependents = HashSet::new();
            for (
                dependent_id, 
                ProcessedFuncInfo { specified_dependencies, name, relation, generics, .. }
            ) in &self.comp_data.processed {
                if all_func_ids.contains(&dependent_id) {
                    continue; 
                }

                for (query, og_location) in specified_dependencies {
                    if og_location.is_none() { continue }
                    let og_location = og_location.unwrap();

                    let location = self
                        .comp_data
                        .query_for_code(query.to_borrowed_fcq())
                        .unwrap();

                    if og_location != location {
                        dependents.insert(FuncQuery {
                            name: name.clone(),  
                            relation: relation.clone(),  
                            generics: generics.clone(),  
                        });
                    }
                }
            }

            set.extend(uncompiled_dependencies);
            set.extend(dependents);

            if set.is_empty() {
                break;
            }
        }

        if all_func_ids.is_empty() {
            return Ok(());
        }

        for id in &all_func_ids {
            let mut spec_dependencies = self
                .comp_data
                .processed[*id]
                .specified_dependencies
                .clone();

            for (func_query, fn_loc) in &mut spec_dependencies {
                if let Some(code_id) = self.comp_data.query_for_code(func_query.to_borrowed_fcq()) {
                    *fn_loc = Some(code_id);
                }
            }

            self.comp_data.processed[*id].specified_dependencies = spec_dependencies;
        }

        self.backend.begin_compilation_round();

        let mut mirs = SecondaryMap::<FuncId, MIR>::new();

        for (func_id, hlr) in hlrs {
            let processed_func_info = &self.comp_data.processed[func_id];
            self.backend.register_function(func_id, processed_func_info);

            let dependencies = hlr
                .dependencies
                .iter()
                .filter_map(|query| {
                    Some((query.clone(), self.comp_data.query_for_id(query)?))
                })
                .collect();
            
            let mir = mir(hlr, dependencies);
            mirs.insert(func_id, mir);
        }

        self.backend.compile_functions(mirs);
        self.backend.end_compilation_round();

        #[cfg(feature = "backend-debug")]
        println!("--finished all compilation--");

        Ok(())
    }
    
    fn run_comp_script(&self) {
        let comp_script_query = FuncQuery {
            name: VarName::None,
            ..Default::default()
        };

        let comp_script_id = self.comp_data.query_for_id(&comp_script_query).unwrap();

        let comp_script_fn = self.backend.get_function(comp_script_id);

        comp_script_fn.downcast::<(), ()>()();
    }

    /// Gets a function from the unit. Returns none if that function doesn't already exist.
    /// Note that if you have a generic function, and the specific variation of that
    /// function that you query for hasn't been compiled, `get_fn` will return None.
    pub fn get_fn(&self, with: impl Into<FuncQuery>) -> Option<&<Backend as IsBackend>::LowerableFuncRef> {
        let with = with.into();

        let id = self.comp_data.query_for_id(&with)?;

        let correct_function_ptr = self.backend.get_function(id);

        #[cfg(feature = "ffi-assertions")]
        {
            assert!(
                correct_function_ptr
                    .typ()
                    .args
                    .iter()
                    .all(|param_type| 
                         !matches!(param_type.as_type_enum(), crate::TypeEnum::Array(_))
                     ),
                "Cannot run function that has array value as parameter. Pass in an array pointer instead."
            );
        }

        return Some(correct_function_ptr);
    }

    pub fn add_lib(&mut self, lib: impl Library) -> &mut Self {
        lib.add_to_unit(self);
        self
    }

    pub fn add_method_deriver(&mut self, func_name: VarName, func: DeriverFunc) {
        self.comp_data.derivers.insert(
            DeriverInfo {
                func_name,
                is_static: false,
            },
            func,
        );
    }

    pub fn add_static_deriver(&mut self, func_name: VarName, func: DeriverFunc) {
        self.comp_data.derivers.insert(
            DeriverInfo {
                func_name,
                is_static: true,
            },
            func,
        );
    }

    pub fn add_type_level_func(&mut self, func_name: TypeName, func: TypeLevelFunc) {
        self.comp_data.type_level_funcs.insert(func_name, func);
    }

    pub fn add_global<T: XcReflect + 'static>(&mut self, name: VarName, val: *mut T) {
        let typ = self.get_reflect_type::<T>().unwrap();

        self.comp_data.globals.insert(
            name.clone(), 
            Global::Value { name: name.clone(), typ: typ.clone() },
       );

        self.backend.add_global(name, typ.get_ref(), val as _);
    }

    pub fn get_fn_by_ptr(&self, ptr: *const usize) 
        -> Option<(FuncId, <Backend as backends::IsBackend>::LowerableFuncRef)> {
        let (id, func) = self.backend.compiled_iter().find(|(_, func)| func.get_pointer() == ptr)?;

        Some((id.clone(), func.clone()))
    }
}

impl CompData {
    pub fn get_type_of_global(&self, global: &Global) -> CResult<Type> {
        match global {
            Global::Value { typ, .. } => Ok(typ.clone().get_ref()),
            Global::Func(query) => self
                .get_func_type(query)
                .map(|func_type| Type::new(TypeEnum::Func(func_type))),
        }
    }
}
