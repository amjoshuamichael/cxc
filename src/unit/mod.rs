use self::backends::IsBackend;
use self::functions::DeriverFunc;
use self::functions::DeriverInfo;
use self::functions::TypeLevelFunc;
pub use self::generations::Generations;
pub use self::value_api::Value;
use crate::FuncType;
use crate::errors::CErr;
use crate::errors::CResultMany;
use crate::hlr::hlr_data_output::HLR;
use crate::hlr::prelude::*;
use crate::lex::*;
use crate::libraries::Library;
use crate::mir::MIR;
use crate::mir::mir;
use crate::parse;
use crate::parse::*;
use crate::{XcReflect as XcReflectMac, xc_opaque};
use crate::Type;
pub use add_external::ExternalFuncAdd;
pub use functions::FuncQuery;
pub use reflect::XcReflect;
use slotmap::SecondaryMap;
use slotmap::SlotMap;
use slotmap::new_key_type;
use std::any::TypeId;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt::Debug;
use std::pin::Pin;
use crate::backend::Backend;
pub use backends::function::*;

mod add_external;
pub mod callable;
mod functions;
pub mod get_type_spec;
mod reflect;
mod rust_type_name_conversion;
mod value_api;
mod generations;
pub mod backends;

use crate as cxc;

pub struct Unit {
    pub comp_data: Pin<Box<CompData>>,
    pub(crate) backend: Backend,
}

#[derive(Clone, Default, XcReflectMac)]
#[xc_opaque]
pub struct CompData {
    typedefs: BTreeMap<TypeName, TypeSpec>,
    pub(crate) type_level_funcs: BTreeMap<TypeName, TypeLevelFunc>,
    reflected_types: BTreeMap<TypeId, Type>,
    pub(crate) func_code: SlotMap<FuncCodeId, FuncCode>,
    derivers: BTreeMap<DeriverInfo, DeriverFunc>,
    intrinsics: BTreeSet<FuncCodeId>,
    processed: SlotMap<FuncId, ProcessedFuncInfo>,
    pub generations: Generations,
    pub globals: BTreeMap<VarName, Type>,
}

#[derive(Debug, Clone)]
pub struct ProcessedFuncInfo {
    pub dependencies: HashMap<FuncQuery, Option<(FuncId, u32)>>,
    pub name: VarName,
    pub relation: TypeRelation,
    pub generics: Vec<Type>,
    pub typ: FuncType,
}

impl ProcessedFuncInfo {
    pub fn to_string(&self, generations: &Generations, func_id: FuncId) -> String {
        let gen = generations.get_gen_of(func_id);
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

new_key_type! {
    pub struct FuncCodeId; 
    pub struct FuncId; 
}

impl Debug for CompData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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

        new
    }

    pub fn push_script(&mut self, script: &str) -> CResultMany<Vec<FuncQuery>> {
        // TODO: make sure the global can only be accessed from a compilation script

        let lexed = lex(script);

        let parsed = parse::parse(lexed).map_err(|errs| {
            #[cfg(feature = "xc-debug")]
            for err in &errs {
                println!("{err}");
            }

            { errs }.drain(..).map(CErr::Parse).collect::<Vec<_>>()
        })?;

        let has_comp_script = parsed.comp_script.is_some();

        let funcs_to_process = {
            for decl in parsed.types.iter().cloned() {
                self.comp_data.add_type_alias(decl.name, decl.typ);
            }

            let mut declarations = Vec::<FuncCodeId>::new();

            for code in parsed.funcs.iter() {
                let is_generic = code.has_generics();

                let decl = self.comp_data.insert_code(code.clone());

                if !is_generic {
                    declarations.push(decl);
                }
            }

            if let Some(comp_script) = parsed.comp_script {
                declarations.push(self.comp_data.insert_code(comp_script));
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

        self.compile_func_set(funcs_to_process.iter().cloned().collect())?;

        if has_comp_script {
            self.run_comp_script();
        }

        Ok(funcs_to_process)
    }

    pub fn compile_func_set(&mut self, mut set: HashSet<FuncQuery>) -> CResultMany<()> {
        if set.is_empty() {
            return Ok(());
        }

        let mut all_func_ids = Vec::<FuncId>::new();

        let mut hlrs = SecondaryMap::<FuncId, HLR>::new();

        while !set.is_empty() {
            for query in &set {
                // TODO: here, get code id, but also get code generics with id
                let code = self.comp_data.get_code(query.clone()).unwrap();

                let func_arg_types = code
                    .args
                    .iter()
                    .map(|VarDecl { type_spec, .. }| {
                         self.comp_data.get_spec(type_spec, query)
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                let func_ret_type = self.comp_data.get_spec(&code.ret_type, query)?;
                let func_type = func_ret_type.func_with_args(func_arg_types);

                if query.generics.is_empty() {
                    self.comp_data.globals.insert(
                        query.name.clone(),
                        func_type.clone()
                    );
                }
            }

            let current_func_ids: Vec<FuncId> = set
                .drain()
                .map(|id| {
                    let code = self.comp_data.get_code(id.clone()).unwrap();

                    let (hlr, processed_func_info) = 
                        hlr(id.clone(), &self.comp_data, code.as_ref())?;

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

                    hlrs.insert(func_id, hlr);
                    all_func_ids.push(func_id);
                    self.comp_data.generations.update(func_id);

                    Ok(func_id)
                })
                .collect::<CResultMany<Vec<FuncId>>>()?;


            let uncompiled_dependencies = current_func_ids
                .iter()
                .flat_map(|f| { self.comp_data.processed[*f].dependencies.keys() })
                .filter(|f| {
                    if !self.has_been_compiled(f) {
                        true
                    } else if self.comp_data.query_for_id(f).is_some() {
                        false
                    } else if let Some(code_id) = self.comp_data.query_for_code(f) &&
                        self.comp_data.intrinsics.contains(&code_id) {
                        false
                    } else {
                        true
                    }
                })
                .cloned()
                .collect::<BTreeSet<FuncQuery>>();

            let mut dependents = BTreeSet::new();
            for (dependent_id, ProcessedFuncInfo { dependencies, name, relation, generics, .. }) 
                in &self.comp_data.processed {

                if all_func_ids.contains(&dependent_id) {
                    continue; 
                }

                for (query, og_location) in dependencies {
                    if og_location.is_none() { continue }
                    let og_location = og_location.unwrap();

                    let func_id = self.comp_data.query_for_id(&query).unwrap();

                    let func_gen = self.comp_data.generations.get_gen_of(func_id);

                    if og_location != (func_id, func_gen) {
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
        }

        for id in &all_func_ids {
            let mut dependencies = self.comp_data.processed[*id].dependencies.clone();

            for (func_query, fn_loc) in &mut dependencies {
                if let Some(func_id) = self.comp_data.query_for_id(&func_query) {
                    dbg!(&func_query);
                    dbg!(&func_id);

                    let func_generation = self.comp_data.generations.get_gen_of(func_id);
                    *fn_loc = Some((func_id, func_generation));
                }
            }

            self.comp_data.processed[*id].dependencies = dependencies;
        }

        self.backend.begin_compilation_round();

        let mut mirs = SecondaryMap::<FuncId, MIR>::new();

        for (func_id, hlr) in hlrs {
            let processed_func_info = &self.comp_data.processed[func_id];
            self.backend.register_function(func_id, processed_func_info);

            let dependencies = processed_func_info
                .dependencies
                .keys()
                .filter_map(|query| {
                    Some((query.clone(), self.comp_data.query_for_id(query)?))
                })
                .collect();
            
            let mir = mir(hlr, dependencies);
            mirs.insert(func_id, mir);
        }

        for (func_id, mir) in mirs {
            self.backend.compile_function(func_id, mir);
        }

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

        let comp_script_fn = self.backend.get_function(comp_script_id).unwrap();

        comp_script_fn.downcast::<(), ()>()();
    }

    /// Gets a function from the unit. Returns none if that function doesn't already exist.
    /// Note that if you have a generic function, and the specific variation of that
    /// function hasn't been compiled, `get_fn` will also return None.
    pub fn get_fn(&self, with: impl Into<FuncQuery>) -> Option<&Func> {
        let with = with.into();

        let id = self.comp_data.query_for_id(&with)?;

        let correct_function_ptr = self.backend.get_function(id);
        assert!(correct_function_ptr.is_some());
        return correct_function_ptr;
    }

    pub fn has_been_compiled(&self, query: &FuncQuery) -> bool {
        if let Some(id) = self.comp_data.query_for_id(query) &&
            self.backend.has_been_compiled(id) {
            true
        } else if let Some(code_id) = self.comp_data.query_for_code(query) && 
            self.comp_data.intrinsics.contains(&code_id) {
            true
        } else {
            false
        }
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

        self.comp_data.globals.insert(name.clone(), typ.get_ref());

        self.backend.add_global(name, typ.get_ref(), val as _);
    }

    pub fn get_fn_by_ptr(&self, ptr: *const usize) -> Option<(FuncId, Func)> {
        let (id, func) = self.backend.compiled_iter().find(|(_, func)| func.get_pointer() == ptr)?;

        Some((id.clone(), func.clone()))
    }
}
