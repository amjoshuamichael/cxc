use self::backends::IsBackend;
use self::functions::DeriverFunc;
use self::functions::DeriverInfo;
use self::functions::TypeLevelFunc;
pub use self::generations::Generations;
pub use self::value_api::Value;
use crate::FuncType;
use crate::errors::CErr;
use crate::errors::CResultMany;
use crate::errors::TResult;
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
pub use functions::UniqueFuncInfo;
pub use reflect::XcReflect;
use slotmap::SlotMap;
use slotmap::new_key_type;
use std::any::TypeId;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
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
    pub generations: Generations,
    dependencies: BTreeMap<UniqueFuncInfo, BTreeSet<UniqueFuncInfo>>,
    pub func_types: BTreeMap<UniqueFuncInfo, FuncType>,
    pub globals: BTreeMap<VarName, Type>,
}

new_key_type! { pub struct FuncCodeId; }

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

    pub fn push_script(&mut self, script: &str) -> CResultMany<Vec<UniqueFuncInfo>> {
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

            let funcs_to_compile: Vec<UniqueFuncInfo> = { declarations }
                .drain(..)
                .map(|code_id| {
                    let code = &self.comp_data.func_code[code_id];
                    let relation = code
                        .relation
                        .clone()
                        .map_inner_type(|spec| self.comp_data.get_spec(&spec, &()).unwrap());

                    let func_info = UniqueFuncInfo {
                        name: code.name.clone(),
                        relation,
                        ..Default::default()
                    };

                    self.comp_data.generations.update(func_info.clone());

                    func_info
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

    pub fn compile_func_set(&mut self, mut set: BTreeSet<UniqueFuncInfo>) -> CResultMany<()> {
        if set.is_empty() {
            return Ok(());
        }

        self.backend.begin_compilation_round();

        let mut all_funcs_to_compile = Vec::new();
        let mut all_func_infos = Vec::new();

        while !set.is_empty() {
            all_func_infos.extend(set.clone().into_iter());

            for info in &set {
                // register all functions
                let code = self.comp_data.get_code(info.clone()).unwrap();

                let func_arg_types = code
                    .args
                    .iter()
                    .map(|VarDecl { type_spec, .. }| {
                         self.comp_data.get_spec(type_spec, info)
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                let func_ret_type = self.comp_data.get_spec(&code.ret_type, info)?;
                let func_type = func_ret_type.func_with_args(func_arg_types);

                if info.generics.is_empty() {
                    self.comp_data.globals.insert(
                        info.name.clone(),
                        func_type.clone()
                    );
                }
            }
            
            let func_reps: Vec<MIR> = { set }
                .into_iter()
                .map(|info| {
                    let code = self.comp_data.get_code(info.clone()).unwrap();

                    let hlr = hlr(info.clone(), &self.comp_data, code)?;
                    let mir = mir(hlr);

                    self.backend.register_function(info.clone(), mir.func_type.clone());

                    Ok(mir)
                })
                .collect::<CResultMany<Vec<MIR>>>()?;

            set = func_reps
                .iter()
                .flat_map(|f| { f.dependencies.clone().into_iter() })
                .filter(|f| 
                    !self.has_been_compiled(f).unwrap() && !all_func_infos.contains(&f)
                )
                .collect();

            for func in func_reps.iter() {
                let depended_on_by = self.comp_data
                    .dependencies
                    .get(&func.info)
                    .cloned()
                    .unwrap_or_default(); // if dependencies aren't listed, assume 
                                          // there aren't any

                let depended_on_by = depended_on_by
                    .into_iter()
                    .filter(|f| self.comp_data.generations.update(f.clone()));

                set.extend(depended_on_by);
            }

            all_funcs_to_compile.extend(func_reps.into_iter());
        }

        for func in &all_funcs_to_compile {
            for depends_on in &func.dependencies {
                let calling_set = self.comp_data.dependencies.entry(depends_on.clone()).or_default();
                calling_set.insert(func.info.clone());
            }

            self.comp_data.func_types.insert(func.info.clone(), func.func_type.clone());
        }

        for mir in all_funcs_to_compile {
            self.backend.compile_function(mir);
        }

        self.backend.end_compilation_round();

        #[cfg(feature = "backend-debug")]
        println!("--finished all compilation--");

        Ok(())
    }
    
    fn run_comp_script(&self) {
        let func = self.backend.get_function(UniqueFuncInfo {
            name: VarName::None,
            ..Default::default()
        }).unwrap();

        func.downcast::<(), ()>()();
    }

    pub fn get_fn(&self, with: impl Into<UniqueFuncInfo>) -> Option<&Func> {
        self.backend.get_function(with)
    }

    pub fn has_been_compiled(&self, info: &UniqueFuncInfo) -> TResult<bool> {
        if let Some(decl) = self.comp_data.get_declaration_of(info)? && 
            self.comp_data.intrinsics.contains(&decl) {
            Ok(true)
        } else if self.backend.has_been_compiled(&info) {
            Ok(true)
        } else {
            Ok(false)
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

    pub fn get_fn_by_ptr(&self, ptr: *const usize) -> Option<(UniqueFuncInfo, Func)> {
        let (info, func): (&UniqueFuncInfo, &Func) = self.backend.compiled_iter().find(|(_, func)| func.get_pointer() == ptr)?;

        Some((info.clone(), func.clone()))
    }
}
