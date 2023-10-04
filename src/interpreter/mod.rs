use std::{collections::{HashMap, HashSet}, sync::{Arc, RwLock}};

use slotmap::SecondaryMap;

use crate::{unit::{backends::{IsBackend, LowerableFunc}, FuncId, callable::CallInput, ProcessedFuncInfo}, mir::MIR, FuncType, Value, Type, VarName};

mod lowered_func;
mod interpreter;
mod call_with_values;

use lowered_func::LoweredInterpreterFunc;

#[derive(Default, Debug)]
pub struct IntepreterState {
    mirs: SecondaryMap<FuncId, MIR>,
    globals: HashMap<VarName, Global>,
    external_functions: SecondaryMap<FuncId, ExternalFunc>,
}

#[derive(Debug)]
struct Global {
    typ: Type,
    pointer: *const usize,
}

#[derive(Debug)]
struct ExternalFunc {
    pointer: *const usize,
    typ: FuncType,
}

#[derive(Default)]
pub struct InterpreterBackend {
    state: Arc<RwLock<IntepreterState>>,
    func_refs: SecondaryMap<FuncId, LowerableInterpreterFunc>,
    to_recompile: HashSet<FuncId>,
}

#[derive(Debug, Clone)]
pub struct LowerableInterpreterFunc {
    state: Arc<RwLock<IntepreterState>>,
    func_id: FuncId,
}

impl LowerableInterpreterFunc {
    pub fn get_pointer(&self) -> *const usize {
        unsafe { std::mem::transmute(self.func_id) }
    }

    pub fn typ(&self) -> FuncType {
        self.state.read().unwrap().mirs[self.func_id].func_type.clone()
    }

    pub fn downcast<A, R>(&self) -> LoweredInterpreterFunc<A, R> where A: CallInput<R> {
        self.lower::<A, R>()
    }

    pub fn call_void(&self) -> Value {
        // even though we're using the () as the return type in this lower call, the
        // LoweredInterpreterFunc is still going to produce a cxc::Value.
        self.lower::<(), ()>()()
    }
}

impl LowerableFunc for LowerableInterpreterFunc {
    type LowerTo<A, R> = LoweredInterpreterFunc<A, R>;

    fn lower<A, R>(&self) -> Self::LowerTo<A, R> where A: CallInput<R> {
        LoweredInterpreterFunc::<A, R> {
            state: self.state.clone(),
            func_id: self.func_id,
            _marker: std::marker::PhantomData::default(),
        }
    }
}

impl IsBackend for InterpreterBackend {
    type CallableFuncRef<A, R> = LoweredInterpreterFunc<A, R> where A: CallInput<R>;

    type LowerableFuncRef = LowerableInterpreterFunc;

    fn create() -> Self { Self::default() }

    fn begin_compilation_round(&mut self) { }

    fn register_function(&mut self, _: FuncId, _: &ProcessedFuncInfo) { }

    fn mark_to_recompile(&mut self, func_id: FuncId) { 
        self.to_recompile.insert(func_id);
    }

    fn compile_functions(&mut self, to_compile: SecondaryMap<FuncId, MIR>) {
        let mut state_lock = self.state.write().unwrap();
        for (func_id, mir) in to_compile {
            self.func_refs.insert(func_id, LowerableInterpreterFunc {
                state: self.state.clone(),
                func_id,
            });

            state_lock.mirs.insert(func_id, mir);
        }
    }

    fn end_compilation_round(&mut self) { 
        self.to_recompile.clear()
    }

    fn has_been_compiled(&self, id: FuncId) -> bool {
        !self.to_recompile.contains(&id) && self.state.read().unwrap().mirs.contains_key(id)
    }

    fn get_function(&self, func_id: FuncId) -> &Self::LowerableFuncRef {
        &self.func_refs[func_id]
    }

    fn compiled_iter<'a>(&'a self) -> Box<dyn Iterator<Item = (FuncId, &Self::LowerableFuncRef)> + 'a> {
        Box::new(self.func_refs.iter())
    }

    fn add_global(&mut self, name: VarName, typ: Type, pointer: *mut usize) {
        self.state.write().unwrap().globals.insert(name, Global { typ, pointer });
    }

    fn add_external_func(
        &mut self, 
        id: FuncId, 
        typ: FuncType, 
        _: &ProcessedFuncInfo, 
        pointer: *const usize
    ) {
        self.state.write().unwrap().external_functions.insert(id, ExternalFunc {
            pointer, 
            typ,
        });
    }
}
