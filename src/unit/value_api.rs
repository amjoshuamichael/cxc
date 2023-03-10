use crate::{
    errors::CResult,
    hlr::hlr,
    lex::{indent_parens, lex, VarName},
    parse::{self, Expr, FuncCode, TypeRelation},
    to_llvm::{add_sret_attribute_to_func, compile_routine},
    typ::{FuncType, ReturnStyle},
    TypeEnum, Unit, XcReflect,
};
use std::{collections::HashMap, mem::transmute};

use crate::Type;

use super::UniqueFuncInfo;

pub type ExternFunc<I, O> = unsafe extern "C" fn(_: I, ...) -> O;

#[derive(Default, Debug)]
pub struct XcValue {
    typ: Type,
    data: Vec<u8>,
}

impl XcReflect for XcValue {
    fn alias_code() -> String { "XcValue = { typ: Type, data: Vec<u8> }".to_string() }
}

// This MAX_BYTES static is used as an output for functions that return a value
// larger than 16 bytes. These functions really return through a pointer that is
// passed as an argument, but on ARM machines, this pointer has its own special
// register, so we need rust to put the pointer into this register. This could
// be done through inline assmebly, but it's difficult to target arm assembly on
// rust. Therefore, we use this static as a workaround.
const MAX_VALUE_SIZE: usize = 4096;
type MaxBytes = [u8; MAX_VALUE_SIZE];
static mut MAX_BYTES: MaxBytes = [0; MAX_VALUE_SIZE];

impl XcValue {
    pub fn new_from_arr<const N: usize>(typ: Type, data: [u8; N]) -> Self {
        let size = typ.size() as usize;
        let data = data[0..size].to_vec();

        Self { typ, data }
    }

    pub fn new_from_vec(typ: Type, data: Vec<u8>) -> Self { Self { typ, data } }

    pub unsafe fn new_from_ptr(typ: Type, ptr: *const u8) -> Self {
        let slice = std::slice::from_raw_parts(ptr, typ.size());

        Self {
            typ,
            data: slice.to_vec(),
        }
    }

    #[cfg(feature = "bytemuck")]
    pub fn new_reflect<T: XcReflect + bytemuck::NoUninit>(data: T, unit: &Unit) -> Self {
        let typ = unit
            .get_reflect_type::<T>()
            .expect("type contains generics");

        Self {
            typ,
            data: Vec::from(bytemuck::bytes_of(&data)),
        }
    }

    pub fn to_string(&self, unit: &mut Unit) -> String {
        let info = UniqueFuncInfo {
            name: "to_string".into(),
            relation: TypeRelation::MethodOf(self.typ.clone()),
            ..Default::default()
        };

        unit.compile_func_set(vec![info.clone()]).unwrap();

        let mut output = String::new();

        unsafe {
            let func = unit.get_fn_by_info::<(&mut String, *const usize), ()>(&info);
            func(&mut output, self.const_ptr());
        };

        indent_parens(output)
    }

    pub unsafe fn get_data_as<T>(&self) -> *const T { self.data.as_ptr() as *const T }
    pub unsafe fn consume<T>(mut self) -> T {
        let capacity = self.data.capacity();
        let data_ptr = self.data.as_mut_ptr();
        let mut casted_vec =
            Vec::from_raw_parts(data_ptr as *mut T, std::mem::size_of::<T>(), capacity);
        let mut drained_vec = casted_vec.drain(..);

        // self needs to stay around until the end of the function
        std::mem::forget(self);

        drained_vec.next().unwrap()
    }

    pub fn const_ptr(&self) -> *const usize { &*self.data as *const [u8] as *const usize }

    pub fn get_size(&self) -> usize { self.data.len() }
    pub fn get_type(&self) -> &Type { &self.typ }
    pub fn get_slice(&self) -> &[u8] { &*self.data }
}

impl Unit {
    pub fn get_value(&mut self, of: &str) -> CResult<XcValue> {
        if of == "" {
            return Ok(XcValue::default());
        }

        let temp_name = "Newfunc";

        self.reset_execution_engine();

        let expr = {
            let mut lexed = lex(of);
            let mut context = lexed.split(VarName::temp(), HashMap::new());

            Expr::Block(vec![Expr::Return(box parse::parse_expr(&mut context).unwrap())])
        };

        let code = FuncCode::from_expr(expr);

        let info = UniqueFuncInfo {
            name: VarName::from(temp_name),
            ..Default::default()
        };

        let mut func_rep = hlr(info.clone(), self.comp_data.clone(), code)?;

        {
            let dependencies = func_rep.get_func_dependencies().drain().collect();
            self.compile_func_set(dependencies)?;
        }

        let mut fcs = {
            let TypeEnum::Func(func_type) = func_rep.func_type.as_type_enum()
                else { unreachable!() };
            let ink_func_type = func_type.llvm_func_type(&self.context);

            let mut function = self.module.add_function(temp_name, ink_func_type, None);
            add_sret_attribute_to_func(&mut function, &self.context, &func_type.ret);

            self.new_func_comp_state(
                func_rep.take_tree(),
                func_rep.take_data_flow(),
                function,
                func_rep.take_arg_names(),
            )
        };

        {
            let block = fcs.context.append_basic_block(fcs.function, "");
            fcs.builder.position_at_end(block);
            compile_routine(&mut fcs, &self.module);
        };

        if crate::LLVM_DEBUG {
            println!("{}", self.module.print_to_string().to_string());
        }

        let func_addr = self
            .execution_engine
            .borrow()
            .get_function_address(temp_name)
            .unwrap();

        let TypeEnum::Func(FuncType { ret: ret_type, .. }) = 
            func_rep.func_type.as_type_enum() else { panic!() };

        let value = unsafe {
            match ret_type.return_style() {
                ReturnStyle::Direct | ReturnStyle::ThroughI64 | ReturnStyle::ThroughI32 => {
                    let new_func: ExternFunc<(), i64> = transmute(func_addr);
                    let out: [u8; 8] = transmute(new_func(()));
                    XcValue::new_from_arr(ret_type.clone(), out)
                },
                ReturnStyle::ThroughI64I32
                | ReturnStyle::ThroughI64I64
                | ReturnStyle::MoveIntoI64I64 => {
                    let new_func: ExternFunc<(), (i64, i64)> = transmute(func_addr);
                    let out: [u8; 16] = transmute(new_func(()));
                    XcValue::new_from_arr(ret_type.clone(), out)
                },
                ReturnStyle::Sret => {
                    let new_func: ExternFunc<(), MaxBytes> = transmute(func_addr);
                    MAX_BYTES = new_func(());
                    let data_vec = MAX_BYTES[..ret_type.size()].to_vec();
                    XcValue::new_from_vec(ret_type.clone(), data_vec)
                },
                ReturnStyle::Void => {
                    panic!("value returns none!")
                },
            }
        };

        self.execution_engine
            .borrow()
            .free_fn_machine_code(fcs.function);

        unsafe { fcs.function.delete() };

        Ok(value)
    }
}

impl<'a> IntoIterator for &'a XcValue {
    type Item = &'a u8;
    type IntoIter = std::slice::Iter<'a, u8>;

    fn into_iter(self) -> Self::IntoIter { self.data.iter() }
}

impl IntoIterator for XcValue {
    type Item = u8;
    type IntoIter = std::vec::IntoIter<u8>;

    fn into_iter(self) -> Self::IntoIter { self.data.into_iter() }
}
