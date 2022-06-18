use crate::core_lib::CORE_LIB;
use crate::hlr::prelude::NodeData::*;
use crate::hlr::prelude::*;
use crate::parse::prelude::Opcode;
use crate::to_llvm::prelude::*;
use inkwell::context::Context;
use inkwell::execution_engine::JitFunction;

type SumFunc = unsafe extern "C" fn(u64, u64, u64) -> u64;

fn context_and_info<'a>() -> (Context, ProgramInfo<'a>) {
    (Context::create(), ProgramInfo::default())
}

#[test]
fn basic_llvm() {
    let mut ci = context_and_info();
    let compiler = Compiler::from_context_and_info(&ci.0, &mut ci.1);

    // Actual function generation
    let i64_type = compiler.context.i64_type();
    let fn_type = i64_type.fn_type(&[i64_type.into(), i64_type.into(), i64_type.into()], false);
    let function = compiler.module.add_function("sum", fn_type, None);
    let basic_block = compiler.context.append_basic_block(function, "entry");

    compiler.builder.position_at_end(basic_block);

    let x = function.get_nth_param(0).unwrap().into_int_value();
    let y = function.get_nth_param(1).unwrap().into_int_value();
    let z = function.get_nth_param(2).unwrap().into_int_value();

    let sum = compiler.builder.build_int_add(x, y, "sum");
    let sum = compiler.builder.build_int_add(sum, z, "sum");

    compiler.builder.build_return(Some(&sum));

    let sum = unsafe { compiler.execution_engine.get_function("sum").ok() };
    let sum: JitFunction<SumFunc> = sum.unwrap();

    let x = 1u64;
    let y = 2u64;
    let z = 3u64;

    unsafe {
        assert_eq!(sum.call(x, y, z), x + y + z);
    }
}

// TODO: write more tests
