use inkwell::context::Context;

use super::Kind;
use crate::Type;
use memoize::memoize;

#[memoize]
pub(super) fn size_of_type(typ: Type) -> usize {
    let context = Context::create();
    let size_int_value = typ.to_any_type(&context).size_of().unwrap();

    // Calculating the size of a type depends on many things.
    // Instead of guesstimating it, we just ask LLVM to write a program
    // that calculates it for us, and then compile and run that
    // program.

    let context = Context::create();
    let module = context.create_module("");
    let builder = context.create_builder();
    let func_type = size_int_value.get_type().fn_type(&[], false);
    let function = module.add_function("".into(), func_type, None);
    let basic_block = context.append_basic_block(function, "");
    builder.position_at_end(basic_block);
    builder.build_return(Some(&size_int_value));
    let execution_engine = module
        .create_jit_execution_engine(inkwell::OptimizationLevel::None)
        .unwrap();
    let val = unsafe { execution_engine.run_function(function, &[]) };
    val.as_int(false) as usize
}
