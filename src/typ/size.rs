use inkwell::{
    context::Context,
    targets::{CodeModel, RelocMode, Target, TargetMachine},
    OptimizationLevel,
};

use super::Kind;
use crate::Type;
use memoize::memoize;

#[memoize]
pub(super) fn size_of_type(typ: Type) -> u64 {
    let triple = TargetMachine::get_default_triple();
    let target = Target::from_triple(&triple).unwrap();
    let machine = target
        .create_target_machine(
            &triple,
            &*TargetMachine::get_host_cpu_name().to_string(),
            &*TargetMachine::get_host_cpu_features().to_string(),
            OptimizationLevel::None,
            RelocMode::Default,
            CodeModel::Default,
        )
        .unwrap();
    let target_data = machine.get_target_data();
    let context = Context::create();
    target_data
        .get_bit_size(&typ.to_basic_type(&context))
        .div_ceil(8)
}
