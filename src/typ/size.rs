use inkwell::{
    context::Context,
    targets::{CodeModel, RelocMode, Target, TargetData, TargetMachine},
    OptimizationLevel,
};

use super::Kind;
use crate::{unit::make_context, Type};
use std::cell::RefCell;
use std::collections::btree_map::BTreeMap;

std::thread_local! {
  static MEMOIZED_SIZES: RefCell<BTreeMap<Type, u64>> = RefCell::new(BTreeMap::new());
  static TARGETS: RefCell<Option<(&'static Context, TargetData)>> = RefCell::new(None);
}

pub(super) fn size_of_type(typ: Type) -> u64 {
    let cached_size = MEMOIZED_SIZES.with(|memoized| {
        let memoized = memoized.borrow_mut();
        memoized.get(&typ).copied()
    });

    if let Some(size) = cached_size {
        return size;
    }

    TARGETS.with(|opt| {
        let opt_borrow = opt.borrow();
        if let Some((context, target_data)) = opt_borrow.as_ref() {
            target_data
                .get_bit_size(&typ.to_basic_type(&context))
                .div_ceil(8)
        } else {
            std::mem::drop(opt_borrow);

            let triple = TargetMachine::get_default_triple();
            let target = Target::from_triple(&triple).unwrap();
            let machine = target
                .create_target_machine(
                    &triple,
                    &TargetMachine::get_host_cpu_name().to_string(),
                    &TargetMachine::get_host_cpu_features().to_string(),
                    OptimizationLevel::None,
                    RelocMode::Default,
                    CodeModel::Default,
                )
                .unwrap();
            let target_data = machine.get_target_data();
            let context = make_context();

            let size = target_data
                .get_bit_size(&typ.to_basic_type(&context))
                .div_ceil(8);

            *opt.borrow_mut() = Some((context, target_data));

            size
        }
    })
}
