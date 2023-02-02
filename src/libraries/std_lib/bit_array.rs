use crate::{libraries::Library, ArrayType, CompData, Type, TypeEnum};

pub struct BitArrayLib;

impl Library for BitArrayLib {
    fn add_to_unit(&self, unit: &mut crate::Unit) {
        unit.add_type_level_func("BoolArrayToNum".into(), bool_array_to_num);
        unit.push_script(include_str!("bit_array.cxc")).unwrap();
    }
}

fn bool_array_to_num(input: Vec<Type>, _: &CompData) -> Type {
    assert_eq!(input.len(), 1);
    let TypeEnum::Array(ArrayType { count, .. }) = input[0].as_type_enum() else { return Type::void() };

    match count {
        0..=8 => Type::i(8),
        9..=16 => Type::i(16),
        17..=32 => Type::i(32),
        33..=64 => Type::i(64),
        _ => Type::void(),
    }
}
