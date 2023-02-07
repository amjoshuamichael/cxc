use std::fmt::Debug;
use std::ops::{BitAnd, BitOrAssign};

use crate::{libraries::Library, CompData, Type};

pub struct BitArrayLib;

impl Library for BitArrayLib {
    fn add_to_unit(&self, unit: &mut crate::Unit) {
        unit.add_type_level_func("NumOfSize".into(), num_of_size);
        unit.push_script(include_str!("bit_array.cxc")).unwrap();
    }
}

fn num_of_size(input: Vec<Type>, _: &CompData) -> Type {
    assert_eq!(input.len(), 1);

    match input[0].size() {
        0..=8 => Type::i(8),
        9..=16 => Type::i(16),
        17..=32 => Type::i(32),
        33..=64 => Type::i(64),
        _ => Type::void(),
    }
}

pub struct BitArray<T: Copy> {
    num: T,
}

impl<T: BitAnd<u32> + BitOrAssign<u32> + Copy> BitArray<T>
where
    <T as BitAnd<u32>>::Output: PartialOrd<u32>,
{
    pub fn get(&self, index: u32) -> bool { return (self.num & (1 << index)) > 0 }
    pub fn set(&mut self, index: u32, to: bool) { self.num |= (to as u32) << index }
    pub fn new() -> BitArray<u32> { BitArray::<u32> { num: 0 } }
}

impl<T: Copy + std::fmt::Binary> Debug for BitArray<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:b}", self.num)
    }
}
