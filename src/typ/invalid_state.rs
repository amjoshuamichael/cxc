use crate::hlr::expr_tree::{ArrayLitGen, NodeDataGen, StructLitGen};
use crate::parse::InitOpts;
use crate::{hlr::expr_tree::HNodeData, BoolType, FloatType, FuncType, IntType, RefType, Type};
use crate::{ArrayType, Repr, StructType, TypeEnum};

use super::{SumType, UnknownType, VariantType, VoidType};

pub trait InvalidState {
    fn invalid_state(&self, index: u32) -> Option<Box<dyn NodeDataGen>>;
}

impl InvalidState for Type {
    fn invalid_state(&self, index: u32) -> Option<Box<dyn NodeDataGen>> {
        self.as_type_enum().invalid_state(index)
    }
}

impl InvalidState for RefType {
    fn invalid_state(&self, index: u32) -> Option<Box<dyn NodeDataGen>> {
        if index == 0 {
            Some(Box::new(HNodeData::Number {
                value: 0,
                lit_type: Type::i(64),
            }))
        } else {
            None
        }
    }
}

impl InvalidState for BoolType {
    fn invalid_state(&self, index: u32) -> Option<Box<dyn NodeDataGen>> {
        if index + 2 < 256 {
            Some(Box::new(HNodeData::Number {
                value: (index + 2) as u64,
                lit_type: Type::i(8),
            }))
        } else {
            None
        }
    }
}

impl InvalidState for IntType {
    fn invalid_state(&self, _: u32) -> Option<Box<dyn NodeDataGen>> {
        if self.size % 8 != 0 {
            todo!()
        } else {
            None
        }
    }
}

impl InvalidState for FloatType {
    fn invalid_state(&self, _: u32) -> Option<Box<dyn NodeDataGen>> { None }
}

impl InvalidState for FuncType {
    fn invalid_state(&self, index: u32) -> Option<Box<dyn NodeDataGen>> {
        if index == 0 {
            Some(Box::new(HNodeData::Number {
                value: 0,
                lit_type: Type::i(64),
            }))
        } else {
            None
        }
    }
}

impl InvalidState for ArrayType {
    fn invalid_state(&self, index: u32) -> Option<Box<dyn NodeDataGen>> {
        let elem_invalid_state = self.base.invalid_state(index)?;

        Some(Box::new(ArrayLitGen {
            var_type: Type::new(TypeEnum::Array(self.clone())),
            parts: vec![elem_invalid_state],
            initialize: InitOpts::Uninit,
        }))
    }
}

impl InvalidState for StructType {
    fn invalid_state(&self, index: u32) -> Option<Box<dyn NodeDataGen>> {
        if self.repr == Repr::Transparent {
            return None;
        }

        for (field_name, field_type) in &self.fields {
            if field_type.invalid_state(0).is_some() {
                return Some(Box::new(StructLitGen {
                    var_type: Type::new(TypeEnum::Struct(self.clone())),
                    fields: vec![(field_name.clone(), field_type.invalid_state(index)?)],
                    initialize: InitOpts::Uninit,
                }));
            }
        }

        None
    }
}

impl InvalidState for SumType {
    fn invalid_state(&self, index: u32) -> Option<Box<dyn NodeDataGen>> {
        let variant_count = self.variants.len() as u32;

        if self.has_internal_discriminant() {
            self.largest_variant_data()
                .invalid_state(index + variant_count - 1)
        } else if variant_count + index < 256 {
            Some(Box::new(StructLitGen {
                var_type: self.largest_variant_as_struct(),
                fields: vec![(
                    "tag".into(),
                    Box::new(HNodeData::Number {
                        value: (variant_count + index) as u64,
                        lit_type: Type::i(8),
                    }),
                )],
                initialize: InitOpts::Uninit,
            }))
        } else {
            None
        }
    }
}

impl SumType {
    pub fn tag_data(&self, index: u32) -> Box<dyn NodeDataGen> {
        self.largest_variant_data().invalid_state(index).unwrap()
    }
}

impl InvalidState for VariantType {
    fn invalid_state(&self, _: u32) -> Option<Box<dyn NodeDataGen>> { unreachable!() }
}

impl InvalidState for UnknownType {
    fn invalid_state(&self, _: u32) -> Option<Box<dyn NodeDataGen>> { unreachable!() }
}

impl InvalidState for VoidType {
    fn invalid_state(&self, _: u32) -> Option<Box<dyn NodeDataGen>> { None }
}
