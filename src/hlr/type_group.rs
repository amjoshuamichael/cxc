use super::prelude::*;
use crate::parse::*;
use std::collections::HashMap;
use std::sync::Arc;

#[derive(Debug, Default, Clone)]
pub struct TypeGroup(pub HashMap<String, TypeEnum>);

impl TypeGroup {
    pub fn add(&mut self, name: String, t: TypeEnum) {
        self.0.insert(name, t);
    }

    pub fn get_base(&self, name: &String) -> Option<TypeEnum> {
        for t in &self.0 {
            if t.0 == name {
                return Some(t.1.clone());
            }
        }

        None
    }

    pub fn get_spec(&self, type_spec: &TypeSpec) -> Option<TypeEnum> {
        dbg!(type_spec);
        let first_char = type_spec.name.chars().next();

        match first_char {
            Some('i') | Some('u') | Some('f') => {
                if type_spec.name.chars().skip(1).all(|c| c.is_digit(10)) {
                    // TypeSpec is accessing a primitive value
                    let bit_width: u32 = type_spec
                        .name
                        .chars()
                        .skip(1)
                        .collect::<String>()
                        .parse()
                        .unwrap();

                    return match first_char {
                        Some('u') | Some('i') => {
                            Some(TypeEnum::int_of_size(bit_width))
                        },
                        Some('f') => Some(TypeEnum::float_of_size(bit_width)),
                        _ => unreachable!(),
                    };
                }
            },
            _ => {},
        }

        Some(
            self.get_base(&type_spec.name)?
                .ref_x_times(type_spec.ref_count),
        )
    }

    pub fn add_types(&mut self, rhs: &TypeGroup) {
        for t in &rhs.0 {
            if self.get_base(&t.0).is_none() {
                self.add(t.0.clone(), t.1.clone())
            }
        }
    }
}
