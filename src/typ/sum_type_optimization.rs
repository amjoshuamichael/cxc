use crate::{
    hlr::expr_tree::{MemberGen, NodeDataGen, UnarOpGen},
    parse::Opcode,
};

use super::*;

pub struct FieldsIter {
    over: Type,
    index: usize,
    inner: Option<Box<FieldsIter>>,
}

impl FieldsIter {
    pub fn new(from: Type) -> FieldsIter {
        FieldsIter {
            over: from,
            index: 0,
            inner: None,
        }
    }

    fn build_accessor_nodes(
        &self,
        center: Box<dyn NodeDataGen + 'static>,
    ) -> Box<dyn NodeDataGen> {
        let accessed: Box<dyn NodeDataGen> = match self.over.as_type_enum() {
            TypeEnum::Struct(StructType { fields }) => {
                let (field_name, field_type) = fields[self.index].clone();

                box MemberGen {
                    object: center,
                    field: field_name,
                    ret_type: field_type,
                }
            },
            TypeEnum::Ref(RefType { base }) => box UnarOpGen {
                op: Opcode::Deref(1),
                hs: center,
                ret_type: base.clone(),
            },
            TypeEnum::Variant(_) => unreachable!(),
            TypeEnum::Sum(_) => todo!(),
            TypeEnum::Array(_) => todo!(),
            _ => return center,
        };

        if let Some(inner_iter) = &self.inner {
            inner_iter.build_accessor_nodes(accessed)
        } else {
            accessed
        }
    }
}

impl Iterator for FieldsIter {
    type Item = Type;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(inner) = &mut self.inner {
            if let Some(next) = inner.next() {
                return Some(next);
            }

            self.index += 1;
        }

        let to_return: &Type = match self.over.as_type_enum() {
            TypeEnum::Struct(StructType { fields }) => {
                let iterating_over = &fields.get(self.index)?.1;
                self.inner = Some(box FieldsIter::new(iterating_over.clone()));
                iterating_over
            },
            TypeEnum::Ref(RefType { base }) => {
                if self.index > 0 {
                    return None;
                };
                self.inner = Some(box FieldsIter::new(base.clone()));
                base
            },
            TypeEnum::Variant(_) => unreachable!(),
            TypeEnum::Sum(SumType { variants }) => {
                if self.index > variants.len() {
                    return None;
                }
                &variants[self.index].1
            },
            TypeEnum::Array(ArrayType { base, count }) => {
                if self.index as u32 > *count {
                    return None;
                }
                self.inner = Some(box FieldsIter::new(base.clone()));
                base
            },
            _ => return None,
        };

        return Some(to_return.clone());
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_struct() {
        let mut iter = FieldsIter::new(Type::new(TypeEnum::Struct(StructType {
            fields: vec![("a".into(), Type::i(32)), ("b".into(), Type::f32())],
        })));

        assert_eq!(iter.next(), Some(Type::i(32)));
        assert_eq!(iter.next(), Some(Type::f32()));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_ref() {
        let mut iter = FieldsIter::new(
            Type::new(TypeEnum::Struct(StructType {
                fields: vec![
                    ("a".into(), Type::i(32).get_ref()),
                    ("b".into(), Type::f32()),
                ],
            }))
            .get_ref(),
        );

        assert!(matches!(iter.next().unwrap().as_type_enum(), TypeEnum::Struct(_)));
        assert_eq!(iter.next(), Some(Type::i(32).get_ref()));
        assert_eq!(iter.next(), Some(Type::i(32)));
        assert_eq!(iter.next(), Some(Type::f32()));
        assert_eq!(iter.next(), None);
    }
}
