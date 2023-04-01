use std::ops::DerefMut;

use super::*;

pub struct PrimitiveFieldsIter {
    inner: FieldsIter,
    skip_parent_on_next: bool,
}

impl PrimitiveFieldsIter {
    pub fn new(from: Type) -> Self {
        Self {
            inner: FieldsIter::new(from),
            skip_parent_on_next: false,
        }
    }
}

impl Deref for PrimitiveFieldsIter {
    type Target = FieldsIter;
    fn deref(&self) -> &Self::Target { &self.inner }
}

impl DerefMut for PrimitiveFieldsIter {
    fn deref_mut(&mut self) -> &mut Self::Target { &mut self.inner }
}

impl Iterator for PrimitiveFieldsIter {
    type Item = Type;

    fn next(&mut self) -> Option<Self::Item> {
        if self.skip_parent_on_next {
            self.skip_parent_of_last();
        }

        loop {
            let next_type = self.inner.next()?;
            match next_type.as_type_enum() {
                TypeEnum::Array(_) | TypeEnum::Struct(_) => {},
                TypeEnum::Ref(_) => {
                    self.skip_parent_on_next = true;
                    return Some(next_type);
                },
                _ => return Some(next_type),
            }
        }
    }
}

#[derive(Debug)]
pub struct FieldsIter {
    over: Type,
    index: usize,
    inner: Option<Box<FieldsIter>>,
    has_iterated_at_least_once: bool,
}

impl FieldsIter {
    pub fn new(from: Type) -> FieldsIter {
        FieldsIter {
            over: from,
            index: 0,
            inner: None,
            has_iterated_at_least_once: false,
        }
    }

    pub fn skip_parent_of_last(&mut self) {
        if let Some(inner) = &mut self.inner {
            if inner.has_iterated_at_least_once {
                inner.skip_parent_of_last();
                return;
            }
        }

        self.inner = None;
        self.index += 1;
    }

    pub fn skip_children_of_last(&mut self) {
        if let Some(inner) = &mut self.inner {
            if inner.has_iterated_at_least_once {
                inner.skip_children_of_last();
                return;
            }
        }

        self.inner = None;
    }
}

impl Iterator for FieldsIter {
    type Item = Type;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(inner) = &mut self.inner && let Some(next) = inner.next() {
            return Some(next);
        }

        if self.has_iterated_at_least_once {
            self.index += 1;
        } else {
            self.has_iterated_at_least_once = true;
        }

        let to_return: &Type = match self.over.as_type_enum() {
            TypeEnum::Struct(StructType { fields, .. }) => {
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
            TypeEnum::Array(ArrayType { base, count }) => {
                if self.index as u32 > *count {
                    return None;
                }
                self.inner = Some(box FieldsIter::new(base.clone()));
                base
            },
            _ => return None,
        };

        Some(to_return.clone())
    }
}

impl Type {
    pub fn fields_iter(&self) -> FieldsIter { FieldsIter::new(self.clone()) }
    pub fn primitive_fields_iter(&self) -> PrimitiveFieldsIter { PrimitiveFieldsIter::new(self.clone()) }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fields_iter_struct() {
        let mut iter = FieldsIter::new(Type::new_tuple(vec![Type::i(32), Type::f32()]));

        assert_eq!(iter.next(), Some(Type::i(32)));
        assert_eq!(iter.next(), Some(Type::f32()));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn fields_iter_ref() {
        let mut iter = FieldsIter::new(
            Type::new(TypeEnum::Struct(StructType {
                fields: vec![
                    ("a".into(), Type::i(32).get_ref()),
                    ("b".into(), Type::f32()),
                ],
                ..Default::default()
            }))
            .get_ref(),
        );

        assert!(matches!(iter.next().unwrap().as_type_enum(), TypeEnum::Struct(_)));
        assert_eq!(iter.next(), Some(Type::i(32).get_ref()));
        assert_eq!(iter.next(), Some(Type::i(32)));
        assert_eq!(iter.next(), Some(Type::f32()));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn fields_iter_skip_parent_of_last() {
        let mut iter = FieldsIter::new(Type::new_tuple(vec![
            Type::i(64),
            Type::new_tuple(vec![Type::bool(), Type::i(32).get_ref()]),
            Type::f32(),
        ]));

        assert_eq!(iter.next(), Some(Type::i(64)));
        assert_eq!(
            iter.next(),
            Some(Type::new_tuple(vec![Type::bool(), Type::i(32).get_ref()]))
        );
        assert_eq!(iter.next(), Some(Type::bool()));
        iter.skip_parent_of_last();
        assert_eq!(iter.next(), Some(Type::f32()));
    }

    #[test]
    fn fields_iter_skip_children_of_last() {
        let mut iter = FieldsIter::new(Type::new_tuple(vec![
            Type::i(64),
            Type::new_tuple(vec![Type::bool(), Type::i(32).get_ref()]),
            Type::f32(),
            Type::new_tuple(vec![Type::bool(), Type::i(16)]),
        ]));

        assert_eq!(iter.next(), Some(Type::i(64)));
        assert_eq!(
            iter.next(),
            Some(Type::new_tuple(vec![Type::bool(), Type::i(32).get_ref()]))
        );
        iter.skip_children_of_last();
        assert_eq!(iter.next(), Some(Type::f32()));
        assert_eq!(iter.next(), Some(Type::new_tuple(vec![Type::bool(), Type::i(16)])));
        assert_eq!(iter.next(), Some(Type::bool()));
        iter.skip_children_of_last();
        assert_eq!(iter.next(), Some(Type::i(16)));
    }

    #[test]
    fn fields_iter_primitive_collect() {
        let mut iter = PrimitiveFieldsIter::new(Type::new_tuple(vec![
            Type::i(64),
            Type::new_tuple(vec![Type::bool(), Type::i(32).get_ref()]),
            Type::f32(),
        ]));

        assert_eq!(iter.next(), Some(Type::i(64)));
        assert_eq!(iter.next(), Some(Type::bool()));
        assert_eq!(iter.next(), Some(Type::i(32).get_ref()));
        assert_eq!(iter.next(), Some(Type::f32()));
        assert_eq!(iter.next(), None);
    }
}
