use std::marker::PhantomData;

#[derive(Copy, Clone, Default)]
pub struct IdGenerator<T: From<u32>> {
    next: u32,
    _phantom: PhantomData<T>,
}

impl<T: From<u32>> IdGenerator<T> {
    pub fn new(&mut self) -> T {
        let output = T::from(self.next);
        self.next += 1;
        output
    }
}

macro_rules! make_id {
    ($type:ident) => {
        #[derive(Copy, Clone, Default, Eq, Ord, PartialEq, PartialOrd)]
        pub struct $type(u32);

        impl From<u32> for $type {
            fn from(val: u32) -> $type {
                $type(val)
            }
        }
    }
}

make_id!(FuncId);
