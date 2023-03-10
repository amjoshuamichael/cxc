pub trait Externalize<R> {
    type Externalized;

    fn externalize(addr: usize) -> Self::Externalized;
}

macro_rules! impl_externalize {
    ($($t:ident)*) => {
        impl<$($t,)* R> Externalize<R> for ($($t,)*) {
            type Externalized = unsafe extern "C" fn($($t,)*) -> R;

            fn externalize(addr: usize) -> Self::Externalized {
                unsafe { std::mem::transmute(addr) }
            }
        }
    }
}

impl<A, R> Externalize<R> for [A] {
    type Externalized = unsafe extern "C" fn(A) -> R;

    fn externalize(addr: usize) -> Self::Externalized { unsafe { std::mem::transmute(addr) } }
}

impl_externalize!();
impl_externalize!(A B);
impl_externalize!(A B C);
impl_externalize!(A B C D);
impl_externalize!(A B C D E);
impl_externalize!(A B C D E F);
impl_externalize!(A B C D E F G);
impl_externalize!(A B C D E F G H);
impl_externalize!(A B C D E F G H I);
impl_externalize!(A B C D E F G H I J);
