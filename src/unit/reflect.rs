use std::rc::Rc;

use crate::{
    lex::lex,
    parse::{self, TypeDecl},
    Type, TypeName, Unit, VarName,
};

pub trait XcReflect {
    fn alias_code() -> String;

    fn type_decl() -> TypeDecl {
        let alias_code = Self::alias_code();
        let anonymous = !alias_code.contains("=");

        let mut lexer = lex(&*Self::alias_code());

        if anonymous {
            let mut spec_lexer = lexer.split(VarName::default(), Default::default());
            let spec = parse::parse_type_spec(&mut spec_lexer).expect("error in type spec");
            TypeDecl {
                name: TypeName::Anonymous,
                typ: spec,
                contains_generics: false,
                dependencies: spec_lexer.return_info().1,
            }
        } else {
            parse::file(lexer)
                .expect("error in type decl")
                .types_iter()
                .next()
                .expect("error in type decl")
                .clone()
        }
    }
}

macro_rules! impl_reflect {
    ($($t:ty),*) => {
        $(
            impl XcReflect for $t {
                fn alias_code() -> String {
                    stringify!($t).into()
                }
            }
        )*
    };
}

impl_reflect!(
    u8,
    u16,
    u32,
    u64,
    u128,
    usize,
    i8,
    i16,
    i32,
    i64,
    i128,
    isize,
    f32,
    f64,
    bool,
    char,
    String,
    ()
);

macro_rules! impl_reflect_tuple {
    ( $( $elem:ident )+ ) => {
        impl<$($elem: XcReflect),+> XcReflect for ($($elem),+) {
            fn alias_code() -> String {
                let mut code = String::from("{");
                $(
                    code += &*$elem::alias_code();
                    code += ", ";
                )+
                code += "}";
                code
            }
        }
    };
}

impl_reflect_tuple! { A B }
impl_reflect_tuple! { A B C }
impl_reflect_tuple! { A B C D }
impl_reflect_tuple! { A B C D E }
impl_reflect_tuple! { A B C D E F }
impl_reflect_tuple! { A B C D E F G }
impl_reflect_tuple! { A B C D E F G H }
impl_reflect_tuple! { A B C D E F G H I }
impl_reflect_tuple! { A B C D E F G H I J }

macro_rules! impl_reflect_tuple {
    ( $( $arg:ident )+ ; $ret:ident ) => {
        impl<$($arg: XcReflect),+, $ret: XcReflect> XcReflect for fn($($arg),+) -> $ret {
            fn alias_code() -> String {
                let mut code = String::from("(");
                $(
                    code += &*$arg::alias_code();
                    code += ", ";
                )+
                code += ") -> ";
                code += &*$ret::alias_code();
                code
            }
        }
    };
}

impl_reflect_tuple! { A B; R }
impl_reflect_tuple! { A B C; R }
impl_reflect_tuple! { A B C D; R }
impl_reflect_tuple! { A B C D E; R }
impl_reflect_tuple! { A B C D E F; R }
impl_reflect_tuple! { A B C D E F G; R }
impl_reflect_tuple! { A B C D E F G H; R }
impl_reflect_tuple! { A B C D E F G H I; R }
impl_reflect_tuple! { A B C D E F G H I J; R }

impl<'u> Unit<'u> {
    pub fn get_reflect_type<T: XcReflect>(&self) -> Option<Type> {
        let decl = T::type_decl();

        if !decl.contains_generics {
            Some(
                self.comp_data
                    .get_spec(&decl.typ, &Vec::new())
                    .expect("failure to get reflected type")
                    .with_name(decl.name),
            )
        } else {
            None
        }
    }

    pub fn add_reflect_type<T: XcReflect>(&mut self) -> Option<Type> {
        let decl = T::type_decl();

        {
            let comp_data = Rc::get_mut(&mut self.comp_data).unwrap();
            comp_data.add_type_alias(decl.name.clone(), decl.typ.clone());
        }

        if !decl.contains_generics {
            Some(
                self.comp_data
                    .get_spec(&decl.typ, &Vec::new())
                    .unwrap()
                    .with_name(decl.name),
            )
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{LLVMContext, Unit};

    #[test]
    fn reflect_basic() {
        let context = LLVMContext::new();
        let mut unit = Unit::new(&context);

        let typ = unit.add_reflect_type::<i32>().unwrap();
        assert_eq!(typ, Type::i(32));
    }

    #[test]
    fn reflect_tuple() {
        let context = LLVMContext::new();
        let mut unit = Unit::new(&context);

        let typ = unit.add_reflect_type::<(i32, f32)>().unwrap();
        assert_eq!(typ, Type::new_tuple(vec![Type::i(32), Type::f(32)]));
    }

    #[test]
    fn reflect_func() {
        let context = LLVMContext::new();
        let mut unit = Unit::new(&context);

        let typ = unit.add_reflect_type::<fn(i32, f32) -> i32>().unwrap();
        assert_eq!(typ, Type::i(32).func_with_args(vec![Type::i(32), Type::f(32)]));
    }
}
