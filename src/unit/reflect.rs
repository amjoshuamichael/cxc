use crate::{
    lex::lex,
    parse::{self, GenericLabels, TypeDecl},
    CompData, Type, TypeName, Unit, VarName,
};

pub trait XcReflect {
    fn alias_code() -> String;

    fn type_decl() -> TypeDecl {
        let alias_code = Self::alias_code();
        let is_named = alias_code.contains("=");

        let mut lexer = lex(&*Self::alias_code());

        if is_named {
            parse::file(&mut lexer)
                .expect("error in type decl")
                .types_iter()
                .next()
                .expect("error in type decl")
                .clone()
        } else {
            let mut spec_lexer = lexer.split(VarName::temp(), GenericLabels::default());
            let spec = parse::parse_type_spec(&mut spec_lexer).expect("error in type spec");

            TypeDecl {
                name: TypeName::Anonymous,
                typ: spec,
                contains_generics: false,
            }
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

impl_reflect!(u8, u16, u32, u64, u128, usize, i8, i16, i32, i64, i128, isize, f32, f64, bool);

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

macro_rules! impl_reflect_func {
    ( $( $arg:ident )+ ; $ret:ident ) => {
        impl<$($arg: XcReflect),+, $ret: XcReflect> XcReflect for fn($($arg),+) -> $ret {
            fn alias_code() -> String {
                let mut code = String::from("(");
                $(
                    code += &*$arg::alias_code();
                    code += ", ";
                )+
                code += "); ";
                code += &*$ret::alias_code();
                code
            }
        }
    };
}

impl_reflect_func! { A B; R }
impl_reflect_func! { A B C; R }
impl_reflect_func! { A B C D; R }
impl_reflect_func! { A B C D E; R }
impl_reflect_func! { A B C D E F; R }
impl_reflect_func! { A B C D E F G; R }
impl_reflect_func! { A B C D E F G H; R }
impl_reflect_func! { A B C D E F G H I; R }
impl_reflect_func! { A B C D E F G H I J; R }

impl Unit {
    pub fn get_reflect_type<T: XcReflect>(&self) -> Option<Type> {
        let decl = T::type_decl();
        type_from_decl(&*self.comp_data, decl)
    }

    pub fn add_reflect_type<T: XcReflect>(&mut self) -> Option<Type> {
        let decl = T::type_decl();

        {
            let comp_data = self.comp_data_mut();
            comp_data.add_type_alias(decl.name.clone(), decl.typ.clone());
        }

        type_from_decl(&*self.comp_data, decl)
    }
}

fn type_from_decl(comp_data: &CompData, decl: TypeDecl) -> Option<Type> {
    if !decl.contains_generics {
        Some(
            comp_data
                .get_spec(&decl.typ, &Vec::new())
                .unwrap()
                .with_name(decl.name),
        )
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Unit;

    #[test]
    fn reflect_basic() {
        let mut unit = Unit::new();

        let typ = unit.add_reflect_type::<i32>().unwrap();
        assert_eq!(typ, Type::i(32));
    }

    #[test]
    fn reflect_tuple() {
        let mut unit = Unit::new();

        let typ = unit.add_reflect_type::<(i32, f32)>().unwrap();
        assert_eq!(typ, Type::new_tuple(vec![Type::i(32), Type::f(32)]));
    }

    #[test]
    fn reflect_func() {
        let mut unit = Unit::new();

        let typ = unit.add_reflect_type::<fn(i32, f32) -> i32>().unwrap();
        assert_eq!(typ, Type::i(32).func_with_args(vec![Type::i(32), Type::f(32)]));
    }
}
