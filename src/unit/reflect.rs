use std::{rc::Rc, sync::Arc, any::TypeId};
#[cfg(feature = "ffi-assertions")]
use crate::errors::TResult;

use crate::{
    lex::lex,
    parse::{self, GenericLabels, TypeDecl, context::FuncParseData},
    CompData, Type, TypeName, Unit, VarName,
};

/// Allows for reflection of a given type, using [`Unit::add_reflect_type`] or
/// [`Unit::get_reflect_type`].
pub trait XcReflect: 'static {
    /// Provide the code that would produce this type, as a string.
    fn spec_code() -> String;

    /// Provide the type declaration for this type.
    fn type_decl() -> TypeDecl {
        let spec_code = Self::spec_code();
        let is_named = spec_code.contains('=');

        let mut lexer = lex(&Self::spec_code());

        if is_named {
            parse::file(&mut lexer)
                .expect(&*format!("error in type decl {spec_code}"))
                .types.iter()
                .next()
                .expect("error in type decl")
                .clone()
        } else {
            let mut spec_lexer = lexer.split(FuncParseData::default(), GenericLabels::default());

            let name = if let Ok(possible_name) = spec_lexer.peek_tok()
                       && let Ok(name) = possible_name.clone().type_name()
                       && matches!(name, TypeName::Other(_))
                       && spec_lexer.peek_by(1).is_err() {
                name
            } else {
                TypeName::Anonymous
            };

            let spec = parse::parse_type_spec(&mut spec_lexer).expect("error in type spec");

            TypeDecl {
                name,
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
                fn spec_code() -> String {
                    stringify!($t).into()
                }
            }
        )*
    };
}

impl_reflect!(u8, u16, u32, u64, u128, usize, i8, i16, i32, i64, i128, isize, f32, f64, bool);

macro_rules! impl_reflect_set {
    ( $( $elem:ident )+ ) => {
        impl<$($elem: XcReflect,)+> XcReflect for ($($elem,)+) {
            fn spec_code() -> String {
                let mut code = String::from("{");
                $(
                    code += &*$elem::spec_code();
                    code += ", ";
                )+
                code += "}";
                code
            }
        }

        impl<$($elem: XcReflect),+, R: XcReflect> XcReflect for fn($($elem),+) -> R {
            fn spec_code() -> String {
                let mut code = String::from("ExternRust((");
                $(
                    code += &*$elem::spec_code();
                    code += ", ";
                )+
                code += "); ";
                code += &*R::spec_code();
                code += "))";
                code
            }
        }

        impl<$($elem: XcReflect),+, R: XcReflect> XcReflect for unsafe extern "C" fn($($elem),+) -> R {
            fn spec_code() -> String {
                let mut code = String::from("(");
                $(
                    code += &*$elem::spec_code();
                    code += ", ";
                )+
                code += "); ";
                code += &*R::spec_code();
                code
            }
        }
    };
}

impl_reflect_set! { A }
impl_reflect_set! { A B }
impl_reflect_set! { A B C }
impl_reflect_set! { A B C D }
impl_reflect_set! { A B C D E }
impl_reflect_set! { A B C D E F }
impl_reflect_set! { A B C D E F G }
impl_reflect_set! { A B C D E F G H }
impl_reflect_set! { A B C D E F G H I }
impl_reflect_set! { A B C D E F G H I J }

macro_rules! impl_reflect_generic {
    ( $arg:ident ) => {
        impl<T: XcReflect> XcReflect for $arg<T> {
            fn spec_code() -> String {
                // TODO: make this "reflected" keyword more important in code
                let mut code = String::from("Reflected = ");
                code += stringify!($arg);
                code += "<";

                if T::type_decl().name == TypeName::Anonymous {
                    code += &*T::spec_code();
                } else {
                    code += &*T::type_decl().name.to_string();
                }

                code += ">";
                code
            }
        }
    };
}

impl_reflect_generic!(Option);
impl_reflect_generic!(Vec);
impl_reflect_generic!(Rc);
impl_reflect_generic!(Arc);

impl<T: XcReflect> XcReflect for &'static [T] {
    fn spec_code() -> String {
        let mut code = String::from("Reflected = { +ptr: &");
        code += &*T::spec_code();
        code += ", +len: u64 }";
        code
    }
}

impl Unit {
    pub fn get_reflect_type<T: XcReflect + 'static>(&self) -> Option<Type> {
        self.comp_data.get_reflect_type::<T>()
    }

    pub fn add_reflect_type<T: XcReflect>(&mut self) -> Option<Type> {
        let decl = T::type_decl();

        self.add_type_with_decl::<T>(decl)
    }

    pub fn add_many_reflect_types(&mut self, type_decls: &[TypeDecl]) {
        for decl in type_decls {
            self.comp_data.add_type_spec(decl.name.clone(), decl.typ.clone());
        }
    }

    pub fn add_type_with_decl<T: XcReflect>(&mut self, decl: TypeDecl) -> Option<Type> {
        self.comp_data.add_type_spec(decl.name.clone(), decl.typ.clone());

        let typ = type_from_decl(&self.comp_data, decl)?;

        #[cfg(feature = "ffi-assertions")]
        {
            Self::do_size_assertion::<T>(&typ, "");

            if let Ok(xc_opt) = self
                .comp_data
                .get_spec(&"Option<T>".into(), &vec![typ.clone()])
            {
                Self::do_size_assertion::<Option<T>>(&xc_opt, "This is likely because the rust version contains a pointer that the cxc version does not, or vise versa. See https://stackoverflow.com/questions/46557608.");
            }        
        }

        self.comp_data.reflected_types.insert(std::any::TypeId::of::<T>(), typ.clone());

        Some(typ)
    }

    #[cfg(feature = "ffi-assertions")]
    fn do_size_assertion<T>(with: &Type, ex_err: &str) {
        assert_eq!(
            std::mem::size_of::<T>(),
            with.size(),
            "Improper reflection. {:?} in cxc has size {},
                and {} in rust has size {}. {}",
            with,
            with.size(),
            std::any::type_name::<T>(),
            std::mem::size_of::<T>(),
            ex_err
        );
    }

    #[cfg(feature = "ffi-assertions")]
    pub fn assert_size_of<T: 'static>(&self) -> TResult<()> {
        let name = std::any::type_name::<T>();
        let last_colon = name.rfind(":").unwrap_or(0);

        self.assert_size_of_with_name::<T>(&name[(last_colon + 1)..])
    }

    #[cfg(feature = "ffi-assertions")]
    pub fn assert_size_of_with_name<T: 'static>(&self, name: &str) -> TResult<()> {
        Self::do_size_assertion::<T>(
            &self.comp_data.get_by_name(&name.into())?, 
            ""
        );

        Ok(())

    }
}

impl CompData {
    pub fn type_of_val<T: XcReflect>(&self, _: &T) -> Type { 
        self.get_reflect_type::<T>().unwrap() 
    }

    pub fn get_reflect_type<T: XcReflect + 'static>(&self) -> Option<Type> {
        if let Some(typ) = self.reflected_types.get(&TypeId::of::<T>()) {
            return Some(typ.clone());
        }

        let decl = T::type_decl();

        if decl.name == "Reflected".into() {
            Some(self.get_spec(&decl.typ, &()).unwrap())
        } else {
            type_from_decl(&self, decl)
        }
    }
}


fn type_from_decl(comp_data: &CompData, decl: TypeDecl) -> Option<Type> {
    if !decl.contains_generics {
        Some(
            comp_data
                .get_spec(&decl.typ, &Vec::new())
                .unwrap()
                .with_name(decl.name)
                .with_generics(&Vec::new()),
        )
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Unit, typ::ABI};

    #[test]
    fn reflect_basic() {
        let unit = Unit::new();

        let typ = unit.get_reflect_type::<i32>().unwrap();
        assert_eq!(typ, Type::i(32));
    }
    

    #[test]
    fn reflect_tuple() {
        let unit = Unit::new();

        let typ = unit.get_reflect_type::<(i32, f32)>().unwrap();
        assert_eq!(typ, Type::new_tuple(vec![Type::i(32), Type::f(32)]));
    }

    #[test]
    fn reflect_func() {
        let unit = Unit::new();

        let typ = unit.get_reflect_type::<fn(i32, f32) -> i32>().unwrap();
        assert_eq!(typ, Type::i(32).func_with_args(vec![Type::i(32), Type::f(32)], ABI::Rust));
    }

    #[test]
    fn reflect_vec() {
        let mut unit = Unit::new();


        unit.push_script("Vec<T> = { len: u64, cap: u64, loc: &T }").unwrap();
        let typ = unit.get_reflect_type::<Vec<u64>>().unwrap();
        assert_eq!(typ, unit.comp_data.get_spec(&"Vec<u64>".into(), &()).unwrap());
    }
}
