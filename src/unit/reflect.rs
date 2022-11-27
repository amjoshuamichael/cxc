use std::rc::Rc;

use crate::{
    lex::{lex, Tok},
    parse::{parse_generics, parse_type_decl, TokenStream, TypeDecl},
    Type, Unit,
};

pub trait XcReflect {
    fn alias_code<'a>() -> &'a str;

    fn type_decl() -> TypeDecl {
        let mut lexer = lex(Self::alias_code());
        let Tok::TypeName(type_name) = lexer.next_tok().unwrap() else { panic!() };
        let generics =
            parse_generics(&mut lexer).expect("failure to parse generics");
        let parser = lexer.split(type_name, generics);
        parse_type_decl(parser)
            .expect("failure to parse type alias")
            .0
    }
}

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
            comp_data.add_type_alias(decl.name, decl.typ.clone());
        }

        if !decl.contains_generics {
            self.comp_data.get_spec(&decl.typ, &Vec::new())
        } else {
            None
        }
    }
}
