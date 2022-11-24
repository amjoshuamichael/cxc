use std::collections::HashMap;

use crate::{
    lex::{lex, Tok},
    parse::{parse_type_decl, TokenStream, TypeDecl},
    Type, Unit,
};

pub trait XcReflect {
    fn alias_code<'a>() -> &'a str;

    fn type_decl() -> TypeDecl {
        let mut lexer = lex(Self::alias_code());
        let Tok::TypeName(type_name) = lexer.next_tok().unwrap() else { panic!() };
        let parser = lexer.split(type_name, HashMap::new());
        parse_type_decl(parser)
            .expect("failure to parse type alias")
            .0
    }
}

impl<'u> Unit<'u> {
    pub fn get_reflected_type<T: XcReflect>(&self) -> Type {
        let type_decl = T::type_decl();

        self.comp_data
            .get_spec(&type_decl.typ, &Vec::new())
            .expect("failure to get reflected type")
            .with_name(type_decl.name)
    }
}
