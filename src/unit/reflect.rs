use std::rc::Rc;

use crate::{
    lex::lex,
    parse::{self, TypeDecl},
    Type, Unit,
};

pub trait XcReflect {
    fn alias_code<'a>() -> &'a str;

    fn type_decl() -> TypeDecl {
        let lexer = lex(Self::alias_code());
        let parsed = match parse::file(lexer) {
            Ok(file) => file,
            Err(err) => {
                println!(
                    "found error in reflection of {}",
                    std::any::type_name::<Self>()
                );
                println!("{}", Self::alias_code());
                dbg!(&err);
                panic!();
            },
        };

        assert_eq!(
            parsed.decl_count(),
            1,
            "you can only create one type in the reflection!"
        );

        let type_decl = parsed
            .types_iter()
            .next()
            .expect("cannot declare function in type reflection!");

        type_decl.clone()
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
