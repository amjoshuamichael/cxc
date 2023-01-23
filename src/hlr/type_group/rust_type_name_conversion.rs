use crate::{unit::CompData, Type};
use logos::{Lexer, Logos};

impl<'a> CompData<'a> {
    pub fn type_of<T>(&self, _: &T) -> Type { self.to_type::<T>() }

    pub fn to_type<T>(&self) -> Type {
        let type_name = std::any::type_name::<T>();
        let mut lexer = RustTok::lexer(type_name);
        type_from_tok(&mut lexer)
    }
}

fn type_from_tok(lexer: &mut Lexer<RustTok>) -> Type {
    use RustTok::*;
    let next = lexer.next().unwrap();

    match next {
        Int(size) => Type::i(size),
        Float(size) => match size {
            32 => Type::f32(),
            64 => Type::f64(),
            _ => panic!(),
        },
        Function => {
            assert_eq!(lexer.next().unwrap(), LeftParen);

            let args = if lexer.clone().next().unwrap() == RghtParen {
                Vec::new()
            } else {
                let mut args = Vec::new();

                loop {
                    args.push(type_from_tok(lexer));

                    if lexer.next().unwrap() == RghtParen {
                        break;
                    }
                }

                args
            };

            let ret = if lexer.clone().next() == Some(RetArrow) {
                lexer.next();
                type_from_tok(lexer)
            } else {
                Type::unknown()
            };

            ret.func_with_args(args)
        },
        _ => {
            unreachable!()
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn int() {
        let type_group = CompData::new();
        assert_eq!(type_group.to_type::<i32>(), Type::i(32));
    }

    #[test]
    fn float() {
        let type_group = CompData::new();
        assert_eq!(type_group.to_type::<f64>(), Type::f64());
    }

    #[test]
    fn function() {
        let type_group = CompData::new();
        assert_eq!(
            type_group.to_type::<fn(i32, i64) -> f64>(),
            Type::f64().func_with_args(vec![Type::i(32), Type::i(64)])
        );
    }
}

#[derive(Logos, Debug, PartialEq, Eq, Clone)]
enum RustTok {
    #[regex("i[0-9]*", |t| t.slice()[1..].parse::<u32>().unwrap())]
    Int(u32),

    #[regex("f[0-9]*", |t| t.slice()[1..].parse::<u32>().unwrap())]
    Float(u32),

    #[token("fn")]
    Function,

    #[token("(")]
    LeftParen,

    #[token(")")]
    RghtParen,

    #[token("->")]
    RetArrow,

    #[token(",")]
    Comma,

    #[error]
    Error,

    #[regex(r"(#.*\n)|[ \t\n\f]+", logos::skip)]
    Whitespace,
}
