use crate::parse::ParseErrorSpanned;

pub type CompResultMany<T> = Result<T, Vec<CompError>>;

#[derive(Debug)]
pub enum CompError {
    Parse(ParseErrorSpanned),
}
