use crate::lex::Token;
use logos::Lexer;

pub fn parse_int(lex: &mut Lexer<Token>) -> Option<u128> {
    let token_str = lex.slice();
    let token = token_str.chars().filter(|c| *c != '_').collect::<String>();

    if token.len() == 1 {
        return Some(token.parse().unwrap());
    }

    match token.chars().nth(1)? {
        'b' => convert_base(&token[2..], 2),
        'o' => convert_base(&token[2..], 8),
        'x' => convert_base(&token[2..], 16),
        _ => Some(token.parse().unwrap()),
    }
}

pub fn parse_float(lex: &mut Lexer<Token>) -> Option<String> {
    let mut token = lex.slice().to_string();
    token = token.chars().filter(|c| *c != '_').collect();

    if token.chars().nth(0)? == '.' {
        token = format!("0{}", token);
    }

    // TODO: implement scientific notation

    return Some(token);
}

pub fn convert_base(input: &str, base: u128) -> Option<u128> {
    let mut output: u128 = 0;
    let mut base_multiplier = 1;

    for c in input.chars().rev() {
        output += base_multiplier * match_base16(c)?;
        base_multiplier *= base;
    }

    Some(output)
}

fn match_base16(char: char) -> Option<u128> {
    let mut str_dest = [0; 1];

    if let Ok(num) = char.encode_utf8(&mut str_dest).parse() {
        return Some(num);
    }

    match char {
        'a' => Some(10),
        'b' => Some(11),
        'c' => Some(12),
        'd' => Some(13),
        'e' => Some(14),
        'f' => Some(15),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::convert_base;

    #[test]
    fn test_base_conversion() {
        assert_eq!(convert_base("101010", 2).unwrap(), 42);
        assert_eq!(convert_base("52", 8).unwrap(), 42);
        assert_eq!(convert_base("2a", 16).unwrap(), 42);
    }
}
