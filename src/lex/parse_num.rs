use std::sync::Arc;

use crate::{lex::Tok, parse::ParsedFloat};
use logos::Lexer;

pub fn parse_int(token: &mut Lexer<Tok>) -> Option<u64> {
    let plain_number = token
        .slice()
        .chars()
        .filter(|c| *c != '_')
        .collect::<String>();

    if plain_number.len() == 1 {
        return plain_number.parse().ok();
    }

    match &plain_number[..2] {
        "0b" => convert_base(&plain_number[2..], 2),
        "0o" => convert_base(&plain_number[2..], 8),
        "0x" => convert_base(&plain_number[2..], 16),
        _ => plain_number.parse().ok(),
    }
}

pub fn parse_bool(token: &mut Lexer<Tok>) -> Option<bool> {
    let true_or_false: String = token.slice().chars().collect();
    match &*true_or_false {
        "true" => Some(true),
        "false" => Some(false),
        _ => None,
    }
}

pub fn parse_string(token: &mut Lexer<Tok>) -> Option<Arc<str>> {
    let string_len = token.slice().len() - 1;
    Some(Arc::from(&token.slice()[1..string_len]))
}

pub fn parse_dotted_int(token: &mut Lexer<Tok>) -> Option<(u128, u128)> {
    let num_text = token
        .slice()
        .chars()
        .filter(|c| *c != '_' && *c != '+')
        .collect::<String>();

    let (left, right) = if num_text.chars().next()? == '.' {
        ("0", &num_text[1..])
    } else {
        let mut split = num_text.split('.');
        (split.next()?, split.next()?)
    };

    Some((left.parse().ok()?, right.parse().ok()?))
}

pub fn parse_float(token: &mut Lexer<Tok>) -> Option<ParsedFloat> {
    let num_text = token
        .slice()
        .chars()
        .filter(|c| *c != '_' && *c != '+')
        .collect::<String>();


    match num_text.chars().position(|c| c == 'e') {
        None => {
            let dot_loc = num_text.chars().position(|c| c == '.').unwrap();
            Some(ParsedFloat {
                l: num_text[0..dot_loc].parse().ok()?,
                r: num_text[(dot_loc + 1)..].parse().ok()?,
                exp: None,
            })
        },
        Some(e_pos) => parse_scientific_notation(num_text, e_pos),
    }
}

fn parse_scientific_notation(num_text: String, e_pos: usize) -> Option<ParsedFloat> {
    let coeff_str = &num_text[..e_pos];
    let exp_str = &num_text[e_pos + 1..];

    let decimal_pos = coeff_str.chars().rev().position(|c| c == '.').unwrap();

    Some(ParsedFloat {
        l: num_text[0..decimal_pos].parse::<u128>().ok()?,
        r: num_text[(decimal_pos + 1)..e_pos].parse::<u128>().ok()?,
        exp: Some(num_text[(e_pos + 1)..].parse::<i128>().ok()?),
    })
}

fn convert_base(input: &str, base: u32) -> Option<u64> {
    let mut output: u64 = 0;

    for (digit_index, digit) in input.chars().rev().enumerate() {
        let digit_index: u32 = digit_index.try_into().unwrap();
        let base_multiplier: u64 = base.pow(digit_index).try_into().ok()?;
        output += match_base16(digit)? * base_multiplier;
    }

    Some(output)
}

fn match_base16(digit: char) -> Option<u64> { Some(digit.to_digit(16)?.into()) }
