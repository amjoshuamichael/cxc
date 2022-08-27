use crate::lex::Tok;
use logos::Lexer;

pub fn parse_int(token: &mut Lexer<Tok>) -> Option<u128> {
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

pub fn parse_float(token: &mut Lexer<Tok>) -> Option<f64> {
    let num_text = token
        .slice()
        .chars()
        .filter(|c| *c != '_' && *c != '+')
        .collect::<String>();

    match num_text.chars().position(|c| c == 'e') {
        None => num_text.parse().ok(),
        Some(e_pos) => parse_scientific_notation(num_text, e_pos),
    }
}

fn parse_scientific_notation(num_text: String, e_pos: usize) -> Option<f64> {
    let coeff_str = &num_text[..e_pos];
    let exp_str = &num_text[e_pos + 1..];

    let without_decimal =
        coeff_str.chars().filter(|c| *c != '.').collect::<String>();
    let decimal_pos = coeff_str.chars().rev().position(|c| c == '.').unwrap();

    let coefficient: f64 = without_decimal.parse::<f64>().ok()?;
    let exponent: f64 = exp_str.parse::<f64>().ok()? - decimal_pos as f64;
    let base: f64 = 10.0;

    let output = coefficient * base.powf(exponent);

    return Some(output);
}

fn convert_base(input: &str, base: u32) -> Option<u128> {
    let mut output: u128 = 0;

    for (digit_index, digit) in input.chars().rev().enumerate() {
        let digit_index: u32 = digit_index.try_into().unwrap();
        let base_multiplier: u128 = base.pow(digit_index).try_into().ok()?;
        output += match_base16(digit)? * base_multiplier;
    }

    Some(output)
}

fn match_base16(digit: char) -> Option<u128> { Some(digit.to_digit(16)?.into()) }
