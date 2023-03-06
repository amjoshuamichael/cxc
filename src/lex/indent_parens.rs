/// Takes in an input like `(x = (1 + 2))` and converts it to:
/// (
///     x = (
///         1 + 2
///     )
/// )
pub fn indent_parens(input: String) -> String {
    let mut indentation_level = 0;
    let mut output = String::new();
    let mut is_start_of_line = true;

    for c in input.chars() {
        match c {
            '{' => {
                indentation_level += 1;

                output += "{\n";
                output += &*"  ".repeat(indentation_level);

                is_start_of_line = true;
            },
            '}' => {
                indentation_level -= 1;

                output += "\n";
                output += &*"  ".repeat(indentation_level);
                output += "}";

                is_start_of_line = true;
            },
            _ => {
                if is_start_of_line && c == ' ' {
                    continue;
                }

                is_start_of_line = false;
                output += &*c.to_string();

                if c == '\n' {
                    output += &*"  ".repeat(indentation_level);
                }
            },
        }
    }

    output
}
