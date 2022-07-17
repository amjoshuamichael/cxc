fn square_fn(input: u32) -> u32 {
    input * input
}

fn get_square_fn() -> Fn() {
    square_fn
}

fn main() {
    x = 2
    y = 2 + x
    (get_square_fn())(2)
}

fn main() {
    x = 2
    y = 2 + x(get_square_fn())(2)
}
