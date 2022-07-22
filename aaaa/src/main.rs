struct Point2D {
    x: i32,
    y: i32,
}

fn main() {
    let x = Point2D {
        x: 0b10000000,
        y: 0b10000000,
    };

    let x_ptr: usize = unsafe { std::mem::transmute(&x.x) };
    let y_ptr: usize = unsafe { std::mem::transmute(&x.y) };
    let z = y_ptr - x_ptr;
    return ();
}
