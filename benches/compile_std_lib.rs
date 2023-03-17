#![feature(test)]

extern crate test;

use cxc::{library::StdLib, Unit};
use test::{black_box, Bencher};

#[bench]
pub fn compile_std_lib(b: &mut Bencher) {
    b.iter(|| {
        for _ in 0..10000 {
            let mut unit = black_box(Unit::new());
            black_box(unit.add_lib(StdLib));
        }
    });
}

#[bench]
pub fn hello_world(b: &mut Bencher) {
    b.iter(|| {
        let mut unit = black_box(Unit::new());
        black_box(unit.add_lib(StdLib));

        unit.push_script(
            r#"
                main() {
                    print("Hello, world!")
                }
                "#,
        )
        .unwrap();

        unit.get_fn("main").unwrap().downcast::<(), ()>()();
    });
}
