#![feature(test)]

extern crate test;

use cxc::{library::StdLib, Unit};
use test::Bencher;

#[bench]
pub fn compile_std_lib(b: &mut Bencher) {
    b.iter(|| {
        for _ in 0..100 {
            let mut unit = Unit::new();
            unit.add_lib(StdLib);
            unit.push_script("
                main() {
                    # use some stdlib stuff
                    rcint := Rc<i32>:new(32)
                    rcfloat := Rc<f32>:new(32.0)
                    arcint := Arc<i32>:new(34)
                    arcfloat := Arc<f32>:new(34.0)
                    vecint := Vec<i32>:new()
                    vecint.push(234)
                    vecfloat := Vec<f32>:new()
                    vecfloat.push(234.0)
                }
            ").unwrap();
        }
    });
}
