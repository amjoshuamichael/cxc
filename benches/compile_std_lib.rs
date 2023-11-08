#![feature(test)]

extern crate test;

use cxc::{library::StdLib, Unit};
use test::Bencher;

#[bench]
pub fn compile_std_lib(b: &mut Bencher) {
    b.iter(|| {
        let mut outputs = Vec::<i32>::new();

        for n in 0..1000 {
            let mut unit = Unit::new();
            unit.add_lib(StdLib);
            unit.push_script(&*format!("
                main(); i32 {{
                    # use some stdlib stuff
                    rcint := Rc<i32>:new(32)
                    rcint_clone := rcint.clone()
                    rcint_string := rcint_clone.to_string()
                    rcfloat := Rc<f32>:new(32.0)
                    arcint := Arc<i32>:new(34)
                    arcfloat := Arc<f32>:new(34.0)

                    vecint := vec_from_arr([91, 92, 93, 94])
                    vecint.push(234)

                    vecint2 := Vec<i32>:init_with(4, 90)
                    for vecint2 {{
                        vecint.push(*it)
                    }}

                    vecfloat := Vec<f32>:new()
                    vecfloat.push(234.0)

                    sum := 0.0
                    for vecfloat {{ sum = sum + *it }}

                    sum2 := vecint.get(u64 0)
                    for vecint {{
                        sum2 = sum2 + *it 
                        sum2 = sum2 + {n}
                    }}

                    ; sum2
                }}
            ")).unwrap();

            let output = unit.get_fn("main").unwrap().downcast::<(), i32>()();
            outputs.push(output);

            std::hint::black_box(unit);
            std::hint::black_box(output);
        }
        std::hint::black_box(outputs);
    });
}
