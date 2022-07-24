# cxc

`cxc` is an experimental scripting language designed from the ground up for speed: both of writing code, and of running code. It is built to be used as a scripting language with Rust as its host, and as such, it is designed for full interoperability with a simple api. Syntax is concise and low-level intention is inferred, not explicitly stated by the programmer. Like Rust, `cxc` compiles down to [LLVM](llvm.org) and then to machine code. This makes for code that reads simply, but runs quickly.

This repository contians the compiler. The compiler uses [inkwell](https://github.com/TheDan64/inkwell), so if you wish to contribute, ensure you have LLVM installed, and the subdirectories `llvm/includes` and `llvm/bin` both in your path.

## Example

`prime.cxc`
```ruby
is_prime : prim::i32 (num : prim::i32) {
    divider : prim::i32 = 0
    output : prim::i32 = 1

    @ divider < num {
        ? num % div == 0 {
            output = 1
        }

        divider = divider + 1
    }

    ! output
}
```

`main.rs`
```rust
fn main() {
    let context = Context::create();
    let mut unit = unit::Unit::new(&context);

    unit.push_script(include_str!("prime.cxc");

    let is_prime = unit.get_fn("is_prime");
    assert_eq!(is_prime(29), 1);
}
```
