# cxc

`cxc` is a high-performance scripting language designed for speed: both of writing code, and of running code. It is built to be used as a Rust-hosted scripting language and as such, it has full interoperability with rust. `cxc` syntax is concise and low-level intention is inferred, not explicitly stated by the programmer. Like Rust, `cxc` compiles down to [LLVM](llvm.org) and then to machine code. This makes for code that reads simply, but runs quickly.

This repository contians the compiler. The compiler uses [inkwell](https://github.com/TheDan64/inkwell), so if you wish to contribute, ensure you have LLVM installed, and the subdirectories `llvm/includes` and `llvm/bin` both in your path.

## Example

`prime.cxc`
```ruby
# function that takes a 32 bit integer and returns a 32 bit integer
# Returns 1 if num is prime and 0 if num is composite.
is_prime(num: i32): bool { 
    divider: i32 = 2 # declare two variables

    @ divider < num { # while divider is less than num
        ? num % divider == 0 { # if num is divisible by divider
            ; true # number is not prime, so return true
        }

        divider = divider + 1 # increment divider
    }

    ; false # num is not divisible by any numbers, so return false
}
```

`main.rs`
```rust
fn main() {
    let context = Context::create();
    let mut unit = unit::Unit::new(&context);

    unit.push_script(include_str!("prime.cxc"));

    let is_prime = unit.get_fn("is_prime");
    assert_eq!(is_prime(29), 1);
}
```

One of the future goals of `cxc` is to implement [a large-scale type inference system](https://www.youtube.com/watch?v=fDTt_uo0F-g) so that neither variable types nor function parameter types have to be declared. This would allow the language to be written like a dynamic language, with concise syntax, but run with statically typed speed, without any type errors at runtime. 
