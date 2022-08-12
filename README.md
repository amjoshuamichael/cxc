# cxc

`cxc` is an experimental scripting language designed from the ground up for speed: both of writing code, and of running code. It is built to be used as a scripting language with Rust as its host, and as such, it is designed for full interoperability with a simple api. Also as a scripting language, compile times are much shorter than they would be in other compiled languages. Syntax is concise and low-level intention is inferred, not explicitly stated by the programmer. Like Rust, `cxc` compiles down to [LLVM](llvm.org) and then to machine code. This makes for code that reads simply, but runs quickly.

This repository contians the compiler. The compiler uses [inkwell](https://github.com/TheDan64/inkwell), so if you wish to contribute, ensure you have LLVM installed, and the subdirectories `llvm/includes` and `llvm/bin` both in your path.

## Example

`prime.cxc`
```ruby
# function that takes a 32 bit integer and returns a 32 bit integer
# Returns 1 if num is prime and 0 if num is composite.
is_prime : prim:i32 (num : prim:i32) { 
    divider : prim:i32 = 2 # declare two variables
    output : prim:i32 = 1

    @ divider < num { # while divider is less than num
        ? num % divider == 0 { # if num is divisible by divider
            output = 0 # set output to 1, because number is not prime
        }

        divider = divider + 1 # increment divider
    }

    ! output # return output
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
