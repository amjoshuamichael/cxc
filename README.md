# cxc
`cxc` is a high-performance scripting language designed for speed: both of writing code, and of running code. It is built to be used as a Rust-hosted scripting language and as such, it has full interoperability with rust. `cxc` syntax is concise and low-level intention is inferred, not explicitly stated by the programmer. Like Rust, `cxc` compiles down to [LLVM](https://llvm.org) and then to machine code. `cxc` has Rust-level performance, but can be compiled at runtime.

This repository contains the compiler. The compiler uses [inkwell](https://github.com/TheDan64/inkwell), so if you wish to contribute, ensure you have LLVM installed, and the subdirectories `llvm/includes` and `llvm/bin` both in your path.

## Example
`prime.cxc`
```ruby
# function that takes a 32 bit integer and returns a boolean
is_prime(num: i32); bool { 
    divider = 2 # declare divider (type is inferred as i32)

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
    let mut unit = cxc::Unit::new();

    unit.push_script(include_str!("prime.cxc"));

    let is_prime = unit.get_fn_by_name::<(), bool>("is_prime");
    assert_eq!(unsafe { is_prime(29) }, true);
}
```

# ⚠️ WARNING ⚠️  ( USE AT YOUR OWN RISK )
* The compiler is very young, and there are still many bugs. (If you are using the language, please feel free to throw any issue reports my way! Any feedback of any kind is greatly appreciated!!)
* `cxc` is a low-level language, so it does a lot of low-level and unsafe operations at runtime. this means that the communication between `cxc` and rust code is unsafe and unstable. Because the compiler is not mature, many of the FFI and ABI bugs have not been ironed out.
* This also isn't a quick and easy tool for scripting. [There are many (better) options for adding scripting to an application.](https://www.boringcactus.com/2020/09/16/survey-of-rust-embeddable-scripting-languages.html) `cxc` is designed for applications that are serious about scripting, or where the ability to script is a main feature.
* `cxc` is only being tested on the latest version of rust nightly. FFI issues *will* occur if you use any other version.
