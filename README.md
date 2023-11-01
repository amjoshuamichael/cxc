# cxc
cxc is a high-performance scripting language designed for speed: both of writing code, and of running code. It is built to be used as a Rust-hosted scripting language and as such, it has full interoperability with Rust. cxc is about playing with software at a low level–getting your hands dirty *without* gloves, just because it's fun. Like Rust, cxc compiles down to an MIR, which can then be compiled or interpreted by a number of backends, including LLVM and Cranelift. Because of this, cxc has near-Rust performance, but it can be compiled and modified at runtime.

## Installation
Put the following in your `Cargo.toml`:
```toml
[dependencies]
cxc = "0.3"
```

The default features of the crate use [cranelift](https://cranelift.dev/) as a compiler backend. Alternatively, you can activate the "backend-llvm" feature, which uses [LLVM](https://llvm.org/), but it does require that you have LLVM installed, and the subdirectories `llvm/includes` and `llvm/bin` both in your path. Both backends have full feature parity. The Cranelift backend has faster compile times, and is more portable, but the emitted code is slower. The LLVM backend is less portable because it requires that users have LLVM installed, but the emitted code is faster.

You can access the LLVM backend by putting this in your `Cargo.toml`:
```toml
[dependencies]
cxc = {
    version = "0.3",
    default-features = false, 
    features = ["backend-llvm", "ffi-assertions"]
}
```

## Example


`prime.cxc`
```ruby
# function that takes a 32 bit integer and returns a boolean
is_prime(num: i32); bool {
    divider := 2 # declare divider (type is inferred as i32)

    @ divider < num { # while divider is less than num
        ? num % divider == 0 { # if num is divisible by divider
            ; false # number is not prime, so return false
        }

        divider = divider + 1 # increment divider
    }

    ; true # num is not divisible by any numbers, so return true
}
```

`main.rs`
```rust
fn main() {
    let mut unit = cxc::Unit::new();

    let prime_code: String = 
        include_str!("../README.md").lines().skip(27).take(14).collect::<Vec<_>>().join("\n");
    unit.push_script(&*prime_code).unwrap();

    let is_prime = unit.get_fn("is_prime").unwrap().downcast::<(i32,), bool>();

    assert_eq!(unsafe { is_prime(29) }, true);
}
```

# ⚠️ WARNING ⚠️  ( USE AT YOUR OWN RISK )
* The compiler is very young, and there are still many bugs. (If you are using the language, please feel free to throw any issue reports my way! Feedback of any kind is greatly appreciated.)
* cxc is a low-level language, so it does a lot of low-level and unsafe operations at runtime. this means that the communication between cxc and Rust code is unsafe and unstable.
* The "ffi-assertions" feature is designed to catch isses when you improperly match up Rust and cxc types and functions, but it isn't a catch-all. The [cxc_derive](https://github.com/amjoshuamichael/cxc_derive) macro helps with this, but it's easy to mess up.
* **Minimum Supported Rust Version (MSRV) is latest nightly.** The crate uses unstable features, and FFI bugs *will* occur if you use any other version. 
* There is no documentation, because the langauge is **still changing**. Look in the tests folder for some examples if you want to see some.

If any of these are deal breakers for you, you can try a [different Rust scripting solution](https://arewegameyet.rs/ecosystem/scripting/).
