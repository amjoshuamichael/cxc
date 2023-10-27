// run using cargo run --bin tests.rs
use std::process::Command;

fn main() {
    let features = [
        "backend-cranelift",
        "backend-interpreter",
        "backend-llvm",
    ];

    for feature in features {
        let status = Command::new("cargo")
            .arg("test")
            .arg("--no-default-features")
            .arg("--features")
            .arg(format!(r#"{feature} ffi-assertions"#))
            .status()
            .expect("failed to start cargo test");
        if !status.success() {
            std::process::exit(status.code().unwrap_or(1));
        }
    }
}
