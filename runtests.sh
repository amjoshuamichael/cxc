cargo t --no-default-features --features="backend-cranelift ffi-assertions"
cargo t --no-default-features --features="backend-llvm ffi-assertions"
cargo c --no-default-features --features="backend-llvm"
cargo c --no-default-features --features="backend-cranelift"
echo "\033[0;37m -------------------------------------------------"
echo ""
echo "\033[0;32m all tests passing with all feature combinations!!"
