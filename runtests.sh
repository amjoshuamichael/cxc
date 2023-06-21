echo "\033[1;33m -------------------------------------------------"
echo "\033[1;33m testing cranelift"
cargo t --no-default-features --features="backend-cranelift ffi-assertions" || (echo "\033[0;31m cranelift failed" && exit 1)
echo "\033[1;33m -------------------------------------------------"
echo "\033[1;33m testing llvm"
cargo t --no-default-features --features="backend-llvm ffi-assertions" || (echo "\033[0;31m llvm failed" && exit 1)
echo "\033[1;33m -------------------------------------------------"
echo "\033[1;33m running cargo check for different configurations"
cargo c --no-default-features --features="backend-llvm" && cargo c --no-default-features --features="backend-cranelift" || (echo "\033[0;31m cannot compile without ffi-assertions" && exit 1)
echo "\033[0;32m -------------------------------------------------"
echo ""
echo "\033[0;32m all tests passing with all feature combinations!!"
