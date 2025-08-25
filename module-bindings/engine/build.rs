//! Generates the C FFI layer from Rust code.

use std::env::current_dir;

use module_codegen::FfiBuilder;

fn main() {
    FfiBuilder::new()
        .input_path(&current_dir().unwrap().join("src/lib.rs"))
        .engine_package_path("crate")
        .add_no_mangle(false)
        .write();
}
