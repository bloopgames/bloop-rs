//! Generates the C FFI layer from Rust code.

use module_codegen::FfiBuilder;

fn main() {
    FfiBuilder::new()
        .engine_package_path("crate")
        .add_no_mangle(false)
        .write();
}
