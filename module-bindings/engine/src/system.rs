use std::ffi::{CStr, CString};

pub fn system_name_generator(module_name: &str, system_name: &str) -> String {
    format!("{module_name}::{system_name}")
}

pub fn system_name_generator_c(module_name: &CStr, system_name: &CStr) -> CString {
    let seperator = b"::";
    let module_name_bytes = module_name.to_bytes();
    let system_name_bytes = system_name.to_bytes();
    let mut string_bytes =
        Vec::with_capacity(module_name_bytes.len() + seperator.len() + system_name_bytes.len());
    string_bytes.extend_from_slice(module_name_bytes);
    string_bytes.extend_from_slice(seperator);
    string_bytes.extend_from_slice(system_name_bytes);
    CString::new(string_bytes).unwrap()
}
