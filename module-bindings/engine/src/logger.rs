use log::{Metadata, Record};

pub struct GameLogger;

impl log::Log for GameLogger {
    fn enabled(&self, _metadata: &Metadata) -> bool {
        true
    }

    fn log(&self, record: &Record) {
        if self.enabled(record.metadata()) {
            let module_path = record.module_path().unwrap_or("");
            let file = record.file().unwrap_or("");

            let msg = if let Some(line) = record.line() {
                format!("[{module_path}] {file}:{line}\n{}", record.args())
            } else {
                format!("[{module_path}] {file}\n{}", record.args())
            };

            #[cfg(not(feature = "dynamic_wasm"))]
            unsafe {
                _LOG_FN.unwrap_unchecked()(msg.as_ptr(), msg.len(), record.level() as usize);
            }

            #[cfg(feature = "dynamic_wasm")]
            unsafe {
                crate::wasm::alloc_and_write_external_slice(msg.as_bytes(), |ptr| {
                    _LOG_FN.unwrap_unchecked()(ptr.cast(), msg.len(), record.level() as usize);
                });
            }
        }
    }

    fn flush(&self) {}
}

/// Log a utf8 (pointer, size, level) message.
pub static mut _LOG_FN: Option<unsafe extern "C" fn(*const u8, usize, usize)> = None;
