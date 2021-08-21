pub mod ast;
pub mod data;
pub mod env;
pub mod eval;
pub mod parser;

#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
pub mod wasm {
    use wasm_bindgen::prelude::*;

    #[wasm_bindgen]
    extern "C" {
        fn alert(s: &str);
    }

    #[wasm_bindgen]
    pub fn greet(name: &str) {
        alert(&format!("Hello, {}!", name));
    }
}

#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
pub use wasm::*;
