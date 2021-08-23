use parking_lot::Mutex;
use once_cell::sync::Lazy;
use std::collections::HashMap;
use ahash::RandomState;

// every datatype is formatted upon emitting (atm)
pub enum Value {
    String(String),
    FInt(f64),
    Int(i32),
    Nothing,
}

#[derive(Debug, Clone)]
pub struct MemoryLayout {
    pub(crate) value: String,
}

pub trait Manager {
    fn fetch(key: &str) -> Option<Value>;
    fn alloc(key: String, mem: Value);
}

pub static MEMORY_REP: Lazy<Mutex<HashMap<String, MemoryLayout, RandomState>>> =
    Lazy::new(|| {
        Mutex::new(HashMap::default())
    });

// don't ask me what this is...
impl Manager for MemoryLayout {
    fn fetch(key: &str) -> Option<Value> {
        let memory_map = &MEMORY_REP;

        if memory_map.lock().contains_key(key) {
            let ret = &*memory_map.lock();
            let layout = ret.get(key).unwrap();
            Some(
                Value::String(
                    layout.clone().value
                )
            )
        } else {
            // value not found in memory
            None
        }
    }

    fn alloc(key: String, mem: Value) {
        let mem = match mem {
            Value::String(mem) => mem,
            Value::FInt(mem) => mem.to_string(),
            Value::Int(mem) => mem.to_string(),
            Value::Nothing => unimplemented!(),
        };

        let rep = Self {
            value: mem,
        };

        MEMORY_REP.lock().insert(key, rep);
    }
}