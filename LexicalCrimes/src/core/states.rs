use super::tokens::Token;

use parking_lot::Mutex;
use std::collections::HashMap;
use ahash::RandomState;
use once_cell::sync::Lazy;

#[derive(Copy, Clone)]
pub enum Operation {
    Allocation,
    _Threads,
    StateChange,
    Loop(i32),
    _CurrIter(i32),
}

#[derive(Copy, Clone)]
pub struct ProgramState {
    pub(super) function: Token,
    pub(super) operation: Operation,
    pub(super) line: usize,
}

pub const STATE_KEY: &str = "state";

pub static PROGRAM_STATE: Lazy<Mutex<HashMap<&str, ProgramState, RandomState>>> =
    Lazy::new(|| {
        let hasher = RandomState::new();
        Mutex::new(HashMap::with_capacity_and_hasher(1, hasher))
    });

impl ProgramState {
    pub fn read_state() -> Self {
        *PROGRAM_STATE.lock().get(STATE_KEY).unwrap()
    }

    pub fn set_state(function: Token, operation: Operation, line: usize) {
        let state = Self {
            function,
            operation,
            line,
        };
        PROGRAM_STATE.lock().insert(STATE_KEY, state);
    }
}