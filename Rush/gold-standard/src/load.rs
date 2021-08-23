use std::collections::HashMap;
use std::mem::transmute;

use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncId, Linkage, Module};
use target_lexicon::Triple;

use crate::io::{print, println, print_int, print_float};
use crate::math::ipowi;
use crate::mem::{free, malloc};

pub const PRINT_INT_SYMBOL: &str = "print_int";
pub const PRINT_INT_ADDRESS: *const u8 =
    unsafe { transmute(print_int as unsafe extern "C" fn(_)) };

pub const PRINT_FLOAT_SYMBOL: &str = "print_float";
pub const PRINT_FLOAT_ADDRESS: *const u8 =
    unsafe { transmute(print_float as unsafe extern "C" fn(_)) };

pub const PRINT_SYMBOL: &str = "print";
pub const PRINT_ADDRESS: *const u8 =
    unsafe { transmute(print as unsafe extern "C" fn(_) -> _) };

pub const PRINTLN_SYMBOL: &str = "println";
pub const PRINTLN_ADDRESS: *const u8 =
    unsafe { transmute(println as unsafe extern "C" fn(_) -> _) };

pub const MALLOC_SYMBOL: &str = "malloc";
pub const MALLOC_ADDRESS: *const u8 =
    unsafe { transmute(malloc as unsafe extern "C" fn(_) -> _) };

pub const FREE_SYMBOL: &str = "free";
pub const FREE_ADDRESS: *const u8 =
    unsafe { transmute(free as unsafe extern "C" fn(_)) };

pub const IPOWI_SYMBOL: &str = "ipowi";
pub const IPOWI_ADDRESS: *const u8 =
    unsafe { transmute(ipowi as unsafe extern "C" fn(_, _) -> _) };

pub const STRCMP_SYMBOL: &str = "string_compare";
pub const STRCMP_ADDRESS: *const u8 =
    unsafe { transmute(ipowi as unsafe extern "C" fn(_, _) -> _) };

pub const SYMBOLS: [(&str, *const u8); 8] = [
    (PRINT_SYMBOL, PRINT_ADDRESS),
    (PRINTLN_SYMBOL, PRINTLN_ADDRESS),
    (MALLOC_SYMBOL, MALLOC_ADDRESS),
    (FREE_SYMBOL, FREE_ADDRESS),
    (IPOWI_SYMBOL, IPOWI_ADDRESS),
    (STRCMP_SYMBOL, STRCMP_ADDRESS),
    (PRINT_INT_SYMBOL, PRINT_INT_ADDRESS),
    (PRINT_FLOAT_SYMBOL, PRINT_FLOAT_ADDRESS),
];

pub fn load_symbols(jit_builder: &mut JITBuilder) {
    jit_builder.symbols(SYMBOLS);
}

fn fn_declare<'a>(
    module: &mut JITModule,
    ids: &mut HashMap<&'a str, FuncId>,
    name: &'a str,
    params: &[AbiParam],
    out_param: Option<&AbiParam>,
) {
    let mut sig = module.make_signature();

    for p in params.iter() {
        sig.params.push(p.to_owned());
    }

    out_param.into_iter().for_each(|p| {
        sig.returns.push(p.to_owned());
    });

    let fid = module.declare_function(name, Linkage::Import, &sig).unwrap();

    ids.insert(name, fid);
}

pub fn declare_functions(module: &mut JITModule) -> HashMap<&'static str, FuncId> {
    let ptr_type = AbiParam::new(Type::triple_pointer_type(&Triple::host()));
    let int_type = AbiParam::new(types::I64);
    let float_type = AbiParam::new(types::F64);

    let mut ids = HashMap::new();

    fn_declare(module, &mut ids, PRINT_SYMBOL, &[ptr_type.clone()], Some(&int_type));
    fn_declare(module, &mut ids, PRINTLN_SYMBOL, &[ptr_type.clone()], Some(&int_type));
    fn_declare(module, &mut ids, MALLOC_SYMBOL, &[int_type.clone()], Some(&ptr_type));
    fn_declare(module, &mut ids, FREE_SYMBOL, &[ptr_type.clone()], None);
    fn_declare(module, &mut ids, IPOWI_SYMBOL, &[int_type.clone(), int_type.clone()], Some(&int_type));
    fn_declare(module, &mut ids, STRCMP_SYMBOL, &[ptr_type.clone(), ptr_type.clone()], Some(&int_type));
    fn_declare(module, &mut ids, PRINT_INT_SYMBOL, &[int_type.clone()], None);
    fn_declare(module, &mut ids, PRINT_FLOAT_SYMBOL, &[float_type.clone()], None);

    ids
}