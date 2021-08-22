use std::ffi;
use std::os::raw;

pub unsafe extern "C" fn print(s: *mut raw::c_char) -> i64 {
    let c_string = ffi::CStr::from_ptr(s);

    if let Ok(rs_str) = c_string.to_str() {
        print!("{}", rs_str);
        0
    } else {
        1
    }
}

pub unsafe extern "C" fn print_int(i: i64) {
    print!("{}", i);
}

pub unsafe extern "C" fn print_float(i: f64) {
    print!("{}", i);
}

pub unsafe extern "C" fn println(s: *mut raw::c_char) -> i64 {
    let c_string = ffi::CStr::from_ptr(s);

    if let Ok(rs_str) = c_string.to_str() {
        println!("{}", rs_str);
        0
    } else {
        1
    }
}