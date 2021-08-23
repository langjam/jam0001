use std::os::raw;

pub unsafe extern "C" fn string_compare(lhs: *mut raw::c_char, rhs: *mut raw::c_char) -> i64 {
    libc::strcmp(lhs, rhs) as i64
}