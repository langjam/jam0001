use std::ffi;

pub unsafe extern "C" fn malloc(size: i64) -> *mut ffi::c_void {
    libc::malloc(size as libc::size_t)
}

pub unsafe extern "C" fn free(ptr: *mut ffi::c_void) {
    libc::free(ptr)
}