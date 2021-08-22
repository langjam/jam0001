pub unsafe extern "C" fn ipowi(lhs: i64, rhs: i64) -> i64 {
    if rhs < 0 {
        0
    } else {
        lhs.pow(rhs as u32)
    }
}