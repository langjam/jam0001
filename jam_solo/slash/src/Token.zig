pub const Type = enum {
    com_start,
    com_end,
    com_insert,

    lit_int,

    op_concat,
    op_define,
    op_equals,

    punct_colon,
    punct_dash,
    punct_equal,
    punct_octo,
    punct_plus,
    punct_slash,
    punct_star,

    unknown,
};

ty: Type,
src: ?[]const u8 = null,
offset: usize,

pub fn is(self: Self, ty: Type) bool {
    return ty == self.ty;
}
