pub const Type = enum {
    comment,
    com_insert,
    ident,

    kw_fn,
    kw_false,
    kw_true,

    lit_int,

    op_concat,
    op_define,
    op_equals,
    op_neq,

    punct_bang,
    punct_colon,
    punct_comma,
    punct_dash,
    punct_equal,
    punct_lbrace,
    punct_lparen,
    punct_newline,
    punct_octo,
    punct_plus,
    punct_rbrace,
    punct_rparen,
    punct_slash,
    punct_star,

    unknown,
};

ty: Type,
src: ?[]const u8 = null,
offset: usize,

const Self = @This();

pub fn is(self: Self, ty: Type) bool {
    return ty == self.ty;
}
