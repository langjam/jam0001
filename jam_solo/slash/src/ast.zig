const Token = @import("Token.zig");

pub const Node = union(enum) {
    infix: Infix,
    integer: TokenNode,
    program: []Node,
    unknown: TokenNode,
};

pub const TokenNode = struct {
    token: Token,
};

pub const Infix = struct {
    op: Token,
    lhs: *Node,
    rhs: *Node,
};
