const Token = @import("Token.zig");

pub const Node = union(enum) {
    prefix: Prefix,
    boolean: TokenNode,
    call: Call,
    comment: TokenNode,
    function: Function,
    ident: TokenNode,
    infix: Infix,
    integer: TokenNode,
    program: []Node,
    unknown: TokenNode,
};

pub const Call = struct {
    name: Token,
    args: []Node,
};

pub const Function = struct {
    name: Token,
    params: []Token,
    body: []Node,
};

pub const Infix = struct {
    op: Token,
    lhs: *Node,
    rhs: *Node,
};

pub const Prefix = struct {
    op: Token,
    rhs: *Node,
};

pub const TokenNode = struct {
    token: Token,
};
