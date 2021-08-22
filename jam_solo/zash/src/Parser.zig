const std = @import("std");
const debug = std.debug;
const Lexer = @import("Lexer.zig");
const Node = @import("ast.zig").Node;
const Token = @import("Token.zig");

allocator: *std.mem.Allocator,
i: ?usize = null,
lexer: *Lexer,
tokens: []Token,

const Self = @This();

pub fn init(allocator: *std.mem.Allocator, lexer: *Lexer) !Self {
    return Self{
        .allocator = allocator,
        .lexer = lexer,
        .tokens = try lexer.lexAll(allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.allocator.free(self.tokens);
}

pub fn parseAll(self: *Self) !Node {
    var nodes = std.ArrayList(Node).init(self.allocator);
    defer nodes.deinit();

    while (self.next()) |node| {
        try nodes.append(node);
    }

    return Node{ .program = nodes.toOwnedSlice() };
}

pub fn next(self: *Self) ?Node {
    if (self.advance()) |t| {
        var token = t;
        if (token.is(.punct_newline)) token = self.advance() orelse return null;

        return switch (token.ty) {
            else => self.parseExpression(token, .lowest) catch @panic("Unable to parse expression."),
        };
    } else {
        return null;
    }
}

// Operator precedence.
const Precedence = enum {
    lowest,
    define,
    assign,
    equals,
    sum,
    product,
    call,
    prefix,

    fn get(token: Token) Precedence {
        return switch (token.ty) {
            .op_define => .define,
            .punct_equal => .assign,
            .op_equals, .op_neq => .equals,
            .punct_plus, .punct_dash, .op_concat => .sum,
            .punct_slash, .punct_star => .product,
            .punct_lparen => .call,
            else => .lowest,
        };
    }

    fn less(a: Precedence, b: Precedence) bool {
        return @enumToInt(a) < @enumToInt(b);
    }
};

// Parse funcs.

// Pratt parser.
fn parseExpression(self: *Self, token: Token, precedence: Precedence) !Node {
    var lhs: Node = undefined;

    if (prefixFnFor(token)) |pfn| {
        lhs = pfn(self, token);
    } else {
        @panic("No prefix parse function for token!");
    }

    while (self.peek()) |pt| {
        if (pt.is(.comment) and !token.is(.punct_equal) and !token.is(.op_define) and !token.is(.op_concat)) {
            // Ignore comments in most contexts.
            _ = self.advance(); // comment
            continue;
        }

        if (!precedence.less(Precedence.get(pt))) break;
        const op_token = self.advance().?;
        const ifn = infixFnFor(op_token);
        lhs = try ifn(self, op_token, lhs);
    }

    return lhs;
}

// Prefix
const PrefixFn = fn (*Self, Token) Node;

fn prefixFnFor(token: Token) ?PrefixFn {
    return switch (token.ty) {
        .comment,
        .ident,
        .kw_false,
        .kw_true,
        .lit_int,
        => Self.parseTokenNode,
        .kw_fn => Self.parseFnDef,
        .punct_bang => Self.parsePrefix,
        .punct_lparen => Self.parseGroup,
        else => null,
    };
}

fn parseGroup(self: *Self, token: Token) Node {
    var node = self.allocator.create(Node) catch @panic("Unable to allocate grouped expression node!");
    node.* = self.parseExpression(self.expectNext(), .lowest) catch @panic("Unable to parse grouped expression!");
    _ = self.expectNextIs(.punct_rparen);

    return .{ .prefix = .{ .op = token, .rhs = node } };
}

fn parseTokenNode(_: *Self, token: Token) Node {
    return switch (token.ty) {
        .kw_false, .kw_true => .{ .boolean = .{ .token = token } },
        .comment => .{ .comment = .{ .token = token } },
        .ident => .{ .ident = .{ .token = token } },
        .lit_int => .{ .integer = .{ .token = token } },
        else => @panic("Invalid token type for TokenNode!"),
    };
}

fn parseFnDef(self: *Self, _: Token) Node {
    const name = self.expectNextIs(.ident);
    _ = self.expectNextIs(.punct_lparen);
    var params = std.ArrayList(Token).init(self.allocator);
    defer params.deinit();

    if (!self.peekIs(.punct_rparen)) {
        params.append(self.advance().?) catch @panic("Unable to append param!");
        _ = self.skip(.comment); // comments after param names

        while (self.skip(.punct_comma)) {
            params.append(self.expectNext()) catch @panic("Unable to append param!");
            _ = self.skip(.comment); // comments after param names
        }
    }

    _ = self.expectNextIs(.punct_rparen);
    _ = self.expectNextIs(.punct_lbrace);
    self.run(isNewline); // Possible newlines after opening brace.
    var body = std.ArrayList(Node).init(self.allocator);
    defer body.deinit();

    while (self.peek()) |pt| {
        if (pt.is(.punct_rbrace)) break;
        const node = self.parseExpression(self.advance().?, .lowest) catch @panic("Unable to parse body node!");
        body.append(node) catch @panic("Unable to append to body!");
        self.run(isNewline); // Possible newlines between expressions.
    }

    _ = self.expectNextIs(.punct_rbrace);

    return Node{ .function = .{
        .name = name,
        .params = params.toOwnedSlice(),
        .body = body.toOwnedSlice(),
    } };
}

fn parsePrefix(self: *Self, token: Token) Node {
    var rhs = self.allocator.create(Node) catch @panic("Unable to allocate prefix right hand side!");
    rhs.* = self.parseExpression(self.expectNext(), .prefix) catch @panic("Unable to parse boolean not expression!");

    return .{ .prefix = .{
        .op = token,
        .rhs = rhs,
    } };
}

// Infix
const InfixFn = fn (*Self, Token, Node) anyerror!Node;

fn infixFnFor(token: Token) InfixFn {
    return switch (token.ty) {
        .op_concat,
        .op_define,
        .op_equals,
        .op_neq,
        .punct_equal,
        .punct_dash,
        .punct_plus,
        .punct_slash,
        .punct_star,
        => Self.parseInfix,
        .punct_lparen => Self.parseFnCall,
        else => @panic("Unknown infix token type?"),
    };
}

fn parseInfix(self: *Self, token: Token, lhs: Node) !Node {
    const rhs_token = self.advance() orelse @panic("Expected right hand side token!");

    var lhs_ptr = try self.allocator.create(Node);
    lhs_ptr.* = lhs;
    var rhs_ptr = try self.allocator.create(Node);
    rhs_ptr.* = try self.parseExpression(rhs_token, Precedence.get(token));

    return Node{ .infix = .{
        .op = token,
        .lhs = lhs_ptr,
        .rhs = rhs_ptr,
    } };
}

fn parseFnCall(self: *Self, _: Token, lhs: Node) !Node {
    const name = lhs.ident.token;
    var args = std.ArrayList(Node).init(self.allocator);
    defer args.deinit();

    if (!self.peekIs(.punct_rparen)) {
        while (self.peek()) |pt| {
            if (pt.is(.punct_rparen)) break;
            const node = self.parseExpression(self.advance().?, .lowest) catch @panic("Unable to parse call arg!");
            args.append(node) catch @panic("Unable to append call arg!");
            _ = self.skip(.punct_comma);
        }
    }

    _ = self.expectNextIs(.punct_rparen);

    return Node{ .call = .{
        .name = name,
        .args = args.toOwnedSlice(),
    } };
}

// Parser movement.
fn advance(self: *Self) ?Token {
    if (self.i) |*index| {
        index.* += 1;
        if (index.* >= self.tokens.len) return null;
    } else {
        self.i = 0;
    }

    return self.tokens[self.i.?];
}

fn peek(self: Self) ?Token {
    if (self.i) |index| {
        return if (index + 1 < self.tokens.len) self.tokens[index + 1] else null;
    } else {
        return if (self.tokens.len > 0) self.tokens[0] else null;
    }
}

fn peekIs(self: Self, ty: Token.Type) bool {
    return if (self.peek()) |pt| ty == pt.ty else false;
}

fn expectNext(self: *Self) Token {
    return if (self.peek()) |_| self.advance().? else @panic("Unexpected end of tokens!");
}

fn expectNextIs(self: *Self, ty: Token.Type) Token {
    return if (self.peekIs(ty)) self.advance().? else @panic("Unexpected token!");
}

fn skip(self: *Self, ty: Token.Type) bool {
    if (self.peekIs(ty)) {
        _ = self.advance();
        return true;
    }

    return false;
}

const TokenPredicate = fn (Token) bool;

fn isNewline(token: Token) bool {
    return token.ty == .punct_newline;
}

fn run(self: *Self, predicate: TokenPredicate) void {
    while (self.peek()) |t| {
        if (!predicate(t)) break;
        _ = self.advance();
    }
}
