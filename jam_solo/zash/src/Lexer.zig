const std = @import("std");
const debug = std.debug;
const Token = @import("Token.zig");

bytes: []const u8,
i: ?usize = null,

const Self = @This();

pub fn lexAll(self: *Self, allocator: *std.mem.Allocator) ![]Token {
    var tokens = std.ArrayList(Token).init(allocator);
    defer tokens.deinit();

    while (self.next()) |token| {
        try tokens.append(token);
    }

    return tokens.toOwnedSlice();
}

pub fn next(self: *Self) ?Token {
    self.skipWhiteSpace();

    const b = self.advance() orelse return null;

    return switch (b) {
        ':' => self.lexColon(),
        '=' => self.lexEquals(),
        '0'...'9' => self.lexNumber(),
        'a'...'z', 'A'...'Z', '_' => self.lexIdent(),
        '#' => self.lexOcto(),
        '+' => self.lexPlus(),
        '!' => self.lexBang(),
        ',' => Token{ .ty = .punct_comma, .offset = self.i.? },
        '-' => Token{ .ty = .punct_dash, .offset = self.i.? },
        '{' => Token{ .ty = .punct_lbrace, .offset = self.i.? },
        '}' => Token{ .ty = .punct_rbrace, .offset = self.i.? },
        '(' => Token{ .ty = .punct_lparen, .offset = self.i.? },
        ')' => Token{ .ty = .punct_rparen, .offset = self.i.? },
        '/' => Token{ .ty = .punct_slash, .offset = self.i.? },
        '*' => Token{ .ty = .punct_star, .offset = self.i.? },
        '\n' => result: {
            self.run(isNewline);
            break :result Token{ .ty = .punct_newline, .offset = self.i.? };
        },
        else => result: {
            //debug.print("\n{u}\n", .{b});
            break :result Token{ .ty = .unknown, .offset = self.i.? };
        },
    };
}

// Lex funcs.
fn lexBang(self: *Self) Token {
    const ty: Token.Type = if (self.skip('=')) .op_neq else .punct_bang;

    return .{
        .ty = ty,
        .offset = self.i.?,
    };
}

fn lexColon(self: *Self) Token {
    const ty: Token.Type = if (self.skip('=')) .op_define else .punct_colon;

    return .{
        .ty = ty,
        .offset = self.i.?,
    };
}

fn lexEquals(self: *Self) Token {
    const ty: Token.Type = if (self.skip('=')) .op_equals else .punct_equal;

    return .{
        .ty = ty,
        .offset = self.i.?,
    };
}

fn isIdent(b: u8) bool {
    return ('a' <= b and b <= 'z') or ('A' <= b and b <= 'Z') or ('0' <= b and b <= '9') or '_' == b;
}

fn lexIdent(self: *Self) Token {
    const start = self.i.?;
    self.run(isIdent);
    const src = self.bytes[start .. self.i.? + 1];

    const ty: Token.Type = ty_detect: {
        if (std.mem.eql(u8, src, "fn")) break :ty_detect .kw_fn;
        if (std.mem.eql(u8, src, "false")) break :ty_detect .kw_false;
        if (std.mem.eql(u8, src, "true")) break :ty_detect .kw_true;
        break :ty_detect .ident;
    };

    return .{
        .ty = ty,
        .src = src,
        .offset = start,
    };
}

fn isNumber(b: u8) bool {
    return '0' <= b and b <= '9';
}

fn lexNumber(self: *Self) Token {
    const start = self.i.?;
    self.run(isNumber);

    return .{
        .ty = .lit_int,
        .src = self.bytes[start .. self.i.? + 1],
        .offset = start,
    };
}

fn lexOcto(self: *Self) Token {
    var token = Token{
        .ty = .punct_octo,
        .offset = self.i.? + 2,
    };

    if (self.skip('=')) {
        token.ty = .com_insert;
    } else if (self.skip(':')) {
        token.ty = .comment;
        var nest_level: usize = 1;

        while (self.advance()) |cb| {
            if ('\n' == cb) {
                break;
            } else if ('#' == cb and self.skip(':')) {
                nest_level += 1;
            } else if (':' == cb and self.skip('#')) {
                if (nest_level > 0) nest_level -= 1;
                if (nest_level == 0) break;
            }
        }

        token.src = std.mem.trim(u8, self.bytes[token.offset .. self.i.? - 1], " ");
    }

    return token;
}

fn lexPlus(self: *Self) Token {
    const ty: Token.Type = if (self.skip('+')) .op_concat else .punct_plus;

    return .{
        .ty = ty,
        .offset = self.i.?,
    };
}

// Lexer movement.
fn advance(self: *Self) ?u8 {
    if (self.i) |*index| {
        index.* += 1;
        if (index.* >= self.bytes.len) return null;
    } else {
        self.i = 0;
    }

    return self.bytes[self.i.?];
}

fn peek(self: *Self) ?u8 {
    if (self.i) |index| {
        return if (index + 1 < self.bytes.len) self.bytes[index + 1] else null;
    } else {
        return if (self.bytes.len > 0) self.bytes[0] else null;
    }
}

fn peekIs(self: *Self, b: u8) bool {
    return if (self.peek()) |pb| b == pb else false;
}

fn skip(self: *Self, b: u8) bool {
    if (self.peekIs(b)) {
        _ = self.advance();
        return true;
    }

    return false;
}

const BytePredicate = fn (u8) bool;

fn isNewline(b: u8) bool {
    return '\n' == b;
}

fn run(self: *Self, predicate: BytePredicate) void {
    while (self.peek()) |b| {
        if (!predicate(b)) break;
        _ = self.advance();
    }
}

fn isWhiteSpace(b: u8) bool {
    return ' ' == b or '\t' == b or '\r' == b;
}

fn skipWhiteSpace(self: *Self) void {
    self.run(isWhiteSpace);
}
