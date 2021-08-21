const std = @import("std");
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
        '#' => self.lexOcto(),
        '+' => self.lexPlus(),
        '-' => Token{ .ty = .punct_dash, .offset = self.i.? },
        '/' => Token{ .ty = .punct_slash, .offset = self.i.? },
        '*' => Token{ .ty = .punct_star, .offset = self.i.? },
        else => Token{ .ty = .unknown, .offset = self.i.? },
    };
}

// Lex funcs.
fn lexColon(self: *Self) Token {
    var ty: Token.Type = .punct_colon;

    if (self.peek()) |pb| {
        switch (pb) {
            '=' => ty = .op_define,
            '#' => ty = .com_end,
            else => {},
        }
    }

    return .{
        .ty = ty,
        .offset = self.i.?,
    };
}

fn lexEquals(self: *Self) Token {
    const ty: Token.Type = if (self.peekIs('=')) .op_equals else .punct_equal;

    return .{
        .ty = ty,
        .offset = self.i.?,
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
    var ty: Token.Type = .punct_octo;

    if (self.peek()) |pb| {
        switch (pb) {
            ':' => ty = .com_start,
            '=' => ty = .com_insert,
            else => {},
        }
    }

    return .{
        .ty = ty,
        .offset = self.i.?,
    };
}

fn lexPlus(self: *Self) Token {
    const ty: Token.Type = if (self.peekIs('+')) .op_concat else .punct_plus;

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

fn run(self: *Self, predicate: BytePredicate) void {
    while (self.peek()) |b| {
        if (!predicate(b)) break;
        _ = self.advance();
    }
}

fn isWhiteSpace(b: u8) bool {
    return ' ' == b or '\t' == b or '\r' == b or '\n' == b;
}

fn skipWhiteSpace(self: *Self) void {
    self.run(isWhiteSpace);
}
