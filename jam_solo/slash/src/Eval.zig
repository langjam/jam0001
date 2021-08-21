const std = @import("std");
const Infix = @import("ast.zig").Infix;
const Node = @import("ast.zig").Node;
const Parser = @import("Parser.zig");

allocator: *std.mem.Allocator,
parser: *Parser,

const Self = @This();

pub const Value = union(enum) {
    integer: isize,

    pub fn format(value: Value, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;

        switch (value) {
            .integer => |i| _ = try writer.print("{}", .{i}),
        }
    }
};

pub fn init(allocator: *std.mem.Allocator, parser: *Parser) Self {
    return .{
        .allocator = allocator,
        .parser = parser,
    };
}

pub fn evalAll(self: *Self) !Value {
    return eval(try self.parser.parseAll());
}

pub fn eval(node: Node) anyerror!Value {
    return switch (node) {
        .infix => |i| evalInfix(i),
        .integer => |i| .{ .integer = try std.fmt.parseInt(isize, i.token.src.?, 10) },
        .program => |p| try evalProgram(p),
        else => @panic("Unknown node type!"),
    };
}

// Eval funcs.
fn evalInfix(i: Infix) !Value {
    return switch (i.op.ty) {
        // Going out on a limb here assuming their ints. Cowboy programming, big time!
        .punct_plus => result: {
            const lhs_v = try eval(i.lhs.*);
            const rhs_v = try eval(i.rhs.*);
            break :result Value{ .integer = lhs_v.integer + rhs_v.integer };
        },
        .punct_dash => result: {
            const lhs_v = try eval(i.lhs.*);
            const rhs_v = try eval(i.rhs.*);
            break :result Value{ .integer = lhs_v.integer - rhs_v.integer };
        },
        .punct_slash => result: {
            const lhs_v = try eval(i.lhs.*);
            const rhs_v = try eval(i.rhs.*);
            break :result Value{ .integer = @divFloor(lhs_v.integer, rhs_v.integer) };
        },
        .punct_star => result: {
            const lhs_v = try eval(i.lhs.*);
            const rhs_v = try eval(i.rhs.*);
            break :result Value{ .integer = lhs_v.integer * rhs_v.integer };
        },
        else => @panic("Unknown infix operator!"),
    };
}

fn evalProgram(p: []Node) !Value {
    var value: Value = undefined;

    for (p) |node| {
        value = try eval(node);
    }

    return value;
}
