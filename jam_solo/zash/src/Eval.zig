const std = @import("std");
const debug = std.debug;
const Call = @import("ast.zig").Call;
const Function = @import("ast.zig").Function;
const Infix = @import("ast.zig").Infix;
const Node = @import("ast.zig").Node;
const Parser = @import("Parser.zig");
const Prefix = @import("ast.zig").Prefix;

allocator: *std.mem.Allocator,
global: Env,
parser: *Parser,

const Self = @This();

pub const Env = struct {
    funcs: std.StringHashMap(Function),
    vars: std.StringHashMap(Value),
    parent: ?*Env,

    pub fn init(allocator: *std.mem.Allocator, parent: ?*Env) Env {
        return .{
            .funcs = std.StringHashMap(Function).init(allocator),
            .vars = std.StringHashMap(Value).init(allocator),
            .parent = parent,
        };
    }

    pub fn deinit(self: *Env) void {
        self.funcs.deinit();
        self.vars.deinit();
    }

    // Variables.
    pub fn getVar(self: Env, name: []const u8) ?Value {
        if (self.vars.get(name)) |v| {
            return v;
        } else if (self.parent) |p| {
            return p.getVar(name);
        } else {
            return null;
        }
    }

    pub fn insertVar(self: *Env, name: []const u8, value: Value) !void {
        try self.vars.put(name, value);
    }

    pub fn updateVar(self: *Env, name: []const u8, value: Value) void {
        if (self.vars.getPtr(name)) |p| {
            p.* = value;
        } else {
            @panic("Undefined variable!");
        }
    }

    pub fn delVar(self: *Env, name: []const u8) void {
        self.vars.remove(name);
    }

    // Funcs
    pub fn getFunc(self: Env, name: []const u8) ?Function {
        if (self.funcs.get(name)) |f| {
            return f;
        } else if (self.parent) |p| {
            return p.getFunc(name);
        } else {
            return null;
        }
    }

    pub fn insertFunc(self: *Env, name: []const u8, func: Function) !void {
        try self.funcs.put(name, func);
    }
};

pub const Value = union(enum) {
    boolean: bool,
    comment: []const u8,
    function,
    integer: isize,

    pub fn format(value: Value, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;

        switch (value) {
            .boolean => |b| _ = try writer.print("{}", .{b}),
            .comment => |c| _ = try writer.print("{s}", .{c}),
            .function => {},
            .integer => |i| _ = try writer.print("{}", .{i}),
        }
    }
};

pub fn init(allocator: *std.mem.Allocator, parser: *Parser) Self {
    return .{
        .allocator = allocator,
        .global = Env.init(allocator, null),
        .parser = parser,
    };
}

pub fn deinit(self: *Self) void {
    self.global.deinit();
}

pub fn evalAll(self: *Self) !Value {
    return self.eval(try self.parser.parseAll(), &self.global);
}

pub fn eval(self: *Self, node: Node, env: *Env) anyerror!Value {
    return switch (node) {
        .boolean => |b| .{ .boolean = std.mem.eql(u8, b.token.src.?, "true") },
        .call => |c| try self.evalFnCall(c, env),
        .ident => |tn| env.getVar(tn.token.src.?) orelse @panic("Undeclared identifier!"),
        .function => |f| result: {
            try env.insertFunc(f.name.src.?, f);
            break :result .function;
        },
        .comment => |c| .{ .comment = c.token.src.? },
        .infix => |i| self.evalInfix(i, env),
        .integer => |tn| .{ .integer = try std.fmt.parseInt(isize, tn.token.src.?, 10) },
        .prefix => |p| self.evalPrefix(p, env),
        .program => |p| try self.evalProgram(p, env),
        else => @panic("Unknown node type!"),
    };
}

// Eval funcs.
fn evalFnCall(self: *Self, c: Call, env: *Env) !Value {
    const func = env.getFunc(c.name.src.?) orelse @panic("Undefined function!");
    var fn_env = Env.init(self.allocator, env);
    defer fn_env.deinit();

    if (c.args.len != func.params.len) @panic("Function call args and params count mismatch!");
    for (func.params) |param, i| {
        const value = try self.eval(c.args[i], env);
        try fn_env.insertVar(param.src.?, value);
    }

    var value: Value = undefined;
    for (func.body) |node| {
        value = try self.eval(node, &fn_env);
    }

    return value;
}

fn evalInfix(self: *Self, i: Infix, env: *Env) !Value {
    // Going out on a limb here assuming the types. Cowboy programming, big time!
    return switch (i.op.ty) {
        .op_concat => result: {
            const c1 = try self.eval(i.lhs.*, env);
            const c2 = try self.eval(i.rhs.*, env);
            var joined = try std.ArrayList(u8).initCapacity(self.allocator, c1.comment.len + c2.comment.len + 1);
            defer joined.deinit();
            joined.appendSliceAssumeCapacity(c1.comment);
            joined.appendAssumeCapacity(' ');
            joined.appendSliceAssumeCapacity(c2.comment);
            break :result .{ .comment = joined.toOwnedSlice() };
        },
        .op_define => result: {
            const value = try self.eval(i.rhs.*, env);
            try env.insertVar(i.lhs.ident.token.src.?, value);
            break :result value;
        },
        .op_equals => result: {
            const lhs_v = try self.eval(i.lhs.*, env);
            const rhs_v = try self.eval(i.rhs.*, env);
            if (lhs_v == .integer) {
                break :result Value{ .boolean = lhs_v.integer == rhs_v.integer };
            } else {
                break :result Value{ .boolean = std.mem.eql(u8, lhs_v.comment, rhs_v.comment) };
            }
        },
        .op_neq => result: {
            const lhs_v = try self.eval(i.lhs.*, env);
            const rhs_v = try self.eval(i.rhs.*, env);
            if (lhs_v == .integer) {
                break :result Value{ .boolean = lhs_v.integer != rhs_v.integer };
            } else {
                break :result Value{ .boolean = !std.mem.eql(u8, lhs_v.comment, rhs_v.comment) };
            }
        },
        .punct_equal => result: {
            const value = try self.eval(i.rhs.*, env);
            env.updateVar(i.lhs.ident.token.src.?, value);
            break :result value;
        },
        .punct_plus => result: {
            const lhs_v = try self.eval(i.lhs.*, env);
            const rhs_v = try self.eval(i.rhs.*, env);
            break :result Value{ .integer = lhs_v.integer + rhs_v.integer };
        },
        .punct_dash => result: {
            const lhs_v = try self.eval(i.lhs.*, env);
            const rhs_v = try self.eval(i.rhs.*, env);
            break :result Value{ .integer = lhs_v.integer - rhs_v.integer };
        },
        .punct_slash => result: {
            const lhs_v = try self.eval(i.lhs.*, env);
            const rhs_v = try self.eval(i.rhs.*, env);
            break :result Value{ .integer = @divFloor(lhs_v.integer, rhs_v.integer) };
        },
        .punct_star => result: {
            const lhs_v = try self.eval(i.lhs.*, env);
            const rhs_v = try self.eval(i.rhs.*, env);
            break :result Value{ .integer = lhs_v.integer * rhs_v.integer };
        },
        else => @panic("Unknown infix operator!"),
    };
}

fn evalPrefix(self: *Self, p: Prefix, env: *Env) !Value {
    // Going out on a limb here assuming the types. Cowboy programming, big time!
    return switch (p.op.ty) {
        .punct_bang => result: {
            const rhs_v = try self.eval(p.rhs.*, env);
            break :result Value{ .boolean = !rhs_v.boolean };
        },
        .punct_lparen => try self.eval(p.rhs.*, env),
        else => @panic("Unknown prefix operator!"),
    };
}

fn evalProgram(self: *Self, p: []Node, env: *Env) !Value {
    var value: Value = undefined;

    for (p) |node| {
        value = try self.eval(node, env);
    }

    return value;
}
