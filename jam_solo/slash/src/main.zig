const std = @import("std");
const debug = std.debug;
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const Eval = @import("Eval.zig");

const src =
    \\123 + 123
    \\2 * 3 - 4 / 2 * 33424
;

pub fn main() anyerror!void {
    var allocator = std.testing.allocator;
    var lexer = Lexer{ .bytes = src };
    var parser = try Parser.init(allocator, &lexer);
    var eval = Eval.init(allocator, &parser);
    debug.print("{}\n", .{try eval.evalAll()});
}
