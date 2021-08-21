const std = @import("std");
const debug = std.debug;
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const Eval = @import("Eval.zig");

const src =
    \\#: This is a comment. :#
    \\#: This one too.
    \\#: Can we #: nest :# too? :#
    \\123 + 123
    \\2 * 3 - 4 #: inline comment :# / 2 * 33424
    \\x := 2112
    \\x
    \\y := #: Assigning a comment :#
    \\y
    \\fn foo(a, b) { a + b }
    \\foo(3, 6)
;

pub fn main() anyerror!void {
    var allocator = std.testing.allocator;
    var lexer = Lexer{ .bytes = src };
    var parser = try Parser.init(allocator, &lexer);
    var eval = Eval.init(allocator, &parser);
    defer eval.deinit();
    debug.print("{}\n", .{try eval.evalAll()});
}
