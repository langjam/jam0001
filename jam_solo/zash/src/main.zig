const std = @import("std");
const debug = std.debug;
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const Eval = @import("Eval.zig");

const src =
    \\ #: Comments can go to the end of the line...
    \\ y := #: ...or can be assigned to a variable. :#
    \\
    \\ 2 * 4 #: ...or be embedded inline! :# / 2
    \\
    \\ #: This function concatenates comments.
    \\ #: So it receives comments as args and 
    \\ #: returns a coment. First-class baby!
    \\ fn comment_concat(a #: comment :#, b #: comment :#) {
    \\     a ++ b #: `++` is the comment concat operator.
    \\  }
    \\
    \\ comment_concat(#: Hey :#, y)
;

pub fn main() anyerror!void {
    var allocator = std.testing.allocator;
    var lexer = Lexer{ .bytes = src };
    var parser = try Parser.init(allocator, &lexer);
    var eval = Eval.init(allocator, &parser);
    defer eval.deinit();
    debug.print("{}\n", .{try eval.evalAll()});
}
