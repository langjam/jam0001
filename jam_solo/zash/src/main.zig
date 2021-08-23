const std = @import("std");
const debug = std.debug;
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const Eval = @import("Eval.zig");

pub fn main() anyerror!void {
    var args = std.process.args();
    var allocator = std.testing.allocator;
    _ = args.skip(); // program name.

    const file_name = try args.next(allocator) orelse {
        debug.print("zash Programmming Language v0.1.\n", .{});
        debug.print("#: Where comments are First-C;ass! :#\n\n", .{});
        debug.print("usage: zash <zash_source_file>\n", .{});
        std.os.exit(1);
    };
    defer allocator.free(file_name);

    var file = try std.fs.cwd().openFile(file_name, .{});
    defer file.close();
    var reader = std.io.bufferedReader(file.reader()).reader();
    const src = try reader.readAllAlloc(allocator, 100 * 1024 * 1024);
    defer allocator.free(src);

    var lexer = Lexer{ .bytes = src };
    var parser = try Parser.init(allocator, &lexer);
    var eval = Eval.init(allocator, &parser);
    defer eval.deinit();
    debug.print("{}\n", .{try eval.evalAll()});
}
