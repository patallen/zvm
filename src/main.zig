const std = @import("std");
const VM = @import("./VM.zig");
const Chunk = @import("./Chunk.zig");

fn repl(allocator: std.mem.Allocator, stdin: anytype, stdout: anytype) !void {
    while (true) {
        try stdout.print("zvm > ", .{});
        var vm = VM.init(allocator);
        var input = try stdin.readUntilDelimiterAlloc(allocator, '\n', 512);
        _ = try vm.interpret(input[0..]);
    }
}

pub fn main() !void {
    var allocator = std.heap.page_allocator;
    var stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    var args = std.process.args();
    _ = args.next();
    if (args.next()) |arg| {
        var file = try std.fs.cwd().openFile(arg, .{});
        var source = try file.readToEndAlloc(allocator, 100000);
        var vm = VM.init(allocator);
        var res = try vm.interpret(source);
        _ = res;
    } else {
        try repl(allocator, stdin, stdout);
    }
}
