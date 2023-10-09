const std = @import("std");
const VM = @import("./VM.zig");
const Chunk = @import("./Chunk.zig");

fn repl(allocator: std.mem.Allocator, stdin: std.io.Reader, stdout: std.io.Writer) !void {
    while (true) {
        try stdout.print("zvm > ", .{});
        var vm = VM.init(allocator);
        var input = try stdin.readUntilDelimiterAlloc(allocator, '\n', 512);
        var res = try vm.interpret(input[0..]);
        _ = res;
        if (vm.sp > 0) {
            try stdout.print("{any}\n", .{vm.stack[vm.sp - 1]});
        }
    }
}

pub fn main() !void {
    var allocator = std.heap.page_allocator;
    var stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    try repl(allocator, stdin, stdout);
}
