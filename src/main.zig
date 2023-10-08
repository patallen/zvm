const std = @import("std");
const VM = @import("./VM.zig");
const Chunk = @import("./Chunk.zig");

fn repl(vm: *VM) !void {
    var allocator = std.heap.page_allocator;
    var stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    while (true) {
        try stdout.print("zvm > ", .{});
        var input = try stdin.readUntilDelimiterAlloc(allocator, '\n', 512);
        var chunk = Chunk.init(std.heap.page_allocator);
        var res = vm.interpret(input[0..], &chunk);
        _ = res;
    }
}

pub fn main() !void {
    var vm = VM.init();
    try repl(&vm);
}

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}
