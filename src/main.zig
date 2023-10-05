const std = @import("std");

fn repl() !void {
    var allocator = std.heap.page_allocator;
    var stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    while (true) {
        try stdout.print("zvm > ", .{});
        var input = try stdin.readUntilDelimiterAlloc(allocator, '\n', 512);
        try stdout.print("IN: {s}\n", .{input});
    }
}

pub fn main() !void {
    try repl();
}

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}
