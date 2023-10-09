const std = @import("std");

const print = std.debug.print;

const Chunk = @import("./Chunk.zig");
const Value = @import("./value.zig").Value;
const Op = Chunk.Op;

pub fn disassembleChunk(chunk: *Chunk, name: []const u8) !void {
    var offset: usize = 0;
    print(".\n", .{});
    print("======== {s} ========\n", .{name});
    while (offset < chunk.code.items.len) {
        offset = try disassembleInstruction(chunk, offset);
    }
}

pub fn disassembleInstruction(chunk: *Chunk, offset: usize) !usize {
    const op = chunk.readOp(offset);
    print(" {d:0>4}: ", .{offset});
    var current_line = chunk.lines.items[offset];

    if (offset > 0 and current_line == chunk.lines.items[offset - 1]) {
        print("    |  ", .{});
    } else {
        print(" {d:4}  ", .{current_line});
    }

    return switch (op) {
        .ret => try simpleInstruction("OP_RETURN", offset),
        .constant => try constantInstruction(chunk, offset),
        .negate => try simpleInstruction("OP_NEGATE", offset),
        .add => try simpleInstruction("OP_ADD", offset),
        .subtract => try simpleInstruction("OP_SUBTRACT", offset),
        .multiply => try simpleInstruction("OP_MULTIPLY", offset),
        .divide => try simpleInstruction("OP_DIVIDE", offset),
        .not => try simpleInstruction("OP_NOT", offset),
        .equals => try simpleInstruction("OP_EQUALS", offset),
        .pow => try simpleInstruction("OP_POW", offset),
        .null => try simpleInstruction("OP_NULL", offset),
        .false => try simpleInstruction("OP_FALSE", offset),
        .true => try simpleInstruction("OP_TRUE", offset),
        .greater => try simpleInstruction("OP_GREATER", offset),
        .less => try simpleInstruction("OP_LESS", offset),
        .print => try simpleInstruction("OP_PRINT", offset),
    };
}

pub fn simpleInstruction(name: []const u8, offset: usize) !usize {
    print("{s}\n", .{name});
    return offset + 1;
}

pub fn constantInstruction(chunk: *Chunk, offset: usize) !usize {
    const idx = chunk.readByte(offset + 1);
    const value = chunk.getConstant(idx);
    print("{s:<16}{any}\n", .{ "OP_CONSTANT", value });
    return offset + 2;
}

test "debug" {
    var allocator = std.testing.allocator;
    var chunk = Chunk.init(allocator);
    defer chunk.deinit();

    try chunk.writeOp(.constant, 123);
    try chunk.writeByte(try chunk.addConstant(Value.number(420.69)), 123);
    try chunk.writeOp(.constant, 124);
    try chunk.writeByte(try chunk.addConstant(Value.number(69)), 124);
    try chunk.writeOp(.ret, 124);
    try disassembleChunk(&chunk, "main");
}
