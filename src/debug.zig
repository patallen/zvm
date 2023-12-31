const std = @import("std");

const print = std.debug.print;

const Chunk = @import("./Chunk.zig");
const Value = @import("./value.zig").Value;
const Op = Chunk.Op;

pub fn disassembleChunk(chunk: *Chunk, name: []const u8) !void {
    var offset: usize = 0;
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
        .ret => try simpleInstruction("RETURN", offset),
        .constant => try constantInstruction("CONSTANT", chunk, offset),
        .negate => try simpleInstruction("NEGATE", offset),
        .add => try simpleInstruction("ADD", offset),
        .subtract => try simpleInstruction("SUBTRACT", offset),
        .multiply => try simpleInstruction("MULTIPLY", offset),
        .divide => try simpleInstruction("DIVIDE", offset),
        .not => try simpleInstruction("NOT", offset),
        .equals => try simpleInstruction("EQUALS", offset),
        .pow => try simpleInstruction("POW", offset),
        .null => try simpleInstruction("NULL", offset),
        .false => try simpleInstruction("FALSE", offset),
        .true => try simpleInstruction("TRUE", offset),
        .greater => try simpleInstruction("GREATER", offset),
        .less => try simpleInstruction("LESS", offset),
        .print => try simpleInstruction("PRINT", offset),
        .pop => try simpleInstruction("POP", offset),
        .define_global => try constantInstruction("DEF_GLOBAL", chunk, offset),
        .load_global => try constantInstruction("LOAD_GLOBAL", chunk, offset),
        .set_global => try constantInstruction("SET_GLOBAL", chunk, offset),
        .load_local => try byteInstruction("LOAD_LOCAL", chunk, offset),
        .set_local => try byteInstruction("SET_LOCAL", chunk, offset),
        .jump_if_false => try jumpInstruction("JUMP_IF_FALSE", 1, chunk, offset),
        .jump => try jumpInstruction("JUMP", 1, chunk, offset),
        .loop => try jumpInstruction("LOOP", -1, chunk, offset),
        .call => try byteInstruction("CALL", chunk, offset),
        .closure => {
            var tmp_offset = offset;
            tmp_offset += 1;
            var constant = chunk.code.items[tmp_offset];
            tmp_offset += 1;
            var func_value = chunk.constants.items[constant];
            print("{s:<16}{any} {any}\n", .{ "OP_CLOSURE", constant, func_value });

            var func = func_value.asFunctionObj();
            for (0..func.upvalue_count) |_| {
                var is_local = chunk.code.items[tmp_offset];
                tmp_offset += 1;
                var index = chunk.code.items[tmp_offset];
                tmp_offset += 1;
                print(" {d:0>4}:     |                    {s} {d}\n", .{
                    tmp_offset,
                    if (is_local == 1) "local" else "upvalue",
                    index,
                });
            }
            return tmp_offset;
        },
        .set_upvalue => try byteInstruction("SET_UPVALUE", chunk, offset),
        .load_upvalue => try byteInstruction("LOAD_UPVALUE", chunk, offset),
    };
}
fn byteInstruction(name: []const u8, chunk: *Chunk, offset: usize) !usize {
    var slot = chunk.code.items[offset + 1];
    print("{s:<16}{any}\n", .{ name, slot });
    return offset + 2;
}

fn jumpInstruction(name: []const u8, direction: i16, chunk: *Chunk, offset: usize) !usize {
    var lhs: u16 = @intCast(chunk.code.items[offset + 1]);
    var rhs: u16 = @intCast(chunk.code.items[offset + 2]);
    var word = lhs << 8 | rhs;
    var amount = @as(isize, @intCast(word)) * direction;
    var off = offset + 3;
    var to: isize = @as(isize, @intCast(off)) + amount;
    print("{s:<16}{d:<4} ({d})\n", .{ name, amount, to });
    return off;
}

pub fn simpleInstruction(name: []const u8, offset: usize) !usize {
    print("{s}\n", .{name});
    return offset + 1;
}

pub fn constantInstruction(name: []const u8, chunk: *Chunk, offset: usize) !usize {
    const idx = chunk.readByte(offset + 1);
    const value = chunk.getConstant(idx);
    print("{s:<16}{any}\n", .{ name, value });
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
    try disassembleChunk(&chunk, "main");
}
