pub const Op = enum(u8) {
    ret,
    constant,
    negate,
    not,
    add,
    subtract,
    multiply,
    divide,
    pow,
    equals,
    greater,
    less,
    false,
    true,
    null,
    print,
    pop,
    define_global,
    load_global,
    set_global,
    load_local,
    set_local,
    jump_if_false,
    jump,
    loop,
    call,
    closure,
    set_upvalue,
    load_upvalue,
};

pub const Byte = u8;

pub const ByteList = std.ArrayList(Byte);
pub const ValueList = std.ArrayList(Value);

code: ByteList,
constants: ValueList,
lines: std.ArrayList(usize),
allocator: std.mem.Allocator,

const Self = @This();
pub fn init(allocator: std.mem.Allocator) Self {
    return .{
        .allocator = allocator,
        .code = ByteList.init(allocator),
        .constants = ValueList.init(allocator),
        .lines = std.ArrayList(usize).init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.code.deinit();
    self.constants.deinit();
    self.lines.deinit();
}

pub fn writeByte(self: *Self, byte: Byte, line: usize) !void {
    try self.code.append(byte);
    try self.lines.append(line);
}

pub fn writeOp(self: *Self, op: Op, line: usize) !void {
    try self.writeByte(@intFromEnum(op), line);
}

pub fn readByte(self: *Self, offset: usize) Byte {
    return self.code.items[offset];
}

pub fn readOp(self: *Self, offset: usize) Op {
    return @enumFromInt(self.readByte(offset));
}

pub fn addConstant(self: *Self, value: Value) !Byte {
    try self.constants.append(value);
    return @as(u8, @intCast(self.constants.items.len - 1));
}

pub fn getConstant(self: *Self, index: Byte) Value {
    return self.constants.items[index];
}

const std = @import("std");
const Value = @import("./value.zig").Value;
