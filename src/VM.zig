const std = @import("std");
const debug = @import("./debug.zig");
const Chunk = @import("./Chunk.zig");
const Op = @import("./Chunk.zig").Op;
const Value = @import("./Chunk.zig").Value;
const Tokenizer = @import("./Tokenizer.zig");
const Compiler = @import("./Compiler.zig");

ip: usize = 0,
chunk: Chunk = undefined,
stack: [256]Value,
sp: u8,

pub const InterpretResult = enum {
    ok,
    err,
};

const Self = @This();

pub fn init() Self {
    var stack = [_]Value{0} ** 256;
    return .{
        .ip = 0,
        .chunk = undefined,
        .stack = stack,
        .sp = 0,
    };
}

fn compileToChunk(self: *Self, source: []const u8) !void {
    var compiler = Compiler.init(std.heap.page_allocator, source);

    var hadError = try compiler.compile();
    _ = hadError;
    self.chunk = compiler.chunk;
}

pub fn resetChunk(self: *Self, chunk: *Chunk) void {
    self.chunk = chunk;
    self.stack = [_]Value{0} ** 256;
    self.ip = 0;
    self.sp = 0;
}

pub fn interpret(self: *Self, source: []const u8) InterpretResult {
    self.compileToChunk(source) catch {
        return .err;
    };
    debug.disassembleChunk(&self.chunk, "chunk") catch {};
    return self.run();
}

fn dumpStack(self: *Self) void {
    std.debug.print(" [", .{});
    for (0..self.sp) |i| {
        std.debug.print("{d}", .{self.stack[i]});
        if (i != self.sp - 1) {
            std.debug.print(" | ", .{});
        }
    }
    std.debug.print("]\n", .{});
}

pub fn run(self: *Self) InterpretResult {
    while (self.ip < self.chunk.code.items.len) {
        // _ = try debug.disassembleInstruction(self.chunk.self.ip);
        var instruction = self.readOp();
        switch (instruction) {
            .ret => {
                std.debug.print("{d}\n", .{self.pop()});
            },
            .constant => {
                var constant = self.chunk.getConstant(self.readByte());
                self.push(constant);
            },
            .negate => {
                self.push(-self.pop());
            },
            .add => {
                const operand = self.pop();
                self.push(self.pop() + operand);
            },
            .subtract => {
                const operand = self.pop();
                self.push(self.pop() - operand);
            },
            .divide => {
                const operand = self.pop();
                self.push(self.pop() / operand);
            },
            .multiply => {
                const operand = self.pop();
                self.push(self.pop() * operand);
            },
            .pow => {
                var operand = self.pop();
                self.push(std.math.pow(f64, self.pop(), operand));
            },
            .equals => {
                // TODO: We need Value unions
                self.push(if (self.pop() == self.pop()) 1 else 0);
            },
            .greater => {
                // TODO: We need Value unions
                var operand = self.pop();
                self.push(if (self.pop() > operand) 1 else 0);
            },
            .less => {
                // TODO: We need Value unions
                var operand = self.pop();
                self.push(if (self.pop() < operand) 1 else 0);
            },
            .false => {
                // TODO: We need Value unions
                self.push(0);
            },
            .true => {
                // TODO: We need Value unions
                self.push(1);
            },
            .null => {
                // TODO: We need Value unions
                self.push(0);
            },
            .not => {
                // TODO: We need Value unions
                self.push(if (self.pop() == 0) 1 else 0);
            },
        }
    }
    self.dumpStack();
    return .ok;
}

fn readOp(self: *Self) Op {
    var op: Op = @enumFromInt(self.readByte());
    return op;
}
fn readByte(self: *Self) u8 {
    var byte = self.chunk.readByte(self.ip);
    self.ip += 1;
    return byte;
}

fn push(self: *Self, value: Value) void {
    self.stack[self.sp] = value;
    self.sp += 1;
}

fn pop(self: *Self) Value {
    self.sp -= 1;
    var value = self.stack[self.sp];
    return value;
}

test "vm" {
    const VM = @This();
    var vm = VM.init();
    _ = vm;
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();
    try chunk.writeByte(@intFromEnum(Op.constant), 123);
    try chunk.writeByte(try chunk.addConstant(420.69), 123);
    try chunk.writeByte(@intFromEnum(Op.constant), 124);
    try chunk.writeByte(try chunk.addConstant(69), 124);
    try chunk.writeByte(@intFromEnum(Op.add), 125);
    try chunk.writeByte(@intFromEnum(Op.constant), 126);
    try chunk.writeByte(try chunk.addConstant(2.0), 126);
    try chunk.writeByte(@intFromEnum(Op.divide), 127);
    try chunk.writeByte(@intFromEnum(Op.negate), 130);
    try chunk.writeByte(@intFromEnum(Op.ret), 130);
    std.debug.print(".\n\n", .{});
    // var res = vm.interpret(&chunk);
    // std.debug.print("{any}\n", .{res});
}
