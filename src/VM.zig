const std = @import("std");
const debug = @import("./debug.zig");
const Chunk = @import("./Chunk.zig");
const Op = @import("./Chunk.zig").Op;
const Value = @import("./value.zig").Value;
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
    var stack = [_]Value{Value.number(0)} ** 256;
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
    self.stack = [_]Value{Value.number(0)} ** 256;
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
        std.debug.print("{any}", .{self.stack[i]});
        if (i != self.sp - 1) {
            std.debug.print(" | ", .{});
        }
    }
    std.debug.print("]\n", .{});
}

pub fn run(self: *Self) InterpretResult {
    while (self.ip < self.chunk.code.items.len) {
        var instruction = self.readOp();
        switch (instruction) {
            .ret => {
                std.debug.print("{any}\n", .{self.pop()});
            },
            .constant => {
                var constant = self.chunk.getConstant(self.readByte());
                self.push(constant);
            },
            .negate => {
                self.push(Value.number(-self.pop().as.number));
            },
            .add => {
                const operand = self.pop().as.number;
                self.push(Value.number(self.pop().as.number + operand));
            },
            .subtract => {
                const operand = self.pop().as.number;
                self.push(Value.number(self.pop().as.number - operand));
            },
            .divide => {
                const operand = self.pop().as.number;
                self.push(Value.number(self.pop().as.number / operand));
            },
            .multiply => {
                const operand = self.pop().as.number;
                self.push(Value.number(self.pop().as.number * operand));
            },
            .pow => {
                var operand = self.pop().as.number;
                var operator = self.pop().as.number;
                self.push(Value.number(std.math.pow(f64, operator, operand)));
            },
            .equals => {
                // TODO: We need Value unions
                var b = self.pop();
                var a = self.pop();
                if (a.type != b.type) {
                    std.debug.print("CANNOT COMPARE VALUES OF DIFFERENT TYPE\n", .{});
                } else {
                    self.push(switch (a.type) {
                        .bool => Value.boolean(a.as.bool == b.as.bool),
                        .number => Value.boolean(a.as.number == b.as.number),
                        .null => Value.boolean(true),
                    });
                }
            },
            .greater => {
                var b = self.pop();
                var a = self.pop();
                if (a.type != .number or b.type != .number) {
                    std.debug.print("Can only test '>' on two numbers.\n", .{});
                } else {
                    self.push(Value.boolean(a.as.number > b.as.number));
                }
            },
            .less => {
                var b = self.pop();
                var a = self.pop();
                if (a.type != .number or b.type != .number) {
                    std.debug.print("Can only test '<' on two numbers.", .{});
                } else {
                    self.push(Value.boolean(a.as.number < b.as.number));
                }
            },
            .false => {
                self.push(Value.boolean(false));
            },
            .true => {
                self.push(Value.boolean(true));
            },
            .null => {
                self.push(Value.null());
            },
            .not => {
                self.push(Value.boolean(!valIsTruthy(self.pop())));
            },
        }
    }
    // self.dumpStack();
    return .ok;
}

fn valIsTruthy(val: Value) bool {
    return switch (val.type) {
        .null => false,
        .bool => val.as.bool,
        .number => val.as.number > 0,
    };
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
    var res = vm.interpret("!(5 - 4 > 3 * 2 == !null)");
    _ = res;
}
