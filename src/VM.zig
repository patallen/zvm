const std = @import("std");
const debug = @import("./debug.zig");
const Chunk = @import("./Chunk.zig");
const Op = @import("./Chunk.zig").Op;
const Value = @import("./value.zig").Value;
const Tokenizer = @import("./Tokenizer.zig");
const Compiler = @import("./Compiler.zig");
const Obj = @import("./object.zig").Obj;
const copyString = @import("./object.zig").copyString;

ip: usize = 0,
allocator: std.mem.Allocator,
chunk: Chunk = undefined,
stack: [256]Value,
sp: u8,
arena: std.heap.ArenaAllocator,

pub const InterpretResult = enum {
    ok,
    err,
};

const Self = @This();

pub fn init(allocator: std.mem.Allocator) Self {
    var stack = [_]Value{Value.number(0)} ** 256;
    return .{
        .ip = 0,
        .chunk = undefined,
        .stack = stack,
        .sp = 0,
        .allocator = allocator,
        .arena = std.heap.ArenaAllocator.init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.arena.deinit();
}

fn compileToChunk(self: *Self, source: []const u8) !void {
    // TODO: This is not being deinitialized... lifetimes are weird. Fix it.
    var compiler = Compiler.init(self.arena.allocator(), source);

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

pub fn interpret(self: *Self, source: []const u8) !InterpretResult {
    self.compileToChunk(source) catch {
        return .err;
    };
    return try self.run();
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

pub fn run(self: *Self) !InterpretResult {
    while (self.ip < self.chunk.code.items.len) {
        var instruction = self.readOp();
        switch (instruction) {
            .print, .ret => std.debug.print("{any}\n", .{self.pop()}),
            .constant => {
                var constant = self.chunk.getConstant(self.readByte());
                self.push(constant);
            },
            .negate => {
                self.push(Value.number(-self.pop().as.number));
            },
            .add => {
                var b = self.pop();
                var a = self.pop();
                if (a.ty != b.ty) {
                    std.debug.print("Can only add elements of the same type.\n", .{});
                }
                switch (a.ty) {
                    .number => self.push(Value.number(a.as.number + b.as.number)),
                    .null => {
                        std.debug.print("can't add nulls together.\n", .{});
                    },
                    .obj => switch (a.as.obj.ty) {
                        .string => {
                            var value = try concat(self.arena.allocator(), a, b);
                            self.push(value);
                        },
                    },
                    .bool => std.debug.print("can't add two bools together.\n", .{}),
                }
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
                if (a.ty != b.ty) {
                    std.debug.print("CANNOT COMPARE VALUES OF DIFFERENT TYPE\n", .{});
                } else {
                    self.push(switch (a.ty) {
                        .bool => Value.boolean(a.as.bool == b.as.bool),
                        .number => Value.boolean(a.as.number == b.as.number),
                        .null => Value.boolean(true),
                        .obj => Value.boolean(true), // TODO: FIX THIS
                    });
                }
            },
            .greater => {
                var b = self.pop();
                var a = self.pop();
                if (a.ty != .number or b.ty != .number) {
                    std.debug.print("Can only test '>' on two numbers.\n", .{});
                } else {
                    self.push(Value.boolean(a.as.number > b.as.number));
                }
            },
            .less => {
                var b = self.pop();
                var a = self.pop();
                if (a.ty != .number or b.ty != .number) {
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
    return switch (val.ty) {
        .null => false,
        .bool => val.as.bool,
        .number => val.as.number > 0,
        .obj => true,
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
    var vm = VM.init(std.testing.allocator);
    defer vm.deinit();
    var res = try vm.interpret("\"hi\"");
    _ = res;
}

fn concat(allocator: std.mem.Allocator, a: Value, b: Value) !Value {
    // TODO: I think we're doing an extra allocation here. Maybe optimize later
    var res = std.ArrayList(u8).init(allocator);
    defer res.deinit();
    _ = try res.writer().write(a.asRawString());
    _ = try res.writer().write(b.asRawString());
    var string_obj = try copyString(allocator, res.items);
    return Value.obj(&string_obj.obj);
}

test "concat" {
    var allocator = std.testing.allocator;
    var bytes = "Hello";
    _ = bytes;

    var a = try copyString(allocator, "Hello,");
    var b = try copyString(allocator, " World!");

    var result = try concat(allocator, Value.obj(&a.obj), Value.obj(&b.obj));
    var string = result.asStringObj();
    defer a.deinit(allocator);
    defer b.deinit(allocator);
    defer string.deinit(allocator);

    try std.testing.expectEqualSlices(u8, "Hello, World!", result.asRawString());
}
