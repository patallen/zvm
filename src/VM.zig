const std = @import("std");
const debug = @import("./debug.zig");
const Chunk = @import("./Chunk.zig");
const Op = @import("./Chunk.zig").Op;
const Value = @import("./value.zig").Value;
const Tokenizer = @import("./Tokenizer.zig");
const Compiler = @import("./Compiler.zig");
const Obj = @import("./object.zig").Obj;
const copyString = @import("./object.zig").copyString;
const ObjStringHashMap = @import("./hashmap.zig").ObjStringHashMap;
const concat = @import("./value.zig").concat;

const debuginstructions: bool = false;

allocator: std.mem.Allocator,
ip: usize = 0,
chunk: Chunk = undefined,
arena: std.heap.ArenaAllocator,
sp: u8,
stack: [256]Value,
globals: ObjStringHashMap(Value),

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
        .globals = ObjStringHashMap(Value).init(allocator),
        .arena = std.heap.ArenaAllocator.init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.arena.deinit();
    self.globals.deinit();
}

fn compileToChunk(self: *Self, source: []const u8) error{CompileError}!void {
    // TODO: This is not being deinitialized... lifetimes are weird. Fix it.
    var compiler = Compiler.init(self.arena.allocator(), source);

    var success = compiler.compile() catch {
        return error.CompileError;
    };
    if (!success) {
        return error.CompileError;
    }
    self.chunk = compiler.chunk;
    if (debuginstructions) {
        try debug.disassembleChunk(&self.chunk, "chunk");
    }
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

fn peek(self: *Self, distance: u8) Value {
    return self.stack[self.sp - distance - 1];
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
        // _ = try debug.disassembleInstruction(&self.chunk, self.ip);
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
            .pop => {
                _ = self.pop();
            },
            .define_global => {
                var global_name = self.chunk.getConstant(self.readByte());
                var value = self.pop();
                try self.globals.put(Obj.String.fromObj(global_name.as.obj), value);
            },
            .load_global => {
                var name_value = self.chunk.getConstant(self.readByte());
                var name_string = Obj.String.fromObj(name_value.as.obj);
                if (self.globals.get(name_string)) |value| {
                    self.push(value);
                } else {
                    // runtime error
                    std.debug.print("Undefined variable: '{s}'\n", .{name_string.bytes});
                    return .err;
                }
            },
            .set_global => {
                var name_value = self.chunk.getConstant(self.readByte());
                var name_string = Obj.String.fromObj(name_value.as.obj);
                if (self.globals.contains(name_string)) {
                    try self.globals.put(name_string, self.peek(0));
                } else {
                    // runtime error
                    std.debug.print("Cannot assign to undefined name: '{s}'\n", .{name_string.bytes});
                    return .err;
                }
            },
            .set_local => {
                var index = self.readByte();
                self.stack[index] = self.peek(0);
            },
            .load_local => {
                var index = self.readByte();
                var value = self.stack[index];
                self.push(value);
            },
            .jump_if_false => {
                var jump_offset = self.readWord();
                if (!valIsTruthy(self.peek(0))) {
                    self.ip += jump_offset;
                }
            },
            .jump => {
                var jump_offset = self.readWord();
                self.ip += jump_offset;
            },
            .loop => {
                var loop_offset = self.readWord();
                self.ip -= loop_offset;
            },
        }
    }
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

fn readWord(self: *Self) u16 {
    var lhs: u16 = @intCast(self.readByte());
    var rhs: u16 = @intCast(self.readByte());
    return lhs << 8 | rhs;
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
    var res = try vm.interpret("var x = 10;");
    _ = res;
}
