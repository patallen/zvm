const std = @import("std");
const debug = @import("./debug.zig");
const Chunk = @import("./Chunk.zig");
const Op = @import("./Chunk.zig").Op;
const Value = @import("./value.zig").Value;
const Tokenizer = @import("./Tokenizer.zig");
const Compiler = @import("./Compiler.zig");
const Obj = @import("./Obj.zig");
const ObjStringHashMap = @import("./hashmap.zig").ObjStringHashMap;
const concat = @import("./value.zig").concat;
const copyString = Obj.copyString;

const debuginstructions: bool = false;

const FRAMES_MAX: usize = 256;
const STACK_MAX: usize = FRAMES_MAX * @as(usize, @intCast((std.math.maxInt(u8) + 1)));

allocator: std.mem.Allocator,
ip: usize = 0,
chunk: *Chunk = undefined,
arena: std.heap.ArenaAllocator,
sp: u8,
globals: ObjStringHashMap(Value),
stack: [STACK_MAX]Value,
frames: [FRAMES_MAX]CallFrame,
frame_count: usize = 0,

pub const InterpretResult = enum {
    ok,
    err,
};

const Self = @This();

const CallFrame = struct {
    sp: usize,
    ip: usize,
    func: *Obj.Function,
};

pub fn init(allocator: std.mem.Allocator) Self {
    return .{
        .ip = 0,
        .chunk = undefined,
        .sp = 0,
        .allocator = allocator,
        .globals = ObjStringHashMap(Value).init(allocator),
        .arena = std.heap.ArenaAllocator.init(allocator),
        .stack = undefined,
        .frames = undefined,
    };
}

pub fn frame(self: *Self) *CallFrame {
    return &self.frames[self.frame_count - 1];
}

fn currentChunk(self: *Self) *Chunk {
    return &self.frame().func.chunk;
}

pub fn deinit(self: *Self) void {
    self.arena.deinit();
    self.globals.deinit();
}

fn compileToChunk(self: *Self, source: []const u8) error{ CompileError, OutOfMemory }!*Obj.Function {
    // TODO: This is not being deinitialized... lifetimes are weird. Fix it.
    var compiler = try Compiler.init(self.arena.allocator(), source);

    var func = try compiler.compile();
    if (debuginstructions) {
        try debug.disassembleChunk(compiler.currentChunk(), "chunk");
    }
    return func;
}

pub fn interpret(self: *Self, source: []const u8) !InterpretResult {
    var main = self.compileToChunk(source) catch {
        return .err;
    };
    self.push(Value.obj(&main.obj));
    _ = self.call(main, 0);
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
    while (self.frame().ip < self.currentChunk().code.items.len) {
        var instruction = self.readOp();
        switch (instruction) {
            .print => std.debug.print("{any}\n", .{self.pop()}),
            .constant => {
                var constant = self.currentChunk().getConstant(self.readByte());
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
                        .function => {
                            std.debug.print("add operation not supported for functions\n", .{});
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
                var global_name = self.currentChunk().getConstant(self.readByte());
                var value = self.pop();
                try self.globals.put(Obj.String.fromObj(global_name.as.obj), value);
            },
            .load_global => {
                var name_value = self.currentChunk().getConstant(self.readByte());
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
                var name_value = self.currentChunk().getConstant(self.readByte());
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
                var slot = self.frame().sp + self.readByte();
                self.stack[slot] = self.peek(0);
            },
            .load_local => {
                var slot = self.readByte();
                var value = self.stack[self.frame().sp + slot];
                self.push(value);
            },
            .jump_if_false => {
                var jump_offset = self.readWord();
                if (!valIsTruthy(self.peek(0))) {
                    self.frame().ip += jump_offset;
                }
            },
            .jump => {
                var jump_offset = self.readWord();
                self.frame().ip += jump_offset;
            },
            .loop => {
                var loop_offset = self.readWord();
                self.frame().ip -= loop_offset;
            },
            .call => {
                var count = self.readByte();
                var func_value = self.peek(count);
                if (!self.callValue(func_value, count)) {
                    return InterpretResult.err;
                }
            },
            .ret => {
                self.frame_count -= 1;
                var retval = self.pop();
                self.sp = @intCast(self.frames[self.frame_count].sp);
                self.push(retval);
            },
        }
    }
    return .ok;
}

fn call(self: *Self, func: *Obj.Function, arg_count: u8) bool {
    if (arg_count != func.arity) {
        self.runtimeError("Wrong number of arguments provided.");
        return false;
    }

    self.frames[self.frame_count] = CallFrame{
        .func = func,
        .ip = 0,
        .sp = self.sp - arg_count - 1,
    };
    self.frame_count += 1;
    return true;
}

fn callValue(self: *Self, callee: Value, arg_count: u8) bool {
    if (callee.isType(.obj)) {
        switch (callee.as.obj.ty) {
            .function => {
                var func = Obj.Function.fromObj(callee.as.obj);
                return self.call(func, arg_count);
            },
            else => {},
        }
    }
    self.runtimeError("Can only call functions and classes.");
    return false;
}

fn runtimeError(self: *Self, message: []const u8) void {
    std.debug.print("Traceback: \n", .{});
    for (self.frames, 0..) |fr, frame_no| {
        if (frame_no >= self.frame_count) {
            break;
        }
        std.debug.print("[line {d}] in ", .{fr.func.chunk.lines.items[fr.ip]});
        if (fr.func.name.bytes.len == 0) {
            std.debug.print("<script>\n", .{});
        } else {
            std.debug.print("{s}()\n", .{fr.func.name.bytes});
        }
    }
    std.debug.print("ERROR: {s}\n", .{message});
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
    var call_frame = self.frame();
    var byte = self.currentChunk().readByte(call_frame.ip);
    call_frame.ip += 1;
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
