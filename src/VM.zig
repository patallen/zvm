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

const CallStack = struct {
    items: [FRAMES_MAX]CallFrame = undefined,
    ptr: *CallFrame = undefined,
    count: usize = 0,
};

const Stack = struct {
    items: [STACK_MAX]Value = undefined,
    ptr: [*]Value = undefined,

    pub fn init(self: *Stack) void {
        self.ptr = &self.items;
    }

    pub fn push(self: *Stack, value: Value) void {
        self.ptr[0] = value;
        self.ptr += 1;
    }

    pub fn pop(self: *Stack) Value {
        self.ptr -= 1;
        return self.ptr[0];
    }

    pub fn peek(self: *Stack, dist: usize) Value {
        return (self.ptr - dist - 1)[0];
    }

    pub fn dump(self: *Stack) void {
        var ptr: [*]Value = &self.items;
        while (ptr != self.ptr) : (ptr += 1) {
            std.debug.print("[", .{});
            std.debug.print("{any}", .{ptr[0]});
            std.debug.print("]", .{});
        }
    }
};

allocator: std.mem.Allocator,
chunk: *Chunk = undefined,
arena: std.heap.ArenaAllocator,
globals: ObjStringHashMap(Value),
stack: Stack,
frames: CallStack,
ip: usize,

pub const InterpretResult = enum {
    ok,
    err,
};

const Self = @This();

const CallFrame = struct {
    slots: [*]Value,
    ip: usize,
    closure: *Obj.Closure,
};

pub fn init(allocator: std.mem.Allocator) Self {
    var vm = Self{
        .ip = 0,
        .chunk = undefined,
        .allocator = allocator,
        .globals = ObjStringHashMap(Value).init(allocator),
        .arena = std.heap.ArenaAllocator.init(allocator),
        .stack = .{},
        .frames = .{},
    };
    return vm;
}

inline fn currentChunk(self: *Self) *Chunk {
    return &self.frames.ptr.closure.func.chunk;
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
    self.stack.init();
    try defineNative(self, "clock", clockNative);
    var main = self.compileToChunk(source) catch {
        return .err;
    };
    // create the main function, push & pop it from the stack for GC purposes
    self.stack.push(Value.obj(&main.obj));
    _ = self.stack.pop();
    var closure = try Obj.Closure.init(self.allocator, main);
    self.stack.push(Value.obj(&closure.obj));
    _ = self.call(closure, 0);
    return try self.run();
}

pub fn run(self: *Self) !InterpretResult {
    while (self.ip < self.currentChunk().code.items.len) {
        if (debuginstructions) {
            std.debug.print("           ", .{});
            self.stack.dump();
            std.debug.print("\n", .{});
            _ = try debug.disassembleInstruction(self.currentChunk(), self.ip);
        }

        var instruction = self.readOp();
        switch (instruction) {
            .print => std.debug.print("{any}\n", .{self.stack.pop()}),
            .constant => {
                var constant = self.currentChunk().getConstant(self.readByte());
                self.stack.push(constant);
            },
            .negate => {
                self.stack.push(Value.number(-self.stack.pop().as.number));
            },
            .add => {
                var b = self.stack.pop();
                var a = self.stack.pop();
                if (a.ty != b.ty) {
                    self.runtimeError("Cannot add '{any}' to '{any}'\n", .{ a, b });
                    return .err;
                }
                switch (a.ty) {
                    .number => self.stack.push(Value.number(a.as.number + b.as.number)),
                    .null => {
                        self.runtimeError("Cannot add 'null' to 'null'\n", .{});
                        return .err;
                    },
                    .obj => switch (a.as.obj.ty) {
                        .string => {
                            var value = try concat(self.arena.allocator(), a, b);
                            self.stack.push(value);
                        },
                        .native, .function, .closure => {
                            self.runtimeError("Cannot add '{any}' to '{any}'\n", .{ a, b });
                            return .err;
                        },
                    },
                    .bool => {
                        self.runtimeError("Cannot add '{any}' to '{any}'\n", .{ a, b });
                        return .err;
                    },
                }
            },
            .subtract => {
                const operand = self.stack.pop().as.number;
                self.stack.push(Value.number(self.stack.pop().as.number - operand));
            },
            .divide => {
                const operand = self.stack.pop().as.number;
                self.stack.push(Value.number(self.stack.pop().as.number / operand));
            },
            .multiply => {
                const operand = self.stack.pop().as.number;
                self.stack.push(Value.number(self.stack.pop().as.number * operand));
            },
            .pow => {
                var operand = self.stack.pop().as.number;
                var operator = self.stack.pop().as.number;
                self.stack.push(Value.number(std.math.pow(f64, operator, operand)));
            },
            .equals => {
                // TODO: We need Value unions
                var b = self.stack.pop();
                var a = self.stack.pop();
                if (a.ty != b.ty) {
                    std.debug.print("CANNOT COMPARE VALUES OF DIFFERENT TYPE\n", .{});
                } else {
                    self.stack.push(switch (a.ty) {
                        .bool => Value.boolean(a.as.bool == b.as.bool),
                        .number => Value.boolean(a.as.number == b.as.number),
                        .null => Value.boolean(true),
                        .obj => Value.boolean(true), // TODO: FIX THIS
                    });
                }
            },
            .greater => {
                var b = self.stack.pop();
                var a = self.stack.pop();
                if (a.ty != .number or b.ty != .number) {
                    self.runtimeError("Cannot test {any} > {any}. Both operants must be numbers.\n", .{ a, b });
                    return .err;
                } else {
                    self.stack.push(Value.boolean(a.as.number > b.as.number));
                }
            },
            .less => {
                var b = self.stack.pop();
                var a = self.stack.pop();
                if (a.ty != .number or b.ty != .number) {
                    self.runtimeError("Cannot test {any} < {any}. Both operants must be numbers.\n", .{ a, b });
                    return .err;
                } else {
                    self.stack.push(Value.boolean(a.as.number < b.as.number));
                }
            },
            .false => {
                self.stack.push(Value.boolean(false));
            },
            .true => {
                self.stack.push(Value.boolean(true));
            },
            .null => {
                self.stack.push(Value.null());
            },
            .not => {
                self.stack.push(Value.boolean(!valIsTruthy(self.stack.pop())));
            },
            .pop => {
                _ = self.stack.pop();
            },
            .define_global => {
                var global_name = self.currentChunk().getConstant(self.readByte());
                var value = self.stack.pop();
                try self.globals.put(Obj.String.fromObj(global_name.as.obj), value);
            },
            .load_global => {
                var name_value = self.currentChunk().getConstant(self.readByte());
                var name_string = Obj.String.fromObj(name_value.as.obj);
                if (self.globals.get(name_string)) |value| {
                    self.stack.push(value);
                } else {
                    // runtime error
                    self.runtimeError("Undefined variable: {s}\n", .{name_string.bytes});
                    return .err;
                }
            },
            .set_global => {
                var name_value = self.currentChunk().getConstant(self.readByte());
                var name_string = Obj.String.fromObj(name_value.as.obj);
                if (self.globals.contains(name_string)) {
                    try self.globals.put(name_string, self.stack.peek(0));
                } else {
                    self.runtimeError("Cannot assign to undefined name: '{s}'\n", .{name_string.bytes});
                    return .err;
                }
            },
            .set_local => {
                var slot = self.readByte();
                self.frames.ptr.slots[slot] = self.stack.peek(0);
            },
            .load_local => {
                var slot = self.readByte();
                var value = self.frames.ptr.slots[slot];
                self.stack.push(value);
            },
            .jump_if_false => {
                var jump_offset = self.readWord();
                if (!valIsTruthy(self.stack.peek(0))) {
                    self.ip += jump_offset;
                    // self.frames.ptr.ip += jump_offset;
                }
            },
            .jump => {
                var jump_offset = self.readWord();
                self.ip += jump_offset;
                // self.frames.ptr.ip += jump_offset;
            },
            .loop => {
                var loop_offset = self.readWord();
                self.ip -= loop_offset;
                // self.frames.ptr.ip -= loop_offset;
            },
            .call => {
                var count = self.readByte();
                var func_value = self.stack.peek(count);
                self.frames.ptr.ip = self.ip;
                if (!self.callValue(func_value, count)) {
                    return InterpretResult.err;
                }
            },
            .ret => {
                var retval = self.stack.pop();
                self.stack.ptr = self.frames.ptr.slots;
                self.frames.count -= 1;
                self.frames.ptr = &self.frames.items[self.frames.count - 1];
                self.ip = self.frames.ptr.ip;
                self.stack.push(retval);
            },
            .closure => {
                var func_value = self.currentChunk().getConstant(self.readByte());
                var function = Obj.Function.fromObj(func_value.as.obj);
                var closure = try Obj.Closure.init(self.allocator, function);
                self.stack.push(Value.obj(&closure.obj));
            },
            .load_upvalue => {},
            .set_upvalue => {},
        }
    }
    return .ok;
}

fn call(self: *Self, closure: *Obj.Closure, arg_count: u8) bool {
    if (arg_count != closure.func.arity) {
        self.runtimeError("Wrong number of arguments provided. Got {d}, expected {d}\n", .{ arg_count, closure.func.arity });
        return false;
    }

    self.frames.items[self.frames.count] = CallFrame{ .closure = closure, .ip = 0, .slots = self.stack.ptr - arg_count - 1 };
    self.frames.ptr = &self.frames.items[self.frames.count];
    self.ip = 0;
    self.frames.count += 1;
    return true;
}

fn defineNative(self: *Self, name: []const u8, function: Obj.NativeFn) !void {
    var name_str = try copyString(self.allocator, name);
    self.stack.push(Value.obj(&name_str.obj));
    var native = try Obj.Native.init(self.allocator, function);
    self.stack.push(Value.obj(&native.obj));
    try self.globals.put(self.stack.items[0].asStringObj(), self.stack.items[1]);
    // push onto the stack and immediately pop (for GC reasons)
    _ = self.stack.pop();
    _ = self.stack.pop();
}

fn clockNative(arg_count: usize, args: []Value) Value {
    _ = args;
    _ = arg_count;
    var ts = std.time.milliTimestamp();
    return Value.number(@floatFromInt(ts));
}

fn callValue(self: *Self, callee: Value, arg_count: u8) bool {
    if (callee.isType(.obj)) {
        switch (callee.as.obj.ty) {
            .closure => {
                var closure = callee.asClosureObj();
                return self.call(closure, arg_count);
            },
            .native => {
                var native = callee.asNativeObj().func;
                var result = native(arg_count, self.stack.ptr[0..arg_count]);
                self.stack.ptr -= arg_count + 1;
                self.stack.push(result);
                return true;
            },
            else => {},
        }
    }
    self.runtimeError("Can only call functions and classes.\n", .{});
    return false;
}

fn runtimeError(self: *Self, comptime message: []const u8, args: anytype) void {
    std.debug.print("Traceback: \n", .{});
    for (self.frames.items, 0..) |fr, frame_no| {
        if (frame_no >= self.frames.count) {
            break;
        }
        std.debug.print("[line {d}] in ", .{fr.closure.func.chunk.lines.items[fr.ip]});
        if (fr.closure.func.name.bytes.len == 0) {
            std.debug.print("<script>\n", .{});
        } else {
            std.debug.print("{s}()\n", .{fr.closure.func.name.bytes});
        }
    }
    std.debug.print("ERROR: " ++ message, args);
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
    return @enumFromInt(self.readByte());
}

fn readByte(self: *Self) u8 {
    var byte = self.currentChunk().readByte(self.ip);
    self.ip += 1;
    return byte;
}

fn readWord(self: *Self) u16 {
    var lhs: u16 = @intCast(self.readByte());
    var rhs: u16 = @intCast(self.readByte());
    return lhs << 8 | rhs;
}

test "vm" {
    const VM = @This();
    var vm = VM.init(std.testing.allocator);
    defer vm.deinit();
    var res = try vm.interpret("var x = 10;");
    _ = res;
}
