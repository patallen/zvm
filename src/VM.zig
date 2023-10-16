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
        std.debug.print(" [", .{});
        for (&self.items) |ptr| {
            if (ptr == self.ptr) {
                break;
            }
            std.debug.print("{any}", .{ptr.*});
            if (ptr != self.ptr - 1) {
                std.debug.print(" | ", .{});
            }
        }
        std.debug.print("]\n", .{});
    }
};

allocator: std.mem.Allocator,
chunk: *Chunk = undefined,
arena: std.heap.ArenaAllocator,
globals: ObjStringHashMap(Value),
stack: Stack,
frames: CallStack,

pub const InterpretResult = enum {
    ok,
    err,
};

const Self = @This();

const CallFrame = struct {
    slots: [*]Value,
    ip: usize,
    func: *Obj.Function,
};

pub fn init(allocator: std.mem.Allocator) Self {
    var vm = Self{
        .chunk = undefined,
        .allocator = allocator,
        .globals = ObjStringHashMap(Value).init(allocator),
        .arena = std.heap.ArenaAllocator.init(allocator),
        .stack = .{},
        .frames = .{},
    };
    return vm;
}

fn currentChunk(self: *Self) *Chunk {
    return &self.frames.ptr.func.chunk;
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
    var main = self.compileToChunk(source) catch {
        return .err;
    };
    self.stack.push(Value.obj(&main.obj));
    _ = self.call(main, 0);
    return try self.run();
}

pub fn run(self: *Self) !InterpretResult {
    while (self.frames.ptr.ip < self.currentChunk().code.items.len) {
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
                        .function => {
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
                    self.frames.ptr.ip += jump_offset;
                }
            },
            .jump => {
                var jump_offset = self.readWord();
                self.frames.ptr.ip += jump_offset;
            },
            .loop => {
                var loop_offset = self.readWord();
                self.frames.ptr.ip -= loop_offset;
            },
            .call => {
                var count = self.readByte();
                var func_value = self.stack.peek(count);
                if (!self.callValue(func_value, count)) {
                    return InterpretResult.err;
                }
            },
            .ret => {
                var retval = self.stack.pop();
                self.stack.ptr = self.frames.ptr.slots;
                self.frames.count -= 1;
                self.frames.ptr = &self.frames.items[self.frames.count - 1];
                self.stack.push(retval);
            },
        }
    }
    return .ok;
}

fn call(self: *Self, func: *Obj.Function, arg_count: u8) bool {
    if (arg_count != func.arity) {
        self.runtimeError("Wrong number of arguments provided.", .{});
        return false;
    }

    self.frames.items[self.frames.count] = CallFrame{ .func = func, .ip = 0, .slots = self.stack.ptr - arg_count - 1 };
    self.frames.ptr = &self.frames.items[self.frames.count];
    self.frames.count += 1;
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
    self.runtimeError("Can only call functions and classes.", .{});
    return false;
}

fn runtimeError(self: *Self, comptime message: []const u8, args: anytype) void {
    std.debug.print("Traceback: \n", .{});
    for (self.frames.items, 0..) |fr, frame_no| {
        if (frame_no >= self.frames.count) {
            break;
        }
        std.debug.print("[line {d}] in ", .{fr.func.chunk.lines.items[fr.ip]});
        if (fr.func.name.bytes.len == 0) {
            std.debug.print("<script>\n", .{});
        } else {
            std.debug.print("{s}()\n", .{fr.func.name.bytes});
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
    var op: Op = @enumFromInt(self.readByte());
    return op;
}
fn readByte(self: *Self) u8 {
    var call_frame = self.frames.ptr;
    var byte = self.currentChunk().readByte(call_frame.ip);
    call_frame.ip += 1;
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
