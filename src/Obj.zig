const std = @import("std");
const Value = @import("./value.zig").Value;
const Chunk = @import("./Chunk.zig");

ty: Type,

const Self = @This();

pub const Type = enum { string, function, native, closure, upvalue };

pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
    switch (self.ty) {
        .function => {
            const typeobj = Function.fromObj(self);
            typeobj.chunk.deinit();
            allocator.destroy(typeobj);
        },
        .string => {
            const typeobj = String.fromObj(self);
            allocator.free(typeobj.bytes);
            allocator.destroy(typeobj);
        },
        .native => {
            const typeobj = Native.fromObj(self);
            allocator.destroy(typeobj);
        },
        .closure => {
            const typeobj = Closure.fromObj(self);
            allocator.free(typeobj.upvalues);
            allocator.destroy(typeobj);
        },
        .upvalue => {
            const typeobj = Upvalue.fromObj(self);
            allocator.destroy(typeobj);
        },
    }
}

pub const String = struct {
    obj: Self,
    bytes: []const u8,
    hash: u64,

    pub fn fromObj(obj: *Self) *String {
        return @fieldParentPtr(String, "obj", obj);
    }
};

pub const Function = struct {
    obj: Self,
    arity: u8,
    chunk: Chunk,
    name: *Self.String,
    upvalue_count: u8 = 0,

    pub fn fromObj(obj: *Self) *Function {
        return @fieldParentPtr(Function, "obj", obj);
    }

    pub fn init(allocator: std.mem.Allocator) !*Function {
        var self = try allocator.create(Function);
        self.* = .{
            .obj = .{ .ty = .function },
            .arity = 0,
            .chunk = Chunk.init(allocator),
            .name = undefined,
        };
        return self;
    }
};

pub const Closure = struct {
    obj: Self,
    func: *Function,
    upvalues: []*Upvalue,

    pub fn fromObj(obj: *Self) *Closure {
        return @fieldParentPtr(Closure, "obj", obj);
    }

    pub fn init(allocator: std.mem.Allocator, func: *Function) !*Closure {
        var upvalues = try allocator.alloc(*Upvalue, func.upvalue_count);
        var self = try allocator.create(Closure);
        self.* = .{
            .obj = .{ .ty = .closure },
            .func = func,
            .upvalues = upvalues,
        };
        return self;
    }
};

pub const NativeFn = *const fn (arg_count: usize, args: []Value) Value;
pub const Native = struct {
    obj: Self,
    func: NativeFn,

    pub fn fromObj(obj: *Self) *Native {
        return @fieldParentPtr(Native, "obj", obj);
    }

    pub fn init(allocator: std.mem.Allocator, func: NativeFn) !*Native {
        var self = try allocator.create(Native);
        self.* = .{
            .obj = .{ .ty = .native },
            .func = func,
        };
        return self;
    }
};

/// Upvalue stores a pointer to a variable outside the scope of the current function (closures)
pub const Upvalue = struct {
    obj: Self,
    location: *Value,

    pub fn fromObj(obj: *Self) *Upvalue {
        return @fieldParentPtr(Upvalue, "obj", obj);
    }

    pub fn init(allocator: std.mem.Allocator, location: *Value) !*Upvalue {
        var self = try allocator.create(Upvalue);
        self.* = .{
            .obj = .{ .ty = .upvalue },
            .location = location,
        };
        return self;
    }
};

pub fn copyString(allocator: std.mem.Allocator, bytes: []const u8) !*Self.String {
    var strmem = try allocator.alloc(u8, bytes.len);
    @memcpy(strmem, bytes);
    var string_obj = try allocator.create(Self.String);
    string_obj.* = .{
        .obj = .{ .ty = .string },
        .bytes = strmem,
        .hash = std.hash_map.hashString(strmem),
    };
    return string_obj;
}

test Function {
    var func = try Function.init(std.testing.allocator);
    defer func.obj.deinit(std.testing.allocator);
    try std.testing.expectEqual(func.obj.ty, .function);
}
