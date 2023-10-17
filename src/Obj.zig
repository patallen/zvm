const std = @import("std");
const Value = @import("./value.zig").Value;
const Chunk = @import("./Chunk.zig");

ty: Type,

const Self = @This();

pub const Type = enum { string, function, native };

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
