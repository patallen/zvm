const std = @import("std");
const Value = @import("./value.zig").Value;

ty: Type,

const Self = @This();

pub const Type = enum { string };

pub const String = struct {
    obj: Self,
    bytes: []const u8,
    hash: u64,

    pub fn fromObj(obj: *Self) *String {
        return @fieldParentPtr(String, "obj", obj);
    }

    pub fn deinit(self: *String, allocator: std.mem.Allocator) void {
        allocator.free(self.bytes);
        allocator.destroy(self);
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
