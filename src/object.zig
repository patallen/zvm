const std = @import("std");
const Value = @import("./value.zig").Value;

pub const ObjType = enum { string };

pub const Obj = struct {
    ty: Type,

    pub const Type = enum { string };

    pub const String = struct {
        obj: Obj,
        bytes: []const u8,
        hash: u64,

        pub fn fromObj(obj: *Obj) *String {
            return @fieldParentPtr(String, "obj", obj);
        }

        pub fn deinit(self: *String, allocator: std.mem.Allocator) void {
            allocator.free(self.bytes);
            allocator.destroy(self);
        }
    };
};

pub fn copyString(allocator: std.mem.Allocator, bytes: []const u8) !*Obj.String {
    var strmem = try allocator.alloc(u8, bytes.len);
    @memcpy(strmem, bytes);
    var string_obj = try allocator.create(Obj.String);
    string_obj.* = .{
        .obj = .{ .ty = .string },
        .bytes = strmem,
        .hash = std.hash_map.hashString(strmem),
    };
    return string_obj;
}
