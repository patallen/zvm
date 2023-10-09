const std = @import("std");
const Value = @import("./value.zig").Value;

pub const ObjType = enum { string };

pub const Obj = struct {
    ty: Type,

    pub const Type = enum { string };

    pub const String = struct {
        obj: Obj,
        bytes: []const u8,

        pub fn fromObj(obj: *Obj) *String {
            return @fieldParentPtr(String, "obj", obj);
        }

        pub fn create(allocator: std.mem.Allocator, bytes: []const u8) !*String {
            var string = try allocator.create(String);
            var obj = .{ .ty = .string };
            string.* = .{ .obj = obj, .bytes = bytes };
            return string;
        }
    };
};

test "tunning" {
    var allocator = std.heap.page_allocator;

    var string = try Obj.String.create(allocator, "hello");
    defer allocator.destroy(string);

    var value = Value.obj(&string.obj);
    std.debug.print("=============\n", .{});
    std.debug.print("{any}\n", .{value.as.obj});
    std.debug.print("{s}\n", .{string.bytes});
}
