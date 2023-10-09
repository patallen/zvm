const std = @import("std");
const Obj = @import("./object.zig").Obj;
const assert = std.debug.assert;

pub const Value = struct {
    ty: Type,
    as: RawValue,

    pub const Type = enum { bool, number, null, obj };
    const RawValue = union { number: f64, bool: bool, obj: *Obj };

    pub fn boolean(value: bool) Value {
        return .{ .ty = .bool, .as = .{ .bool = value } };
    }

    pub fn number(value: f64) Value {
        return .{ .ty = .number, .as = .{ .number = value } };
    }

    pub fn @"null"() Value {
        return .{ .ty = .null, .as = undefined };
    }

    pub fn obj(ptr: *Obj) Value {
        return .{ .ty = .obj, .as = .{ .obj = ptr } };
    }

    pub fn isType(self: *const Value, ty: Value.Type) bool {
        return self.ty == ty;
    }

    pub fn isObjType(self: *const Value, ty: Value.Type, ot: Obj.Type) bool {
        return self.isType(ty) and self.as.obj.ty == ot;
    }

    pub fn asObj(self: *const Value) *Obj {
        assert(self.isType(.obj));
        return self.as.obj;
    }

    pub fn asStringObj(self: *const Value) *Obj.String {
        assert(self.isObjType(.obj, .string));
        return @fieldParentPtr(Obj.String, "obj", self.asObj());
    }

    pub fn asRawString(self: *const Value) []const u8 {
        return self.asStringObj().bytes;
    }

    pub fn format(self: Value, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        return switch (self.ty) {
            .bool => {
                try writer.print("{}", .{self.as.bool});
            },
            .null => {
                try writer.print("null", .{});
            },
            .number => {
                try writer.print("{d}", .{self.as.number});
            },
            .obj => switch (self.as.obj.ty) {
                .string => try writer.print("\"{s}\"", .{Obj.String.fromObj(self.as.obj).bytes}),
            },
        };
    }
};
