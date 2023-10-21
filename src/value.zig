const std = @import("std");
const Obj = @import("./Obj.zig");
const copyString = Obj.copyString;
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

    pub fn asFunctionObj(self: *const Value) *Obj.Function {
        assert(self.isObjType(.obj, .function));
        return @fieldParentPtr(Obj.Function, "obj", self.asObj());
    }

    pub fn asNativeObj(self: *const Value) *Obj.Native {
        assert(self.isObjType(.obj, .native));
        return @fieldParentPtr(Obj.Native, "obj", self.asObj());
    }

    pub fn asClosureObj(self: *const Value) *Obj.Closure {
        assert(self.isObjType(.obj, .closure));
        return @fieldParentPtr(Obj.Closure, "obj", self.asObj());
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
                .string => try writer.print("{s}", .{Obj.String.fromObj(self.as.obj).bytes}),
                .native => try writer.print("fn {s}", .{Obj.Function.fromObj(self.as.obj).name.bytes}),
                .function => try writer.print("fn {s}", .{Obj.Function.fromObj(self.as.obj).name.bytes}),
                .closure => try writer.print("fn {s}", .{Obj.Closure.fromObj(self.as.obj).func.name.bytes}),
            },
        };
    }
};

pub fn concat(allocator: std.mem.Allocator, a: Value, b: Value) !Value {
    // TODO: I think we're doing an extra allocation here. Maybe optimize later
    var res = std.ArrayList(u8).init(allocator);
    defer res.deinit();
    _ = try res.writer().write(a.asRawString());
    _ = try res.writer().write(b.asRawString());
    var string_obj = try copyString(allocator, res.items);
    return Value.obj(&string_obj.obj);
}

test "concat" {
    var allocator = std.testing.allocator;
    var bytes = "Hello";
    _ = bytes;

    var a = try copyString(allocator, "Hello,");
    var b = try copyString(allocator, " World!");

    var result = try concat(allocator, Value.obj(&a.obj), Value.obj(&b.obj));
    var string = result.asStringObj();
    defer a.obj.deinit(allocator);
    defer b.obj.deinit(allocator);
    defer string.obj.deinit(allocator);

    try std.testing.expectEqualSlices(u8, "Hello, World!", result.asRawString());
}
