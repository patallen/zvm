const std = @import("std");

pub const ValueTag = enum { bool, number, null };
pub const Value = struct {
    type: ValueTag,
    as: union {
        number: f64,
        bool: bool,
    },

    pub fn boolean(value: bool) Value {
        return .{ .type = .bool, .as = .{ .bool = value } };
    }

    pub fn number(value: f64) Value {
        return .{ .type = .number, .as = .{ .number = value } };
    }

    pub fn @"null"() Value {
        return .{ .type = .null, .as = undefined };
    }

    pub fn format(self: Value, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        return switch (self.type) {
            .bool => {
                try writer.print("{}", .{self.as.bool});
            },
            .null => {
                try writer.print("null", .{});
            },
            .number => {
                try writer.print("{d}", .{self.as.number});
            },
        };
    }
};
