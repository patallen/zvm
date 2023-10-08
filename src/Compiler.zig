const std = @import("std");
const debug = @import("./debug.zig");
const Tokenizer = @import("./Tokenizer.zig");
const Chunk = @import("./Chunk.zig");
const Parse = @import("./Parse.zig");
const Allocator = std.mem.Allocator;
const Value = @import("./value.zig").Value;

const UNARY_PRECEDENCE = 5;

const OpInfo = struct {
    prec: u8,
    assoc: enum { left, right },
};

fn getOpInfo(tok: Tokenizer.Token) ?OpInfo {
    return switch (tok.tag) {
        .lt => .{ .prec = 0, .assoc = .left },
        .gt => .{ .prec = 0, .assoc = .left },
        .eq_eq => .{ .prec = 0, .assoc = .left },
        .bang_eq => .{ .prec = 0, .assoc = .left },
        .plus => .{ .prec = 1, .assoc = .left },
        .minus => .{ .prec = 1, .assoc = .left },
        .star => .{ .prec = 2, .assoc = .left },
        .slash => .{ .prec = 2, .assoc = .left },
        .star_star => .{ .prec = 3, .assoc = .right },
        else => null,
    };
}

allocator: Allocator,
source: []const u8,
chunk: Chunk,
p: Parse,
hadError: bool = false,
panicMode: bool = false,

const Self = @This();

pub fn init(allocator: Allocator, source: []const u8) Self {
    return .{
        .allocator = allocator,
        .p = Parse.init(source),
        .chunk = Chunk.init(allocator),
        .source = source,
    };
}

pub fn deinit(self: *Self) void {
    self.chunk.deinit();
}

pub fn compile(self: *Self) !bool {
    try self.computeExpression(0);
    return !self.hadError;
}

pub fn consume(self: *Self, expected_tag: Tokenizer.Token.Tag, message: []const u8) void {
    if (self.p.current.tag == expected_tag) {
        self.p.advance();
    } else {
        self.p.errorAtCurrent(message);
    }
}

fn currentChunk(self: *Self) *Chunk {
    return self.chunk;
}

fn emitByte(self: *Self, byte: u8) !void {
    try self.chunk.writeByte(byte, self.p.previous.loc.lineno);
}

fn emitOp(self: *Self, op: Chunk.Op) !void {
    try self.chunk.writeOp(op, self.p.previous.loc.lineno);
}

fn emitReturn(self: *Self) !void {
    try self.chunk.writeOp(.kw_return, self.p.previous.loc.lineno);
}

fn emitConstant(self: *Self, value: Value) !void {
    try self.emitOp(.constant);
    try self.emitByte(try self.chunk.addConstant(value));
}

fn number(self: *Self) !void {
    var loc = self.p.previous.loc;
    var strval = self.source[loc.start..loc.end];
    var fvalue = try std.fmt.parseFloat(Chunk.Value, strval);
    try self.emitConstant(fvalue);
}

fn computeExpression(self: *Self, min_prec: usize) !void {
    self.p.advance();
    if (self.p.current.tag == .eof) return;
    if (self.p.current.tag == .invalid) {
        self.p.errorAtCurrent("Invalid token");
        return;
    }

    try self.computeAtom();
    while (true) {
        var current = self.p.current;
        if (current.tag == .eof) break;
        if (getOpInfo(current)) |op_info| {
            if (op_info.prec < min_prec) {
                break;
            }
            var next_min_prec = if (op_info.assoc == .left) op_info.prec + 1 else op_info.prec;
            try self.computeExpression(next_min_prec);
            try self.computeOp(current);
        } else {
            break;
        }
    }
}

fn computeOp(self: *Self, token: Tokenizer.Token) !void {
    switch (token.tag) {
        .star => try self.emitOp(.multiply),
        .star_star => try self.emitOp(.pow),
        .slash => try self.emitOp(.divide),
        .plus => try self.emitOp(.add),
        .minus => try self.emitOp(.subtract),
        .eq_eq => try self.emitOp(.equals),
        .lt => try self.emitOp(.less),
        .gt => try self.emitOp(.greater),
        .bang_eq => {
            try self.emitOp(.equals);
            try self.emitOp(.not);
        },
        else => {
            std.debug.print("reached 'unreachable' op token:{any}\n", .{token.tag});
            unreachable;
        },
    }
}

fn computeUnaryExpression(self: *Self, op: Chunk.Op) !void {
    try self.computeExpression(UNARY_PRECEDENCE);
    try self.emitOp(op);
}

fn computeAtom(self: *Self) error{ ChunkWriteError, InvalidCharacter }!void {
    var current = self.p.current;
    if (getOpInfo(current) != null) {
        self.p.errorAtCurrent("Expected an atom");
    }
    switch (current.tag) {
        .minus => try self.computeUnaryExpression(.negate),
        .bang => try self.computeUnaryExpression(.not),
        .l_paren => {
            try self.computeExpression(0);
            self.consume(.r_paren, "Expected closing paren");
        },
        .r_paren => self.p.errorAtCurrent("Invalid token"),
        .eof => self.p.errorAtCurrent("Source ended unexpectedly"),
        .kw_false => {
            self.p.advance();
            try self.emitOp(.false);
        },
        .kw_true => {
            self.p.advance();
            try self.emitOp(.true);
        },
        .kw_null => {
            self.p.advance();
            try self.emitOp(.null);
        },
        .number_literal => {
            self.p.advance();
            var str = self.source[current.loc.start..current.loc.end];
            const value = try std.fmt.parseFloat(f64, str);
            try self.emitConstant(Value.number(value));
        },
        else => {
            std.debug.print("reached 'unreachable' atom token:{any}: '{s}'\n", .{
                current.tag,
                self.source[current.loc.start..current.loc.end],
            });
            unreachable;
        },
    }
}

test "Compiler" {
    std.debug.print("\n=== Starting Compiler Tests ===\n", .{});
    const Compiler = @This();
    const source = "!(5 - 4 > 3 * 2 == !false)";

    var compiler = Compiler.init(std.testing.allocator, source);
    defer compiler.deinit();
    _ = try compiler.compile();
    try debug.disassembleChunk(&compiler.chunk, "Compiler Test");
}
