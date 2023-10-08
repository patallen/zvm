const std = @import("std");
const Tokenizer = @import("./Tokenizer.zig");
const Chunk = @import("./Chunk.zig");
const Parse = @import("./Parse.zig");
const Allocator = std.mem.Allocator;

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

pub fn compile(self: *Self) !bool {
    _ = self.p.advance();
    try self.computeExpression(0);
    return !self.hadError;
}

pub fn consume(self: *Self, expected_tag: Tokenizer.Token.Tag, message: []const u8) !void {
    if (self.p.current.tag == expected_tag) {
        try self.advance();
    } else {
        self.errorAtCurrent(message);
    }
}

fn currentChunk(self: *Self) *Chunk {
    return self.chunk;
}

fn emitByte(self: *Self, byte: u8) void {
    self.chunk.writeByte(byte, self.p.previous.loc.lineno);
}

fn emitOp(self: *Self, op: Chunk.Op) void {
    self.chunk.writeOp(op, self.p.previous.loc.lineno);
}

fn emitReturn(self: *Self) void {
    self.chunk.writeOp(.kw_return, self.p.previous.loc.lineno);
}

fn emitConstant(self: *Self, value: Chunk.Value) void {
    self.emitOp(.constant);
    self.emitByte(self.chunk.addConstant(value));
}

fn number(self: *Self) !void {
    var loc = self.p.previous.loc;
    var strval = self.source[loc.start..loc.end];
    var fvalue = try std.fmt.parseFloat(Chunk.Value, strval);
    self.emitConstant(fvalue);
}

fn computeExpression(self: *Self, min_prec: usize) !void {
    _ = min_prec;
    if (self.p.current.tag == .eof) return;
    if (self.p.current.tag == .invalid) {
        self.p.errorAtCurrent("Invalid token");
    }

    try self.computeAtom();
}

fn computeAtom(self: *Self) Allocator.Error!void {
    var current = self.p.current;
    if (current.tag == .minus) {
        _ = self.p.advance();
        try self.computeExpression(1);
        try self.chunk.writeOp(.negate, current.loc.lineno);
    } else if (current.tag == .bang) {
        _ = self.p.advance();
        try self.computeExpression(1);
        try self.chunk.writeOp(.not, current.loc.lineno);
    } else if (current.tag == .l_paren) {
        _ = self.p.advance();
        try self.computeExpression(1);
        if (self.p.current.tag != .r_paren) {
            self.p.errorAtCurrent("Expected closing paren");
        }
    } else if (current.tag == .r_paren) {
        self.p.errorAtCurrent("Invalid token");
    } else if (current.tag == .eof) {
        self.p.errorAtCurrent("Source ended unexpectedly");
    }
}

fn grouping(self: *Self) void {
    self.expression();
    self.consume(.r_paren, "Expected closing paren after expression.");
}

test "Compiler" {
    const Compiler = @This();
    const source = "(1 + 1) * 2 + 3 / 2 + 2";

    var compiler = Compiler.init(std.testing.allocator, source);
    _ = try compiler.compile();
}
