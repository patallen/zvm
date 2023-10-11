const std = @import("std");
const debug = @import("./debug.zig");
const Tokenizer = @import("./Tokenizer.zig");
const Chunk = @import("./Chunk.zig");
const Parse = @import("./Parse.zig");
const Allocator = std.mem.Allocator;
const Value = @import("./value.zig").Value;
const Obj = @import("./object.zig").Obj;
const copyString = @import("./object.zig").copyString;

const UNARY_PRECEDENCE = 5;

const OpInfo = struct {
    prec: u8,
    assoc: enum { left, right },
};

const Local = struct {
    name: Tokenizer.Token,
    depth: ?u8,
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

locals: [256]Local,
local_count: i16,
scope_depth: u8,
allocator: Allocator,
source: []const u8,
chunk: Chunk,
p: Parse,

const Self = @This();

pub fn init(allocator: Allocator, source: []const u8) Self {
    return .{
        .allocator = allocator,
        .p = Parse.init(source),
        .chunk = Chunk.init(allocator),
        .source = source,
        .locals = .{},
        .local_count = 0,
        .scope_depth = 0,
    };
}

pub fn deinit(self: *Self) void {
    self.chunk.deinit();
    self.locals.deinit();
}

pub fn compile(self: *Self) !bool {
    self.p.advance();
    while (!self.match(.eof)) {
        try self.declaration();
    }
    return !self.p.hadError;
}

fn consume(self: *Self, expected_tag: Tokenizer.Token.Tag, message: []const u8) void {
    if (self.p.current.tag == expected_tag) {
        self.p.advance();
    } else {
        self.p.errorAtCurrent(message);
    }
}

fn check(self: *Self, expected_tag: Tokenizer.Token.Tag) bool {
    return self.p.current.tag == expected_tag;
}

fn match(self: *Self, expected_tag: Tokenizer.Token.Tag) bool {
    if (self.check(expected_tag)) {
        self.p.advance();
        return true;
    }
    return false;
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

fn emitGlobal(self: *Self, value: Value) !void {
    _ = value;
    _ = self;
}

fn number(self: *Self) !void {
    var loc = self.p.previous.loc;
    var strval = self.source[loc.start..loc.end];
    var fvalue = try std.fmt.parseFloat(f64, strval);
    try self.emitConstant(Value.number(fvalue));
}

fn computeExpression(self: *Self, min_prec: usize) !void {
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
            self.p.advance();
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

fn synchronize(self: *Self) void {
    self.p.panicMode = false;
    while (self.p.current.tag != .eof) {
        if (self.p.previous.tag == .semicolon) return;
        switch (self.p.current.tag) {
            .kw_print,
            .kw_class,
            .kw_fn,
            .kw_return,
            .kw_if,
            .kw_var,
            .kw_while,
            => break,
            else => {},
        }
        self.p.advance();
    }
}

fn computeUnaryExpression(self: *Self, op: Chunk.Op) !void {
    self.p.advance();
    try self.computeExpression(UNARY_PRECEDENCE);
    try self.emitOp(op);
}

fn declaration(self: *Self) error{ InvalidCharacter, ChunkWriteError, OutOfMemory }!void {
    if (self.match(.kw_var)) {
        try self.variableDeclaration();
    } else {
        try self.statement();
    }
    if (self.p.panicMode) self.synchronize();
}

fn statement(self: *Self) !void {
    if (self.match(.kw_print)) {
        try self.printStatement();
    } else if (self.match(.l_brace)) {
        self.beginScope();
        try self.parseBlock();
        try self.endScope();
    } else {
        try self.expressionStatement();
    }
}

fn resolveLocal(self: *Self, tok: Tokenizer.Token) ?u8 {
    var i: u8 = @intCast(self.local_count - 1);
    while (i > 0) : (i -= 1) {
        if (self.identifiersEqual(tok, self.locals[i].name)) {
            return i - 1;
        }
    }
    return null;
}

fn beginScope(self: *Self) void {
    self.scope_depth += 1;
}

fn endScope(self: *Self) !void {
    self.scope_depth -= 1;
    while (self.local_count > 0) : (self.local_count -= 1) {
        var local = self.locals[@intCast(self.local_count - 1)];
        if (local.depth == null) break;
        if (self.scope_depth >= local.depth.?) break;
        try self.emitOp(.pop);
    }
}

pub fn parseBlock(self: *Self) !void {
    while (!self.check(.r_brace) and !self.check(.eof)) {
        try self.declaration();
    }
    self.consume(.r_brace, "Expected closing '}' to end block.");
}

fn parseVariable(self: *Self, message: []const u8) !u8 {
    self.consume(.ident, message);
    try self.declareVariable();
    if (self.scope_depth > 0) return 0;
    return try self.identifierConstant(&self.p.previous);
}

fn declareVariable(self: *Self) !void {
    var i = self.local_count;
    while (i > 0) : (i -= 1) {
        var local = self.locals[@intCast(i - 1)];
        if (local.depth != null and local.depth.? < self.scope_depth) {
            break;
        }
        if (self.identifiersEqual(local.name, self.p.previous)) {
            std.debug.print("TODO: This should be made an error: Local already exists by that name.", .{});
            return;
        }
    }
    try self.addLocal(self.p.previous);
}

fn identifiersEqual(self: *Self, a: Tokenizer.Token, b: Tokenizer.Token) bool {
    var astr = self.source[a.loc.start..a.loc.end];
    var bstr = self.source[b.loc.start..b.loc.end];
    // TODO: Maybe move to a util?
    if (astr.len != bstr.len) return false;
    return std.mem.eql(u8, astr, bstr);
}

fn identifierConstant(self: *Self, tok: *Tokenizer.Token) !u8 {
    var string_obj = try copyString(self.allocator, self.source[tok.loc.start..tok.loc.end]);
    return self.chunk.addConstant(Value.obj(&string_obj.obj));
}

fn variableDeclaration(self: *Self) !void {
    var global = try self.parseVariable("Expected variable name.");
    self.consume(.eq, "Expected '=' for variable assignment.");
    try self.computeExpression(0);
    self.consume(.semicolon, "Expected ';' following variable declaration.");
    try self.defineVariable(global);
}

fn defineVariable(self: *Self, index: u8) !void {
    if (self.scope_depth > 0) return;
    try self.emitOp(.define_global);
    try self.emitByte(index);
}

fn addLocal(self: *Self, tok: Tokenizer.Token) !void {
    if (self.local_count >= 256) {
        std.debug.print("Too many locals... this should be made an error\n", .{});
        return;
    }
    self.locals[@intCast(self.local_count)] = .{ .depth = self.scope_depth, .name = tok };
    self.local_count += 1;
}

fn expressionStatement(self: *Self) !void {
    try self.computeExpression(0);
    self.consume(.semicolon, "Expected ';' following statement.");
    try self.emitOp(.pop);
}

fn printStatement(self: *Self) !void {
    try self.computeExpression(0);
    self.consume(.semicolon, "Expect ';' following statement.");
    try self.emitOp(.print);
}

fn computeAtom(self: *Self) error{ ChunkWriteError, OutOfMemory, InvalidCharacter }!void {
    var current = self.p.current;
    if (getOpInfo(current) != null) {
        self.p.errorAtCurrent("Expected an atom");
    }
    switch (current.tag) {
        .minus => try self.computeUnaryExpression(.negate),
        .bang => try self.computeUnaryExpression(.not),
        .l_paren => {
            self.p.advance();
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
            try self.number();
        },
        .string_literal => {
            self.p.advance();
            const bytes = self.source[current.loc.start + 1 .. current.loc.end - 1];
            var string_obj = try copyString(self.allocator, bytes);
            try self.emitConstant(Value.obj(&string_obj.obj));
        },
        .ident => {
            self.p.advance();
            try self.namedVariable();
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

fn namedVariable(self: *Self) !void {
    var tok = self.p.previous;
    var arg: u8 = undefined;
    var set_op: Chunk.Op = undefined;
    var load_op: Chunk.Op = undefined;
    if (self.resolveLocal(tok)) |locarg| {
        set_op = .set_local;
        load_op = .load_local;
        arg = locarg;
    } else {
        arg = try self.identifierConstant(&tok);
        set_op = .set_global;
        load_op = .load_global;
    }
    if (self.match(.eq)) {
        try self.computeExpression(0);
        try self.emitOp(set_op);
    } else {
        try self.emitOp(load_op);
    }
    try self.emitByte(arg);
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
