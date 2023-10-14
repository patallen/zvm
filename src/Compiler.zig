const std = @import("std");
const debug = @import("./debug.zig");
const Tokenizer = @import("./Tokenizer.zig");
const Chunk = @import("./Chunk.zig");
const Parse = @import("./Parse.zig");
const Allocator = std.mem.Allocator;
const Value = @import("./value.zig").Value;
const Obj = @import("./Obj.zig");
const copyString = Obj.copyString;

const Error = error{
    OutOfMemory,
    ChunkWriteError,
    InvalidCharacter,
};

const UNARY_PRECEDENCE = 5;
const LOCALS_MAX: usize = 256;

const OpInfo = struct {
    prec: u8,
    assoc: enum { left, right },
};

const Local = struct {
    name: []const u8,
    // depth is set to null initially, and updated once variable is initialized.
    depth: ?u8,
};

pub const FunctionType = enum {
    script,
    function,
};

pub const CompileContext = struct {
    func: *Obj.Function,
    func_type: FunctionType,
    enclosing: ?*CompileContext,
    locals: [LOCALS_MAX]Local = undefined,
    local_count: u8 = 0,
    scope_depth: u8 = 0,

    pub fn init(func: *Obj.Function, func_type: FunctionType, enclosing: ?*CompileContext) CompileContext {
        return .{
            .func = func,
            .func_type = func_type,
            .enclosing = enclosing,
        };
    }
};

allocator: Allocator,
ctx: CompileContext,
source: []const u8,
p: Parse,

const Self = @This();

fn createFunction(allocator: std.mem.Allocator, name: []const u8) !*Obj.Function {
    var func = try Obj.Function.init(allocator);
    func.name = try Obj.copyString(allocator, name);
    return func;
}

pub fn init(allocator: Allocator, source: []const u8) !Self {
    var func = try createFunction(allocator, "<script>");
    var ctx = CompileContext{
        .func = func,
        .func_type = .script,
        .enclosing = null,
        .local_count = 1,
    };
    ctx.locals[0] = .{ .name = "fake", .depth = 0 };

    return .{
        .ctx = ctx,
        .allocator = allocator,
        .p = Parse.init(source),
        .source = source,
    };
}

pub fn deinit(self: *Self) void {
    _ = self;
    // TODO: Took a while to debug this msising "func name" free.
    // There has got to be a better way of managing this.
    // self.func.name.obj.deinit(self.allocator);
    // self.func.obj.deinit(self.allocator);
}

pub fn compile(self: *Self) error{CompileError}!*Obj.Function {
    self.p.advance();
    while (!self.match(.eof)) {
        self.declaration() catch {
            return error.CompileError;
        };
    }
    if (self.p.hadError) {
        try debug.disassembleChunk(self.currentChunk(), "compiler");
    }
    return self.ctx.func;
}

// Utils
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

fn consume(self: *Self, expected_tag: Tokenizer.Token.Tag, message: []const u8) void {
    if (self.p.current.tag == expected_tag) {
        self.p.advance();
    } else {
        self.p.errorAtCurrent(message);
    }
}

// Bytecode Emitters
fn emitByte(self: *Self, byte: u8) !void {
    try self.currentChunk().writeByte(byte, self.p.previous.loc.lineno);
}

fn emitOp(self: *Self, op: Chunk.Op) !void {
    try self.currentChunk().writeOp(op, self.p.previous.loc.lineno);
}

fn emitReturn(self: *Self) !void {
    try self.currentChunk().writeOp(.kw_return, self.p.previous.loc.lineno);
}

fn emitConstant(self: *Self, value: Value) !void {
    try self.emitOp(.constant);
    try self.emitByte(try self.currentChunk().addConstant(value));
}

// Constant Creation
fn string(self: *Self) !void {
    var tok = self.p.previous;
    const bytes = self.source[tok.loc.start + 1 .. tok.loc.end - 1];
    var string_obj = try copyString(self.allocator, bytes);
    try self.emitConstant(Value.obj(&string_obj.obj));
}

fn number(self: *Self) !void {
    var loc = self.p.previous.loc;
    var strval = self.source[loc.start..loc.end];
    var fvalue = try std.fmt.parseFloat(f64, strval);
    try self.emitConstant(Value.number(fvalue));
}

fn handleInvalidToken(self: *Self) bool {
    if (self.p.current.tag == .invalid) {
        self.p.errorAtCurrent("Invalid token");
        return true;
    }
    return false;
}

fn logicalAnd(self: *Self) !void {
    var jump = try self.emitJump(.jump_if_false);
    try self.emitOp(.pop);
    try self.computeExpression(0);
    self.patchJump(jump);
}

fn logicalOr(self: *Self) !void {
    var else_jump = try self.emitJump(.jump_if_false);
    var then_jump = try self.emitJump(.jump);
    self.patchJump(else_jump);
    try self.emitOp(.pop);
    try self.computeExpression(0);
    self.patchJump(then_jump);
}

fn processOperator(self: *Self, min_prec: usize) Error!bool {
    if (self.check(.eof)) return false;
    var current = self.p.current;
    var op_info = getOpInfo(current) orelse return false;
    var next_min_prec = if (op_info.assoc == .left) op_info.prec + 1 else op_info.prec;

    if (op_info.prec < min_prec) return false;

    self.p.advance();

    switch (current.tag) {
        .kw_or => try self.logicalOr(),
        .kw_and => try self.logicalAnd(),
        else => {
            try self.computeExpression(next_min_prec);
            try self.computeOp(current);
        },
    }
    return true;
}

fn computeExpression(self: *Self, min_prec: usize) !void {
    if (self.check(.eof) or self.handleInvalidToken()) return;
    try self.computeAtom();
    while (true) if (!try self.processOperator(min_prec)) break;
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
    try self.computeExpression(UNARY_PRECEDENCE);
    try self.emitOp(op);
}

fn declaration(self: *Self) Error!void {
    if (self.match(.kw_fn)) {
        try self.functionDeclaration();
    } else if (self.match(.kw_var)) {
        try self.variableDeclaration();
    } else {
        try self.statement();
    }
    if (self.p.panicMode) self.synchronize();
}

fn ifStatement(self: *Self) Error!void {
    self.consume(.l_paren, "Expected opening paren.");
    try self.computeExpression(0);
    self.consume(.r_paren, "Expected closing paren.");

    // Emit jump for false condition
    var then_jump = try self.emitJump(.jump_if_false);

    // Pop the condition if it was true
    try self.emitOp(.pop);

    // True block
    try self.statement();

    // Emit jump to skip else block
    var else_jump = try self.emitJump(.jump);

    // Patch the jump for false condition
    self.patchJump(then_jump);

    // Else block
    if (self.match(.kw_else)) try self.statement();

    // Patch the jump to skip else block
    self.patchJump(else_jump);
}

fn emitLoop(self: *Self, loop_start: usize) !void {
    var current_ip = self.currentChunk().code.items.len + 2;
    var loop_offset = current_ip - loop_start;
    if (loop_offset > std.math.maxInt(u16)) {
        // TODO: Proper error handling
        std.debug.print("Loop to big", .{});
    }
    var rhs: u8 = @intCast(loop_offset & 0xFF);
    var lhs: u8 = @intCast(loop_offset >> 8 & 0xFF);
    try self.emitOp(.loop);
    try self.emitByte(lhs);
    try self.emitByte(rhs);
}

fn emitJump(self: *Self, op: Chunk.Op) !usize {
    try self.emitOp(op);
    try self.emitByte(0xFF);
    try self.emitByte(0xFF);
    return self.currentChunk().code.items.len - 2;
}

fn patchJump(self: *Self, jump_ip: usize) void {
    var jump_to = self.currentChunk().code.items.len;
    var offset = jump_to - jump_ip - 2;
    var rhs: u8 = @intCast(offset & 0xFF);
    var lhs: u8 = @intCast(offset >> 8 & 0xFF);
    self.currentChunk().code.items[jump_ip] = lhs;
    self.currentChunk().code.items[jump_ip + 1] = rhs;
}

fn statement(self: *Self) Error!void {
    if (self.match(.kw_if)) {
        try self.ifStatement();
    } else if (self.match(.kw_for)) {
        try self.forStatement();
    } else if (self.match(.kw_while)) {
        try self.whileStatement();
    } else if (self.match(.kw_print)) {
        try self.printStatement();
    } else if (self.match(.l_brace)) {
        self.beginScope();
        try self.parseBlock();
        try self.endScope();
    } else {
        try self.expressionStatement();
    }
}

fn resolveLocal(self: *Self, tok: *Tokenizer.Token) ?u8 {
    var i = self.ctx.local_count;
    while (i > 0) : (i -= 1) {
        var local = self.ctx.locals[i - 1];
        var name = self.source[tok.loc.start..tok.loc.end];
        if (identifiersEqual(local.name, name)) {
            if (local.depth == null) {
                self.p.errorAt(tok, "variable not fully initialized");
            }
            return i - 1;
        }
    }
    return null;
}

fn beginScope(self: *Self) void {
    self.ctx.scope_depth += 1;
}

fn endScope(self: *Self) !void {
    // "Remove" locals at the current scope depth and emit pop operation codes so that
    // local values are removed from scope at runtime.
    self.ctx.scope_depth -= 1;
    while (self.ctx.local_count > 0) : (self.ctx.local_count -= 1) {
        var local = self.ctx.locals[self.ctx.local_count - 1];
        if (local.depth == null) break;
        if (self.ctx.scope_depth >= local.depth.?) break;
        try self.emitOp(.pop);
    }
}

fn whileStatement(self: *Self) !void {
    self.consume(.l_paren, "Expected opening paren after while");
    var loop_to = self.currentChunk().code.items.len - 1;
    try self.computeExpression(0);
    self.consume(.r_paren, "Expected closing paren at end of while statement");
    var jump = try self.emitJump(.jump_if_false);
    try self.emitOp(.pop);
    try self.statement();
    try self.emitLoop(loop_to);
    self.patchJump(jump);
}

fn forStatement(self: *Self) !void {
    self.beginScope();

    // initializer
    self.consume(.l_paren, "Expected opening paren after while");
    if (self.match(.kw_var)) {
        try self.variableDeclaration();
    } else {
        self.consume(.semicolon, "Expected semicolon if no variable declaration.");
    }

    // condition
    // jump here to check condition
    var loop_start = self.currentChunk().code.items.len - 1;
    try self.computeExpression(0);
    self.consume(.semicolon, "Expected semicolon after loop condition.");
    var exit_jump = try self.emitJump(.jump_if_false);
    try self.emitOp(.pop);
    var body_jump = try self.emitJump(.jump);

    // increment
    var incr_start = self.currentChunk().code.items.len - 1;
    try self.computeExpression(0);
    self.consume(.r_paren, "Expected closing paren after for statement.");
    try self.emitLoop(loop_start);

    // body
    self.patchJump(body_jump);
    try self.statement();

    try self.emitLoop(incr_start);
    self.patchJump(exit_jump);
    try self.emitOp(.pop);
    try self.endScope();
}

fn parseBlock(self: *Self) !void {
    while (!self.check(.r_brace) and !self.check(.eof)) {
        try self.declaration();
    }
    self.consume(.r_brace, "Expected closing '}' to end block.");
}

fn parseVariable(self: *Self, message: []const u8) !u8 {
    self.consume(.ident, message);
    try self.declareVariable();
    if (self.ctx.scope_depth > 0) return 0;
    return try self.identifierConstant(&self.p.previous);
}

fn declareVariable(self: *Self) !void {
    if (self.ctx.scope_depth == 0) return;
    var i = self.ctx.local_count;
    while (i > 0) : (i -= 1) {
        var local = self.ctx.locals[@intCast(i - 1)];
        if (local.depth != null and local.depth.? < self.ctx.scope_depth) {
            break;
        }
        var loc = self.p.previous.loc;
        if (identifiersEqual(local.name, self.source[loc.start..loc.end])) {
            std.debug.print("TODO: This should be made an error: Local already exists by that name.", .{});
            return;
        }
    }
    try self.addLocal(self.p.previous);
}

fn identifierConstant(self: *Self, tok: *Tokenizer.Token) !u8 {
    var string_obj = try copyString(self.allocator, self.source[tok.loc.start..tok.loc.end]);
    return self.currentChunk().addConstant(Value.obj(&string_obj.obj));
}

fn variableDeclaration(self: *Self) !void {
    var global = try self.parseVariable("Expected variable name.");
    self.consume(.eq, "Expected '=' for variable assignment.");
    try self.computeExpression(0);
    self.consume(.semicolon, "Expected ';' following variable declaration.");
    try self.defineVariable(global);
}

fn functionDeclaration(self: *Self) !void {
    var global = try self.parseVariable("Expect function name.");
    self.markLocalInitialized();
    try self.function(.function);
    try self.defineVariable(global);
}

fn function(self: *Self, func_type: FunctionType) !void {
    var loc = self.p.current.loc;
    var func = try createFunction(self.allocator, self.source[loc.start..loc.end]);
    var ctx = .{
        .func = func,
        .func_type = func_type,
        .enclosing = &self.ctx,
    };
    self.ctx = ctx;

    self.consume(.l_paren, "Expected open paren following function decl");
    self.consume(.r_paren, "Expected closing paren after function params");
    self.consume(.l_brace, "Expected '{' for after function decl;");
    try self.computeExpression(0);
    self.consume(.semicolon, "Expected ';' following variable declaration.");
    self.ctx = self.ctx.enclosing.?.*;
}

fn defineVariable(self: *Self, index: u8) !void {
    if (self.ctx.scope_depth > 0) {
        self.markLocalInitialized();
        return;
    }
    try self.emitOp(.define_global);
    try self.emitByte(index);
}

fn addLocal(self: *Self, tok: Tokenizer.Token) !void {
    if (self.ctx.local_count >= 256) {
        std.debug.print("Too many locals... this should be made an error\n", .{});
        return;
    }
    var name = self.source[tok.loc.start..tok.loc.end];
    self.ctx.locals[@intCast(self.ctx.local_count)] = .{ .depth = null, .name = name };
    self.ctx.local_count += 1;
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

fn computeAtom(self: *Self) Error!void {
    var current = self.p.current;
    if (getOpInfo(current) != null) {
        self.p.errorAtCurrent("Expected an atom");
    }
    self.p.advance();
    switch (current.tag) {
        .minus => try self.computeUnaryExpression(.negate),
        .bang => try self.computeUnaryExpression(.not),
        .l_paren => {
            try self.computeExpression(0);
            self.consume(.r_paren, "Expected closing paren");
        },
        .r_paren => self.p.errorAtPrevious("Invalid token"),
        .eof => self.p.errorAtPrevious("Source ended unexpectedly"),
        .kw_false => try self.emitOp(.false),
        .kw_true => try self.emitOp(.true),
        .kw_null => try self.emitOp(.null),
        .number_literal => try self.number(),
        .string_literal => try self.string(),
        .ident => try self.namedVariable(),
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
    if (self.resolveLocal(&tok)) |locarg| {
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

fn markLocalInitialized(self: *Self) void {
    self.ctx.locals[@intCast(self.ctx.local_count - 1)].depth = self.ctx.scope_depth;
}

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
        .kw_and => .{ .prec = 0, .assoc = .left },
        .kw_or => .{ .prec = 0, .assoc = .left },
        else => null,
    };
}

pub fn currentChunk(self: *Self) *Chunk {
    return &self.ctx.func.chunk;
}

test "Compiler" {
    std.debug.print("\n=== Starting Compiler Tests ===\n", .{});
    const Compiler = @This();
    const source = "!(5 - 4 > 3 * 2 == !false);";

    var compiler = try Compiler.init(std.testing.allocator, source);
    defer compiler.deinit();
    _ = try compiler.compile();
    // try debug.disassembleChunk(&compiler.chunk, "Compiler Test");
}

fn identifiersEqual(a: []const u8, b: []const u8) bool {
    // TODO: Maybe move to a util?
    if (a.len != b.len) return false;
    return std.mem.eql(u8, a, b);
}
