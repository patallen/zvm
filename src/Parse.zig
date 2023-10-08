const Self = @This();

tokenizer: Tokenizer,
hadError: bool = false,
panicMode: bool = false,
current: Tokenizer.Token = undefined,
previous: Tokenizer.Token = undefined,

pub fn init(source: []const u8) Self {
    var tokenizer = Tokenizer.init(source);
    return .{ .tokenizer = tokenizer };
}

pub fn advance(self: *Self) void {
    self.previous = self.current;
    while (true) {
        self.current = self.tokenizer.scanToken();
        if (self.current.tag != .invalid) break;
        self.errorAtCurrent("invalid token");
    }
}

pub fn errorAtCurrent(self: *Self, message: []const u8) void {
    self.errorAt(&self.current, message);
}

pub fn errorAtPrevious(self: *Self, message: []const u8) void {
    self.errorAt(&self.previous, message);
}

pub fn errorAt(self: *Self, token: *Tokenizer.Token, message: []const u8) void {
    if (self.panicMode) return;
    self.panicMode = true;
    std.debug.print("[line {d}] Error", .{token.loc.lineno});
    if (token.tag == .eof) {
        std.debug.print(" at end.", .{});
    } else if (token.tag == .invalid) {} else {
        std.debug.print(" at '{s}'", .{self.tokenizer.buffer[token.loc.start..token.loc.end]});
    }

    std.debug.print(" {s}\n", .{message});

    self.hadError = true;
    self.panicMode = true;
}

const std = @import("std");
const Tokenizer = @import("./Tokenizer.zig");

test "Parse" {
    const Parse = @This();

    var parse = Parse.init("1 + 1");
    parse.advance();
    parse.advance();
}
