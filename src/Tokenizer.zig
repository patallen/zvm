const std = @import("std");

const KEYWORDS = std.ComptimeStringMap(Token.Tag, .{
    .{ "fun", .kw_fn },
    .{ "class", .kw_class },
    .{ "if", .kw_if },
    .{ "else", .kw_else },
    .{ "true", .kw_true },
    .{ "false", .kw_false },
    .{ "null", .kw_null },
    .{ "and", .kw_and },
    .{ "or", .kw_or },
    .{ "var", .kw_var },
    .{ "while", .kw_while },
    .{ "print", .kw_print },
    .{ "return", .kw_return },
    .{ "super", .kw_super },
});

const Self = @This();

pub const Token = struct {
    tag: Tag,
    loc: Loc,

    pub const Loc = struct {
        start: usize,
        end: usize,
        lineno: usize,
    };

    pub const Tag = enum {
        // single
        l_brace,
        r_brace,
        l_paren,
        r_paren,
        star,
        star_star,
        minus,
        plus,
        semicolon,

        ident,
        number_literal,
        string_literal,

        // maybe multi
        slash,
        bang,
        bang_eq,
        eq,
        eq_eq,
        gt,
        gt_eq,
        lt,
        lt_eq,

        // keywords
        kw_fn,
        kw_class,
        kw_if,
        kw_else,
        kw_true,
        kw_false,
        kw_null,
        kw_and,
        kw_or,
        kw_var,
        kw_while,
        kw_print,
        kw_return,
        kw_super,

        // special
        invalid,
        eof,
    };

    pub fn tagLexeme(self: *Token) []const u8 {
        return switch (self.tag) {
            .kw_true => "true",
            .kw_false => "false",
            .kw_null => "null",
            .bang => "!",
            .bang_eq => "!=",
            .eq => "=",
            .eq_eq => "==",
            .gt => ">",
            .gt_eq => ">=",
            .lt => "<",
            .lt_eq => "<=",
            .l_paren => "(",
            .r_paren => ")",
            .l_brace => "{",
            .r_brace => "}",
            .plus => "+",
            .minus => "-",
            .star => "*",
            .star_star => "**",
            .slash => "/",
            .semicolon => ";",
            .not => "!",
            else => "<tagLexeme not implemented>",
        };
    }
};

buffer: []const u8,
index: usize,
current: usize,
lineno: usize,

prev: Token = undefined,
curr: Token = undefined,

pub fn init(buffer: []const u8) Self {
    return .{
        .buffer = buffer,
        .index = 0,
        .current = 0,
        .lineno = 1,
    };
}

pub fn previous(self: *Self) Token {
    return self.prev;
}

pub fn scanToken(self: *Self) Token {
    var token = Token{
        .tag = .eof,
        .loc = .{
            .lineno = self.lineno,
            .start = self.index,
            .end = undefined,
        },
    };
    var state: State = .start;

    while (true) : (self.index += 1) {
        const c = if (self.index == self.buffer.len) 0 else self.buffer[self.index];
        switch (state) {
            .start => switch (c) {
                0 => {
                    if (self.index != self.buffer.len) {
                        token.tag = .invalid;
                        token.loc.start = self.index;
                        self.index += 1;
                        return token;
                    }
                    break;
                },
                '\n' => {
                    token.loc.start = self.index + 1;
                    self.lineno += 1;
                    token.loc.lineno = self.lineno;
                },
                ' ', '\t', '\r' => {
                    token.loc.start = self.index + 1;
                },
                '*' => {
                    state = .star;
                    token.tag = .star;
                },
                '!' => {
                    state = .bang;
                    token.tag = .bang;
                },
                '<' => {
                    state = .less;
                    token.tag = .lt;
                },
                '>' => {
                    state = .greater;
                    token.tag = .gt;
                },
                '=' => {
                    state = .equal;
                    token.tag = .eq;
                },
                ';' => {
                    token.tag = .semicolon;
                    self.index += 1;
                    break;
                },
                '(' => {
                    token.tag = .l_paren;
                    self.index += 1;
                    break;
                },
                ')' => {
                    token.tag = .r_paren;
                    self.index += 1;
                    break;
                },
                '0'...'9' => {
                    state = .number_int;
                    token.tag = .number_literal;
                },
                '"' => {
                    state = .string;
                    token.tag = .string_literal;
                },
                '+' => {
                    token.tag = .plus;
                    self.index += 1;
                    break;
                },
                '-' => {
                    token.tag = .minus;
                    self.index += 1;
                    break;
                },
                '/' => {
                    // Could potentially be a comment
                    token.tag = .slash;
                    state = .slash;
                },
                '{' => {
                    token.tag = .l_brace;
                    self.index += 1;
                    break;
                },
                '}' => {
                    token.tag = .r_brace;
                    self.index += 1;
                    break;
                },
                else => {
                    if (self.isAlpha(c)) {
                        state = .ident;
                        token.tag = .ident;
                    } else {
                        token.tag = .invalid;
                        token.loc.end = self.index;
                        self.index += 1;
                        return token;
                    }
                },
            },
            .ident => {
                // if it is not alphanumeric, the ident is over. Check for keyword & break at previous byte.
                if (!self.isAlphanumeric(c)) {
                    if (self.getKeyword(token.loc.start, self.index)) |tag| {
                        token.tag = tag;
                    }
                    break;
                }
            },
            .slash => switch (c) {
                '/' => {
                    // Now a comment... eat until newline or EOF
                    state = .comment;
                },
                else => break,
            },
            .comment => switch (c) {
                '\n', 0 => {
                    // Found EOF or newline... go back one and let the tokenizer handle it above
                    self.index -= 1;
                    token.loc.start = self.index;
                    token.tag = .eof;
                    state = .start;
                },
                else => {},
            },
            .star => switch (c) {
                '*' => {
                    token.tag = .star_star;
                    token.loc.end = self.index + 1;
                    self.index += 1;
                    break;
                },
                else => break,
            },
            .greater => switch (c) {
                '=' => {
                    token.tag = .gt_eq;
                    token.loc.end = self.index + 1;
                    self.index += 1;
                    break;
                },
                else => break,
            },
            .less => switch (c) {
                '=' => {
                    token.tag = .lt_eq;
                    token.loc.end = self.index + 1;
                    self.index += 1;
                    break;
                },
                else => break,
            },
            .equal => switch (c) {
                '=' => {
                    token.tag = .eq_eq;
                    token.loc.end = self.index + 1;
                    self.index += 1;
                    break;
                },
                else => break,
            },
            .bang => switch (c) {
                '=' => {
                    token.tag = .bang_eq;
                    token.loc.end = self.index + 1;
                    self.index += 1;
                    break;
                },
                else => break,
            },
            .number_int => switch (c) {
                '.' => {
                    state = .number_dot;
                },
                '0'...'9' => {},
                else => break,
            },
            .number_dot => switch (c) {
                '0'...'9' => {},
                else => break,
            },
            .string => switch (c) {
                '0'...'9', 'a'...'z', 'A'...'Z', '\'', '.', '?', ',', '-', '+' => {},
                '"' => {
                    self.index += 1;
                    break;
                },
                else => {
                    token.tag = .invalid;
                    token.loc.end = self.index;
                    self.index += 1;
                    return token;
                },
            },
        }
    }
    token.loc.end = self.index;
    self.prev = self.curr;
    self.curr = token;
    return token;
}

fn isAlpha(self: *Self, c: u8) bool {
    _ = self;
    return (c >= 'a' and c <= 'z') or
        (c >= 'A' and c <= 'Z') or
        c == '_';
}

fn isNumeric(self: *Self, c: u8) bool {
    _ = self;
    return c >= '0' and c <= '9';
}

fn isAlphanumeric(self: *Self, c: u8) bool {
    return self.isAlpha(c) or self.isNumeric(c);
}

fn getKeyword(self: *Self, start: usize, end: usize) ?Token.Tag {
    var string = self.buffer[start..end];
    std.debug.print("looking for '{s}'\n", .{string});
    return KEYWORDS.get(string);
}

const State = enum {
    start,
    star,
    string,
    number_int,
    number_dot,
    greater,
    less,
    equal,
    bang,
    slash,
    comment,
    ident,
};

test "Tokenizer" {
    const Tokenizer = @This();
    var tokenizer = Tokenizer.init("\n(-1 + 1 - 2.01) / \"hello\";\n!= ! <= < >= > = ==// comment\nclass\n ** *");
    var token = tokenizer.scanToken();
    try std.testing.expectEqual(token.tag, .l_paren);
    try std.testing.expectEqual(
        token.loc,
        .{ .start = 1, .end = 2, .lineno = 2 },
    );

    token = tokenizer.scanToken();
    try std.testing.expectEqual(token.tag, .minus);
    try std.testing.expectEqual(
        token.loc,
        .{ .start = 2, .end = 3, .lineno = 2 },
    );

    token = tokenizer.scanToken();
    try std.testing.expectEqual(token.tag, .number_literal);
    try std.testing.expectEqual(
        token.loc,
        .{ .start = 3, .end = 4, .lineno = 2 },
    );

    token = tokenizer.scanToken();
    try std.testing.expectEqual(token.tag, .plus);
    try std.testing.expectEqual(
        token.loc,
        .{ .start = 5, .end = 6, .lineno = 2 },
    );

    token = tokenizer.scanToken();
    try std.testing.expectEqual(token.tag, .number_literal);
    try std.testing.expectEqual(
        token.loc,
        .{ .start = 7, .end = 8, .lineno = 2 },
    );

    token = tokenizer.scanToken();
    try std.testing.expectEqual(token.tag, .minus);
    try std.testing.expectEqual(
        token.loc,
        .{ .start = 9, .end = 10, .lineno = 2 },
    );

    token = tokenizer.scanToken();
    try std.testing.expectEqual(token.tag, .number_literal);
    try std.testing.expectEqual(
        token.loc,
        .{ .start = 11, .end = 15, .lineno = 2 },
    );

    token = tokenizer.scanToken();
    try std.testing.expectEqual(token.tag, .r_paren);
    try std.testing.expectEqual(
        token.loc,
        .{ .start = 15, .end = 16, .lineno = 2 },
    );

    token = tokenizer.scanToken();
    try std.testing.expectEqual(token.tag, .slash);
    try std.testing.expectEqual(
        token.loc,
        .{ .start = 17, .end = 18, .lineno = 2 },
    );

    token = tokenizer.scanToken();
    try std.testing.expectEqual(token.tag, .string_literal);
    try std.testing.expectEqual(
        token.loc,
        .{ .start = 19, .end = 26, .lineno = 2 },
    );

    token = tokenizer.scanToken();
    try std.testing.expectEqual(token.tag, .semicolon);
    try std.testing.expectEqual(
        token.loc,
        .{ .start = 26, .end = 27, .lineno = 2 },
    );

    token = tokenizer.scanToken();
    try std.testing.expectEqual(token.tag, .bang_eq);
    try std.testing.expectEqual(
        token.loc,
        .{ .start = 28, .end = 30, .lineno = 3 },
    );

    token = tokenizer.scanToken();
    try std.testing.expectEqual(token.tag, .bang);
    try std.testing.expectEqual(
        token.loc,
        .{ .start = 31, .end = 32, .lineno = 3 },
    );

    token = tokenizer.scanToken();
    try std.testing.expectEqual(token.tag, .lt_eq);
    try std.testing.expectEqual(
        token.loc,
        .{ .start = 33, .end = 35, .lineno = 3 },
    );

    token = tokenizer.scanToken();
    try std.testing.expectEqual(token.tag, .lt);
    try std.testing.expectEqual(
        token.loc,
        .{ .start = 36, .end = 37, .lineno = 3 },
    );

    token = tokenizer.scanToken();
    try std.testing.expectEqual(token.tag, .gt_eq);
    try std.testing.expectEqual(
        token.loc,
        .{ .start = 38, .end = 40, .lineno = 3 },
    );

    token = tokenizer.scanToken();
    try std.testing.expectEqual(token.tag, .gt);
    try std.testing.expectEqual(
        token.loc,
        .{ .start = 41, .end = 42, .lineno = 3 },
    );

    token = tokenizer.scanToken();
    try std.testing.expectEqual(token.tag, .eq);
    try std.testing.expectEqual(
        token.loc,
        .{ .start = 43, .end = 44, .lineno = 3 },
    );

    token = tokenizer.scanToken();
    try std.testing.expectEqual(token.tag, .eq_eq);
    try std.testing.expectEqual(
        token.loc,
        .{ .start = 45, .end = 47, .lineno = 3 },
    );

    token = tokenizer.scanToken();
    try std.testing.expectEqual(token.tag, .kw_class);
    try std.testing.expectEqual(
        token.loc,
        .{ .start = 58, .end = 63, .lineno = 4 },
    );
    token = tokenizer.scanToken();
    try std.testing.expectEqual(token.tag, .star_star);
    try std.testing.expectEqual(
        token.loc,
        .{ .start = 65, .end = 67, .lineno = 5 },
    );

    token = tokenizer.scanToken();
    try std.testing.expectEqual(token.tag, .star);
    try std.testing.expectEqual(
        token.loc,
        .{ .start = 68, .end = 69, .lineno = 5 },
    );

    token = tokenizer.scanToken();
    try std.testing.expectEqual(token.tag, .eof);
    try std.testing.expectEqual(
        token.loc,
        .{ .start = 69, .end = 69, .lineno = 5 },
    );
}

test "Tokenizer keywords" {
    const Tokenizer = @This();
    var tokenizer = Tokenizer.init("true != false null");
    var token = tokenizer.scanToken();
    try std.testing.expectEqual(token.tag, .kw_true);

    token = tokenizer.scanToken();
    token = tokenizer.scanToken();
    try std.testing.expectEqual(token.tag, .kw_false);

    token = tokenizer.scanToken();
    try std.testing.expectEqual(token.tag, .kw_null);
}
