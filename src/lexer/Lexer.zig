const std = @import("std");
const utils = @import("../utils/utils.zig");
const Token = @import("token.zig").Token;
const TokenData = @import("token.zig").TokenData;

/// The character used to denote a builtin function
const BuiltinChar = '@';
/// A map of keywords to their respective tokens
const KeywordMap = std.StaticStringMap(Token).initComptime(.{
    .{ "let", .let },
    .{ "const", .@"const" },
    .{ "fn", .@"fn" },
    .{ "return", .@"return" },
    .{ "if", .@"if" },
    .{ "else", .@"else" },
    .{ "while", .@"while" },
    .{ "for", .@"for" },
    .{ "true", .true },
    .{ "false", .false },
    .{ "null", .null },
    .{ "or", .@"or" },
    .{ "and", .@"and" },
});

const Self = @This();

/// A list of generated tokens from the lexer
tokens: std.ArrayList(TokenData),
/// The current position of the lexer
cursor: utils.Cursor(u8),

pub fn init(input: []const u8, allocator: std.mem.Allocator) Self {
    return .{ .tokens = std.ArrayList(TokenData).init(allocator), .cursor = utils.Cursor(u8).init(input) };
}

pub fn deinit(self: *Self) void {
    self.tokens.deinit();
}

/// Reads as many tokens as possible from the current position
pub fn readAll(self: *Self) ![]const TokenData {
    while (self.read()) |token| {
        try self.tokens.append(token);
    }
    return self.tokens.items;
}

/// Reads a single token from the current position or null if no token could be read
pub fn read(self: *Self) ?TokenData {
    self.skipWhitespace();
    if (!self.cursor.canRead()) {
        return null;
    }
    if (self.readComment()) |token| {
        return token;
    }

    if (self.readChar()) |token| {
        return token;
    }
    return switch (self.cursor.current()) {
        '0'...'9' => self.readNumberlike(),
        'a'...'z', 'A'...'Z', '_' => self.readIdentifier(),
        '"' => self.readString(),
        BuiltinChar => self.readBuiltin(),
        else => |char| blk: {
            defer self.cursor.advance();
            break :blk TokenData.create(.{ .invalid = char }, self.cursor.getCurrentPos(), self.cursor.getCurrentPos());
        },
    };
}

/// Skips all whitespace from the current position
inline fn skipWhitespace(self: *Self) void {
    _ = self.cursor.readWhile(std.ascii.isWhitespace);
}

// Reads tokens from a map of characters to tokens or a fallback if no match was found
const TokenCharMap = struct { u8, Token };

/// Attempts to read a token from a map of characters to tokens or a fallback if no match was found
inline fn readCharMap(self: *Self, comptime map: []const TokenCharMap, fallback: Token) Token {
    const next = self.cursor.peek() orelse return fallback;
    inline for (map) |entry| {
        const key, const value = entry;
        if (next == key) {
            defer self.cursor.advance();
            return value;
        }
    }
    return fallback;
}

/// Attempts to read a comment from the current position or null if no comment was found
fn readComment(self: *Self) ?TokenData {
    if (self.cursor.current() != '/' or self.cursor.peek() != '/') {
        return null;
    }
    const data, const position = self.cursor.readUntil(struct {
        fn check(char: u8) bool {
            return char == '\n';
        }
    }.check);
    return TokenData.create(.{ .comment = std.mem.trim(u8, data, "/") }, position.start, position.end);
}

/// Attempts to read a single/double character token from the current position or null if no token was found
fn readChar(self: *Self) ?TokenData {
    const start = self.cursor.getCurrentPos();
    const data: Token = switch (self.cursor.current()) {
        '+' => self.readCharMap(&.{.{ '=', .plus_assignment }}, .plus),
        '-' => self.readCharMap(&.{.{ '=', .minus_assignment }}, .minus),
        '*' => self.readCharMap(&.{ .{ '*', .doublestar }, .{ '=', .star_assignment } }, .star),
        // we do not need to worry about comments here because we parse them before we get to this point
        '/' => self.readCharMap(&.{.{ '=', .slash_assignment }}, .slash),
        '%' => self.readCharMap(&.{.{ '=', .modulo_assignment }}, .modulo),
        '=' => self.readCharMap(&.{.{ '=', .equal }}, .assignment),
        '!' => self.readCharMap(&.{.{ '=', .not_equal }}, .bang),
        '>' => self.readCharMap(&.{.{ '=', .greater_than_equal }}, .greater_than),
        '<' => self.readCharMap(&.{.{ '=', .less_than_equal }}, .less_than),
        '(' => .left_paren,
        ')' => .right_paren,
        '{' => .left_brace,
        '}' => .right_brace,
        ',' => .comma,
        ';' => .semicolon,
        // if we don't have a match, we return null from the function
        else => return null,
    };

    defer self.cursor.advance();
    return TokenData.create(data, start, self.cursor.getCurrentPos());
}

/// Reads a string literal from the current position
fn readString(self: *Self) TokenData {
    const start = self.cursor.getCurrentPos();
    var escaped = false;
    var seen: u2 = 0;
    while (self.cursor.readAndAdvance()) |char| {
        // don't do anything if we're in an escape sequence
        if (escaped) {
            escaped = false;
            continue;
        }
        switch (char) {
            '\\' => escaped = true,
            '"' => {
                seen += 1;
                if (seen == 2) {
                    break;
                }
            },
            else => {},
        }
    }
    const end = self.cursor.getCurrentPos();
    return TokenData.create(.{ .string = std.mem.trim(u8, self.cursor.input[start..end], "\"") }, start, end);
}

/// Reads a number-like token from the current position
fn readNumberlike(self: *Self) TokenData {
    const data, const position = self.cursor.readWhile(struct {
        fn check(char: u8) bool {
            return std.ascii.isDigit(char) or char == '.' or char == '_';
        }
    }.check);
    return TokenData.create(.{ .number = data }, position.start, position.end);
}

/// Reads a builtin function from the current position
fn readBuiltin(self: *Self) TokenData {
    const start = self.cursor.getCurrentPos();
    std.debug.assert(self.cursor.current() == BuiltinChar);
    self.cursor.advance();
    const data = self.readIdentifier();
    return TokenData.create(.{ .builtin = self.cursor.input[start .. data.position.end + 1] }, start, data.position.end);
}

/// Reads an identifier from the current position
fn readIdentifier(self: *Self) TokenData {
    const data, const position = self.cursor.readWhile(struct {
        fn check(char: u8) bool {
            return std.ascii.isDigit(char) or std.ascii.isAlphanumeric(char) or char == '_';
        }
    }.check);

    return TokenData.create(
        KeywordMap.get(data) orelse .{ .identifier = data },
        position.start,
        position.end,
    );
}

/// A helper function to test the lexer
fn tokenizeAndExpect(input: []const u8, expected: []const TokenData) anyerror!void {
    var lexer = Self.init(input, std.testing.allocator);
    defer lexer.deinit();
    const parsed = try lexer.readAll();
    try std.testing.expectEqualDeep(expected, parsed);
}

test "test simple lexer" {
    try tokenizeAndExpect("1 + 2 - 3;", &.{
        TokenData.create(.{ .number = "1" }, 0, 0),
        TokenData.create(.plus, 2, 2),
        TokenData.create(.{ .number = "2" }, 4, 4),
        TokenData.create(.minus, 6, 6),
        TokenData.create(.{ .number = "3" }, 8, 8),
        TokenData.create(.semicolon, 9, 9),
    });
}

test "test char map lexing" {
    try tokenizeAndExpect("2 ** 3", &.{
        TokenData.create(.{ .number = "2" }, 0, 0),
        TokenData.create(.doublestar, 2, 3),
        TokenData.create(.{ .number = "3" }, 5, 5),
    });
}

test "test keyword lexing" {
    try tokenizeAndExpect("let x = 1", &.{
        TokenData.create(.let, 0, 2),
        TokenData.create(.{ .identifier = "x" }, 4, 4),
        TokenData.create(.assignment, 6, 6),
        TokenData.create(.{ .number = "1" }, 8, 8),
    });
}

test "test builtin lexing" {
    try tokenizeAndExpect("@foo()", &.{
        TokenData.create(.{ .builtin = "@foo" }, 0, 3),
        TokenData.create(.left_paren, 4, 4),
        TokenData.create(.right_paren, 5, 5),
    });
}

test "test string lexing" {
    try tokenizeAndExpect("\"hello world\"", &.{
        TokenData.create(.{ .string = "hello world" }, 0, 13),
    });
}

test "test assignment lexing" {
    try tokenizeAndExpect("x = 1", &.{
        TokenData.create(.{ .identifier = "x" }, 0, 0),
        TokenData.create(.assignment, 2, 2),
        TokenData.create(.{ .number = "1" }, 4, 4),
    });
    try tokenizeAndExpect("x += 1", &.{
        TokenData.create(.{ .identifier = "x" }, 0, 0),
        TokenData.create(.plus_assignment, 2, 3),
        TokenData.create(.{ .number = "1" }, 5, 5),
    });
    try tokenizeAndExpect("x -= 1", &.{
        TokenData.create(.{ .identifier = "x" }, 0, 0),
        TokenData.create(.minus_assignment, 2, 3),
        TokenData.create(.{ .number = "1" }, 5, 5),
    });
    try tokenizeAndExpect("x *= 1", &.{
        TokenData.create(.{ .identifier = "x" }, 0, 0),
        TokenData.create(.star_assignment, 2, 3),
        TokenData.create(.{ .number = "1" }, 5, 5),
    });
    try tokenizeAndExpect("x /= 1", &.{
        TokenData.create(.{ .identifier = "x" }, 0, 0),
        TokenData.create(.slash_assignment, 2, 3),
        TokenData.create(.{ .number = "1" }, 5, 5),
    });
    try tokenizeAndExpect("x %= 1", &.{
        TokenData.create(.{ .identifier = "x" }, 0, 0),
        TokenData.create(.modulo_assignment, 2, 3),
        TokenData.create(.{ .number = "1" }, 5, 5),
    });
}
