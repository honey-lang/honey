const std = @import("std");
const utils = @import("utils.zig");
const Position = utils.Position;

const BuiltinChar = '@';

pub const TokenTag = enum {
    /// .number represents an unparsed number (e.g. "123")
    number,
    /// .identifier represents an identifier (e.g. a variable name)
    identifier,
    /// .string represents a string literal (e.g. "hello world")
    string,
    /// .builtin represents a builtin function (e.g. @print)
    builtin,
    /// .let represents the keyword 'let'
    let,
    /// .true represents the keyword 'true'
    true,
    /// .false represents the keyword 'false'
    false,
    /// .plus represents the character '+'
    plus,
    /// .minus represents the character '-'
    minus,
    /// .star represents the character '*'
    star,
    /// .slash represents the character '/'
    slash,
    /// .modulo represents the character '%'
    modulo,
    /// .doublestar represents the character '**'
    doublestar,
    /// .semicolon represents the character ';'
    semicolon,
    /// .assignment represents the character '='
    assignment,
    /// .comma represents the character ','
    comma,
    /// .left_paren represents the character '('
    left_paren,
    /// .right_paren represents the character ')'
    right_paren,
    /// .invalid represents an invalid character
    invalid,
};

pub const Token = union(TokenTag) {
    number: []const u8,
    identifier: []const u8,
    string: []const u8,
    builtin: []const u8,
    let,
    true,
    false,
    plus,
    minus,
    star,
    slash,
    modulo,
    doublestar,
    semicolon,
    assignment,
    comma,
    left_paren,
    right_paren,
    invalid: u8,

    pub fn format(self: Token, _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        return switch (self) {
            .number => |value| try writer.print("{{ .number = \"{s}\" }}", .{value}),
            .identifier => |value| try writer.print("{{ .identifier = \"{s}\" }}", .{value}),
            .string => |value| try writer.print("{{ .string = \"{s}\" }}", .{value}),
            .builtin => |value| try writer.print("{{ .builtin = \"{s}\" }}", .{value}),
            .invalid => |value| try writer.print("{{ .invalid = '{c}' }}", .{value}),
            inline else => try writer.print(".{s}", .{@tagName(self)}),
        };
    }
};

const KeywordMap = std.ComptimeStringMap(Token, .{
    .{ "let", .let },
    .{ "true", .true },
    .{ "false", .false },
});

pub const TokenData = struct {
    token: Token,
    position: Position,

    pub fn create(token: Token, start: usize, end: usize) TokenData {
        return .{
            .token = token,
            .position = .{ .start = start, .end = end },
        };
    }

    pub fn format(self: TokenData, _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("TokenData {{ .token = {s}, .position = {s} }}", .{ self.token, self.position });
    }
};

pub const Cursor = utils.Cursor(u8);
const Self = @This();

tokens: std.ArrayList(TokenData),
cursor: Cursor,

pub fn init(input: []const u8, allocator: std.mem.Allocator) Self {
    return .{ .tokens = std.ArrayList(TokenData).init(allocator), .cursor = Cursor.init(input) };
}

pub fn deinit(self: *Self) void {
    self.tokens.deinit();
}

pub fn readAll(self: *Self) ![]const TokenData {
    while (self.read()) |token| {
        try self.tokens.append(token);
    }
    return self.tokens.items;
}

pub fn read(self: *Self) ?TokenData {
    self.skipWhitespace();
    if (!self.cursor.canRead()) {
        return null;
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

fn skipWhitespace(self: *Self) void {
    _ = self.cursor.readWhile(std.ascii.isWhitespace);
}

const TokenCharMap = struct { u8, Token };
fn readCharMap(self: *Self, comptime map: []const TokenCharMap, fallback: Token) Token {
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

fn readChar(self: *Self) ?TokenData {
    const start = self.cursor.getCurrentPos();
    const data: Token = switch (self.cursor.current()) {
        '+' => .plus,
        '-' => .minus,
        '*' => self.readCharMap(&.{.{ '*', .doublestar }}, .star),
        '/' => .slash,
        '%' => .modulo,
        '=' => .assignment,
        '(' => .left_paren,
        ')' => .right_paren,
        ',' => .comma,
        ';' => .semicolon,
        else => return null,
    };

    defer self.cursor.advance();
    return TokenData.create(data, start, self.cursor.getCurrentPos());
}

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

fn readNumberlike(self: *Self) TokenData {
    const data, const position = self.cursor.readWhile(struct {
        fn check(char: u8) bool {
            return std.ascii.isDigit(char) or char == '.' or char == '_';
        }
    }.check);
    return TokenData.create(.{ .number = data }, position.start, position.end);
}

fn readBuiltin(self: *Self) TokenData {
    const start = self.cursor.getCurrentPos();
    std.debug.assert(self.cursor.current() == BuiltinChar);
    self.cursor.advance();
    const data = self.readIdentifier();
    return TokenData.create(.{ .builtin = self.cursor.input[start .. data.position.end + 1] }, start, data.position.end);
}

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
