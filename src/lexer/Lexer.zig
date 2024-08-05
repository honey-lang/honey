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
    .{ "break", .@"break" },
    .{ "continue", .@"continue" },
    .{ "true", .true },
    .{ "false", .false },
    .{ "null", .null },
    .{ "or", .@"or" },
    .{ "and", .@"and" },
});

pub const Data = struct {
    pub const Error = std.mem.Allocator.Error;

    source: []const u8,
    tokens: std.ArrayList(TokenData),
    line_data: std.ArrayList(utils.Span),

    pub fn init(ally: std.mem.Allocator, source: []const u8) Data {
        return .{
            .source = source,
            .tokens = std.ArrayList(TokenData).init(ally),
            .line_data = std.ArrayList(utils.Span).init(ally),
        };
    }

    pub fn deinit(self: *Data) void {
        self.tokens.deinit();
        self.line_data.deinit();
    }

    pub fn addToken(self: *Data, token: TokenData) Error!void {
        try self.tokens.append(token);
    }

    pub fn addLineData(self: *Data, datum: utils.Span) Error!void {
        try self.line_data.append(datum);
    }

    /// Returns a slice of the source based on the line number provided
    pub fn getLineByNum(self: *Data, line_no: usize) ?[]const u8 {
        const line_index = line_no - 1;
        // line index exceeds bounds. we *should* error here but a null is fine for now
        if (line_index >= self.line_data.items.len) return null;

        const line_data = self.line_data.items[line_index];
        return self.source[line_data.start..line_data.end];
    }

    /// Returns a slice of the source based on the line data provided
    pub fn getLineBySpan(self: *Data, line_data: utils.Span) []const u8 {
        // todo: we should probably do error handling
        return self.source[line_data.start..line_data.end];
    }
};

const Self = @This();
const NewLineSeparator = '\n';

/// The allocator used to allocate the data from the lexer
ally: std.mem.Allocator,
/// The data accumulated from the lexer
data: Data,
/// The current position of the lexer
cursor: utils.Cursor(u8),
/// Where the current starting byte offset is
start_byte_offset: usize = 0,

pub fn init(input: []const u8, ally: std.mem.Allocator) Self {
    return .{
        .ally = ally,
        .cursor = utils.Cursor(u8).init(input),
        .data = Data.init(ally, input),
    };
}

pub fn deinit(self: *Self) void {
    self.data.deinit();
}

/// Reads as many tokens as possible from the current position
pub fn readAll(self: *Self) Data.Error!Data {
    while (try self.read()) |token| {
        try self.data.addToken(token);
    }
    return self.data;
}

/// Reads a single token from the current position or null if no token could be read
pub fn read(self: *Self) Data.Error!?TokenData {
    try self.skipWhitespace();
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

/// Returns true if the char is whitespace and
fn isNonNewLineWhitespace(char: u8) bool {
    return std.ascii.isWhitespace(char) and char != NewLineSeparator;
}

/// Skips all whitespace from the current position
fn skipWhitespace(self: *Self) std.mem.Allocator.Error!void {
    // i'd love to get rid of this (potential) infinite loop
    // at the moment, it's here so that we can ensure we can
    // add the last span to the list before we exit the loop
    while (true) : (self.cursor.advance()) {
        // if we encounter the end of the stream, add the last span and break
        if (!self.cursor.canRead()) {
            try self.createLineData();
            break;
        }

        const current = self.cursor.current();
        // break early if we encounter non-whitespace
        if (!std.ascii.isWhitespace(current)) {
            break;
        }

        // if we a new line, add a span to the list and advance the cursor
        if (current == NewLineSeparator) {
            try self.createLineData();
        }
    }
}

/// Creates a span from the Lexer's current `start_byte_offset` and the cursor's current index
/// Has the side effect of updating `start_byte_offset` to the next cursor index
fn createLineData(self: *Self) std.mem.Allocator.Error!void {
    try self.data.addLineData(.{ .start = self.start_byte_offset, .end = self.cursor.index });
    self.start_byte_offset = self.cursor.index + 1;
}

// Reads tokens from a map of characters to tokens or a fallback if no match was found
const TokenStringEntry = struct { []const u8, Token };

/// Attempts to read a token from a map of characters to tokens or a fallback if no match was found
/// This function will advance the cursor by the length of the matched string if a match is found
inline fn readStringMap(self: *Self, comptime map: []const TokenStringEntry, fallback: Token) Token {
    // extremely ugly but we create a slice from the map and dereference it to get an array of entries to sort
    comptime var sorted = map[0..].*;

    // we sort the map by the length of the string in the entry
    comptime std.mem.sort(TokenStringEntry, &sorted, {}, struct {
        fn cmp(_: void, a: TokenStringEntry, b: TokenStringEntry) bool {
            return a.@"0".len > b.@"0".len;
        }
    }.cmp);

    inline for (sorted) |entry| {
        const key, const value = entry;
        const peeked = self.cursor.peekSliceOrNull(key.len);
        if (peeked) |slice| {
            if (std.mem.eql(u8, key, slice)) {
                defer self.cursor.advanceAmount(key.len - 1);
                return value;
            }
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
            return char == NewLineSeparator;
        }
    }.check);
    return TokenData.create(.{ .comment = std.mem.trim(u8, data, "/") }, position.start, position.end);
}

/// Attempts to read a single/double character token from the current position or null if no token was found
fn readChar(self: *Self) ?TokenData {
    const start = self.cursor.getCurrentPos();
    const data: Token = switch (self.cursor.current()) {
        '+' => self.readStringMap(&.{.{ "+=", .plus_assignment }}, .plus),
        '-' => self.readStringMap(&.{.{ "-=", .minus_assignment }}, .minus),
        '*' => self.readStringMap(&.{
            .{ "**=", .doublestar_assignment },
            .{ "**", .doublestar },
            .{ "*=", .star_assignment },
        }, .star),
        // we do not need to worry about comments here because we parse them before we get to this point
        '/' => self.readStringMap(&.{.{ "/=", .slash_assignment }}, .slash),
        '%' => self.readStringMap(&.{.{ "%=", .modulo_assignment }}, .modulo),
        '=' => self.readStringMap(&.{.{ "==", .equal }}, .assignment),
        '!' => self.readStringMap(&.{.{ "!=", .not_equal }}, .bang),
        '>' => self.readStringMap(&.{.{ ">=", .greater_than_equal }}, .greater_than),
        '<' => self.readStringMap(&.{.{ "<=", .less_than_equal }}, .less_than),
        '.' => self.readStringMap(&.{
            .{ "...", .inclusive_range },
            .{ "..", .exclusive_range },
        }, .dot),
        '(' => .left_paren,
        ')' => .right_paren,
        '[' => .left_bracket,
        ']' => .right_bracket,
        '{' => .left_brace,
        '}' => .right_brace,
        ',' => .comma,
        ';' => .semicolon,
        ':' => .colon,
        '|' => .pipe,
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
fn readNumberlike(self: *Self) ?TokenData {
    const start = self.cursor.getCurrentPos();
    while (self.cursor.canRead()) : (self.cursor.advance()) {
        switch (self.cursor.current()) {
            // if we encounter a dot, we need to check if it's a decimal point or a range operator
            '.' => if (self.cursor.peek() == '.') break,
            '0'...'9', '_' => {},
            inline else => break,
        }
    }
    const end = self.cursor.getCurrentPos();
    return TokenData.create(.{ .number = self.cursor.input[start..end] }, start, end - 1);
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
    try std.testing.expectEqualDeep(expected, parsed.tokens.items);
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

test "test list lexing" {
    try tokenizeAndExpect("[1, 2, 33]", &.{
        TokenData.create(.left_bracket, 0, 0),
        TokenData.create(.{ .number = "1" }, 1, 1),
        TokenData.create(.comma, 2, 2),
        TokenData.create(.{ .number = "2" }, 4, 4),
        TokenData.create(.comma, 5, 5),
        TokenData.create(.{ .number = "33" }, 7, 8),
        TokenData.create(.right_bracket, 9, 9),
    });
}

test "ensure readStringMap sorts by length" {
    const ally = std.testing.allocator;
    const map = &[_]TokenStringEntry{
        .{ "..", .exclusive_range },
        .{ "...", .inclusive_range },
    };
    var lexer_1 = Self.init("...", ally);
    defer lexer_1.deinit();
    const token_1 = lexer_1.readStringMap(map, .dot);
    try std.testing.expectEqual(.inclusive_range, token_1);

    var lexer_2 = Self.init("..5", ally);
    defer lexer_2.deinit();
    const token_2 = lexer_2.readStringMap(map, .dot);
    try std.testing.expectEqual(.exclusive_range, token_2);
}
