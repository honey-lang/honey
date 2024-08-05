const std = @import("std");
const utils = @import("../utils/utils.zig");

pub const TokenTag = enum {
    /// .number represents an unparsed number (e.g. "123")
    number,
    /// .identifier represents an identifier (e.g. a variable name)
    identifier,
    /// .string represents a string literal (e.g. "hello world")
    string,
    /// .builtin represents a builtin function (e.g. @print)
    builtin,
    /// .comment represents a comment (e.g. // this is a comment)
    comment,
    /// .let represents the keyword 'let'
    let,
    /// .const represents the keyword 'const'
    @"const",
    /// .fn represents the keyword 'fn'
    @"fn",
    /// .return represents the keyword 'return'
    @"return",
    /// .if represents the keyword 'if'
    @"if",
    /// .else represents the keyword 'else'
    @"else",
    /// .while represents the keyword 'while'
    @"while",
    /// .for represents the keyword 'for'
    @"for",
    /// .break represents the keyword 'break'
    @"break",
    /// .continue represents the keyword 'continue'
    @"continue",
    /// .true represents the keyword 'true'
    true,
    /// .false represents the keyword 'false'
    false,
    /// .null represents the keyword 'null'
    null,
    /// .or represents the keyword 'or'
    @"or",
    /// .and represents the keyword 'and'
    @"and",
    /// .equal represents the characters '=='
    equal,
    /// .not_equal represents the characters '!='
    not_equal,
    /// .greater_than represents the character '>'
    greater_than,
    /// .greater_than_equal represents the characters '>='
    greater_than_equal,
    /// .less_than represents the character '<'
    less_than,
    /// .less_than_equal represents the characters '<='
    less_than_equal,
    /// .bang represents the character '!'
    bang,
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
    /// .colon represents the character ':'
    colon,
    /// .assignment represents the character '='
    assignment,
    /// .plus_assignment represents the characters '+='
    plus_assignment,
    /// .minus_assignment represents the characters '-='
    minus_assignment,
    /// .star_assignment represents the characters '*='
    star_assignment,
    /// .slash_assignment represents the characters '/='
    slash_assignment,
    /// .modulo_assignment represents the characters '%='
    modulo_assignment,
    /// .doublestar_assignment represents the characters '**='
    doublestar_assignment,
    /// .comma represents the character ','
    comma,
    /// .left_paren represents the character '('
    left_paren,
    /// .right_paren represents the character ')'
    right_paren,
    /// .left_bracket represents the character '['
    left_bracket,
    /// .right_bracket represents the character ']'
    right_bracket,
    /// .left_brace represents the character '{'
    left_brace,
    /// .right_brace represents the character '}'
    right_brace,
    /// .dot represents the character '.'
    dot,
    /// .exclusive_range represents the characters '..'
    exclusive_range,
    /// .inclusive_range represents the characters '..='
    inclusive_range,
    /// .pipe represents the character '|'
    pipe,
    /// .invalid represents an invalid character
    invalid,

    pub fn name(self: TokenTag) []const u8 {
        return switch (self) {
            .equal => "==",
            .not_equal => "!=",
            .greater_than => ">",
            .greater_than_equal => ">=",
            .less_than => "<",
            .less_than_equal => "<=",
            .bang => "!",
            .plus => "+",
            .minus => "-",
            .star => "*",
            .slash => "/",
            .modulo => "%",
            .doublestar => "**",
            .semicolon => ";",
            .colon => ":",
            .assignment => "=",
            .plus_assignment => "+=",
            .minus_assignment => "-=",
            .star_assignment => "*=",
            .slash_assignment => "/=",
            .modulo_assignment => "%=",
            .doublestar_assignment => "**=",
            .comma => ",",
            .left_paren => "(",
            .right_paren => ")",
            .left_bracket => "[",
            .right_bracket => "]",
            .left_brace => "{",
            .right_brace => "}",
            .dot => ".",
            .exclusive_range => "..",
            .inclusive_range => "...",
            .pipe => "|",
            inline else => @tagName(self),
        };
    }
};

pub const Token = union(TokenTag) {
    number: []const u8,
    identifier: []const u8,
    string: []const u8,
    builtin: []const u8,
    comment: []const u8,
    let,
    @"const",
    @"fn",
    @"return",
    @"if",
    @"else",
    @"while",
    @"for",
    @"break",
    @"continue",
    true,
    false,
    null,
    @"or",
    @"and",
    equal,
    not_equal,
    greater_than,
    greater_than_equal,
    less_than,
    less_than_equal,
    bang,
    plus,
    minus,
    star,
    slash,
    modulo,
    doublestar,
    semicolon,
    colon,
    assignment,
    plus_assignment,
    minus_assignment,
    star_assignment,
    slash_assignment,
    modulo_assignment,
    doublestar_assignment,
    comma,
    left_paren,
    right_paren,
    left_bracket,
    right_bracket,
    left_brace,
    right_brace,
    dot,
    exclusive_range,
    inclusive_range,
    pipe,
    invalid: u8,

    pub fn tag(self: Token) TokenTag {
        return std.meta.activeTag(self);
    }

    pub fn format(self: Token, _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        return switch (self) {
            .number => |value| try writer.print("{s}", .{value}),
            .identifier => |value| try writer.print("{s}", .{value}),
            .string => |value| try writer.print("\"{s}\"", .{value}),
            .builtin => |value| try writer.print("{s}", .{value}),
            .comment => |comment| try writer.print("// {s}", .{comment}),
            .invalid => |value| try writer.print("'{c}'", .{value}),
            inline else => try writer.print("{s}", .{self.tag().name()}),
        };
    }

    pub fn isAssignment(self: Token) bool {
        return switch (self) {
            .assignment,
            .plus_assignment,
            .minus_assignment,
            .star_assignment,
            .slash_assignment,
            .modulo_assignment,
            .doublestar_assignment,
            => true,
            inline else => false,
        };
    }
};

pub const TokenData = struct {
    token: Token,
    position: utils.Span,

    pub fn create(token: Token, start: usize, end: usize) TokenData {
        return .{
            .token = token,
            .position = .{ .start = start, .end = end },
        };
    }

    pub fn format(self: TokenData, _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("TokenData {{ .token = {s}, .position = {s} }}", .{ self.token, self.position });
    }

    /// Returns a new `TokenData` with the position offset by `value`
    pub fn offset(self: TokenData, value: isize) TokenData {
        var cloned = self.position.clone();
        _ = cloned.offset(value);
        return .{ .token = self.token, .position = cloned };
    }
};
