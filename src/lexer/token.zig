const std = @import("std");
const Position = @import("../utils/Position.zig");

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
    /// .true represents the keyword 'true'
    true,
    /// .false represents the keyword 'false'
    false,
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
    /// .comma represents the character ','
    comma,
    /// .left_paren represents the character '('
    left_paren,
    /// .right_paren represents the character ')'
    right_paren,
    /// .left_brace represents the character '{'
    left_brace,
    /// .right_brace represents the character '}'
    right_brace,
    /// .invalid represents an invalid character
    invalid,
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
    true,
    false,
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
    assignment,
    plus_assignment,
    minus_assignment,
    star_assignment,
    slash_assignment,
    modulo_assignment,
    comma,
    left_paren,
    right_paren,
    left_brace,
    right_brace,
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

    pub fn isAssignment(self: Token) bool {
        return switch (self) {
            .assignment, .plus_assignment, .minus_assignment, .star_assignment, .slash_assignment, .modulo_assignment => true,
            inline else => false,
        };
    }
};

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
