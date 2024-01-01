const std = @import("std");
const Token = @import("Lexer.zig").Token;
const TokenData = @import("Lexer.zig").TokenData;

pub const Precedence = enum {
    lowest,
    sum,
    product,
    exponent,
    modulo,
    prefix,
    call,

    pub fn fromToken(token: Token) Precedence {
        return switch (token) {
            .plus, .minus => .sum,
            .star, .slash => .product,
            .doublestar => .exponent,
            .modulo => .modulo,
            inline else => .lowest,
        };
    }

    pub fn lessThan(self: Precedence, other: Precedence) bool {
        return self.value() < other.value();
    }

    pub fn greaterThan(self: Precedence, other: Precedence) bool {
        return self.value() > other.value();
    }

    pub fn value(self: Precedence) u8 {
        return switch (self) {
            inline else => |inner| @intFromEnum(inner),
        };
    }
};

pub const Operator = enum {
    plus,
    minus,
    star,
    slash,
    modulo,
    doublestar,

    pub fn fromTokenData(data: TokenData) !Operator {
        return switch (data.token) {
            .plus => .plus,
            .minus => .minus,
            .star => .star,
            .slash => .slash,
            .modulo => .modulo,
            .doublestar => .doublestar,
            else => error.InvalidOperator,
        };
    }

    pub fn format(self: Operator, _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .plus => try writer.writeAll("+"),
            .minus => try writer.writeAll("-"),
            .star => try writer.writeAll("*"),
            .slash => try writer.writeAll("/"),
            .modulo => try writer.writeAll("%"),
            .doublestar => try writer.writeAll("**"),
        }
    }
};

pub const Program = struct {
    statements: std.ArrayList(Statement),

    pub fn init(allocator: std.mem.Allocator) Program {
        return .{ .statements = std.ArrayList(Statement).init(allocator) };
    }

    pub fn deinit(self: *Program) void {
        self.statements.deinit();
    }

    pub fn add(self: *Program, statement: Statement) !void {
        try self.statements.append(statement);
    }
};

pub const Statement = union(enum) {
    expression: Expression,
    let: LetStatement,

    pub fn format(self: Statement, _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        return switch (self) {
            inline else => |inner| writer.print("{s};", .{inner}),
        };
    }
};

pub const LetStatement = struct {
    name: []const u8,
    expression: Expression,

    pub fn format(self: LetStatement, _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        return writer.print("let {s} = {s};", .{ self.name, self.expression });
    }
};

/// Builtins are functions that are built into the language.
/// They are not user-defined and are used to provide basic functionality.
/// For example, `@print("Hello, world!")` is a builtin statement.
pub const BuiltinExpression = struct {
    name: []const u8,
    arguments: []const Expression,
};

pub const PrefixExpression = struct {
    operator: Operator,
    rhs: *Expression,
};

pub const BinaryExpression = struct {
    lhs: *Expression,
    operator: Operator,
    rhs: *Expression,
};

pub const Expression = union(enum) {
    number: f64,
    identifier: []const u8,
    string: []const u8,
    boolean: bool,
    prefix: PrefixExpression,
    binary: BinaryExpression,
    builtin: BuiltinExpression,

    pub fn format(self: Expression, _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        return switch (self) {
            .number => |inner| writer.print("{d}", .{inner}),
            .identifier => |inner| writer.print("{s}", .{inner}),
            .string => |inner| writer.print("\"{s}\"", .{inner}),
            .boolean => |inner| writer.writeAll(if (inner) "true" else "false"),
            .prefix => |inner| writer.print("({s}{s})", .{ inner.operator, inner.rhs }),
            .binary => |inner| writer.print("({s} {s} {s})", .{ inner.lhs, inner.operator, inner.rhs }),
            .builtin => |inner| {
                try writer.print("@{s}(", .{inner.name});
                for (inner.arguments, 0..) |argument, index| {
                    try writer.print("{s}", .{argument});
                    if (index + 1 < inner.arguments.len) {
                        try writer.writeAll(", ");
                    }
                }
                try writer.writeAll(")");
            },
        };
    }
};
