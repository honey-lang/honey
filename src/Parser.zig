const std = @import("std");
const Lexer = @import("Lexer.zig");
const TokenData = Lexer.TokenData;
const Token = Lexer.Token;
const TokenTag = Lexer.TokenTag;

const ast = @import("ast.zig");
const Program = ast.Program;
const Statement = ast.Statement;
const Expression = ast.Expression;
const Precedence = ast.Precedence;
const Operator = ast.Operator;

const honey = @import("honey.zig");
const utils = @import("utils.zig");

pub const ParserError = error{
    UnexpectedToken,
    UnexpectedEOF,
    NoPrefixParseRule,
    NoInfixParseRule,
    OutOfMemory,
    NumberParseFailure,
    ExpectedCurrentMismatch,
    ExpectedPeekMismatch,
};

const ErrorData = struct {
    err: ParserError,
    msg: []const u8,
};

pub const Cursor = utils.Cursor(TokenData);
const Self = @This();

arena: std.heap.ArenaAllocator,
errors: std.ArrayList(ErrorData) = undefined,
cursor: Cursor,

/// Initializes the parser.
pub fn init(tokens: []const TokenData, ally: std.mem.Allocator) Self {
    return .{
        .arena = std.heap.ArenaAllocator.init(ally),
        .cursor = Cursor.init(tokens),
    };
}

/// Deinitializes the parser.
pub fn deinit(self: *Self) void {
    self.errors.deinit();
    self.arena.deinit();
}

pub fn allocator(self: *Self) std.mem.Allocator {
    return self.arena.allocator();
}

/// Allocates a new value on the heap and returns a pointer to it.
fn moveToHeap(self: *Self, value: anytype) ParserError!*@TypeOf(value) {
    const ptr = self.allocator().create(@TypeOf(value)) catch return error.OutOfMemory;
    ptr.* = value;
    return ptr;
}

fn report(self: *Self, err: ParserError, msg: []const u8) void {
    if (self.errors == undefined) {
        self.errors = std.ArrayList(ErrorData).init(self.allocator());
    }
    const data = ErrorData{ .err = err, .msg = msg };
    self.errors.append(data);
}

/// Parses the tokens into an AST.
pub fn parse(self: *Self) ParserError!Program {
    var program = Program.init(self.allocator());
    while (self.cursor.canRead()) {
        const statement = try self.parseStatement();
        try program.add(statement);
    }
    return program;
}

/// Parses a single statement from the tokens.
fn parseStatement(self: *Self) ParserError!Statement {
    return switch (self.currentToken()) {
        .let => try self.parseLetStatement(),
        inline else => try self.parseExpressionStatement(),
    };
}

/// Parses a let statement.
/// let x = 5;
fn parseLetStatement(self: *Self) ParserError!Statement {
    try self.expectPeekAndAdvance(.identifier);
    const identifier_token = self.currentToken();
    try self.expectPeekAndAdvance(.assignment);
    self.cursor.advance();
    const expression = try self.parseExpression(.lowest);
    try self.expectCurrentAndAdvance(.semicolon);
    return .{ .let = .{ .name = identifier_token.identifier, .expression = expression } };
}

fn parseIdentifier(self: *Self) ParserError!Expression {
    return .{ .identifier = try self.expectPeekAndRead(.identifier) };
}

/// Parses an expression as a statement.
fn parseExpressionStatement(self: *Self) ParserError!Statement {
    const expression = try self.parseExpression(.lowest);
    if (self.currentIs(.semicolon)) {
        self.cursor.advance();
    }
    return .{ .expression = expression };
}

/// Parses an expression.
fn parseExpression(self: *Self, precedence: Precedence) ParserError!Expression {
    var lhs = try self.parseExpressionAsPrefix();
    while (self.cursor.canRead() and !self.peekIs(.semicolon) and precedence.lessThan(self.currentPrecedence())) {
        const lhs_ptr = try self.moveToHeap(lhs);
        lhs = try self.parseExpressionAsInfix(lhs_ptr);
    }
    return lhs;
}

/// Parses an expression by attempting to parse it as a prefix.
fn parseExpressionAsPrefix(self: *Self) ParserError!Expression {
    const current = try self.readAndAdvance();
    return switch (current.token) {
        .plus, .minus => blk: {
            const operator = Operator.fromTokenData(current) catch return error.UnexpectedToken;
            const parsed = try self.parseExpression(.prefix);
            const rhs = try self.moveToHeap(parsed);
            break :blk .{ .prefix = .{ .operator = operator, .rhs = rhs } };
        },
        .number => |inner| .{
            .number = std.fmt.parseFloat(f64, inner) catch return error.NumberParseFailure,
        },
        .string => |inner| .{ .string = inner },
        .true => .{ .boolean = true },
        .false => .{ .boolean = false },
        .identifier => |identifier| .{ .identifier = identifier },
        .builtin => |name| try self.parseBuiltinExpression(name),
        inline else => error.NoPrefixParseRule,
    };
}

/// Parses an expression into an infix expression.
fn parseExpressionAsInfix(self: *Self, lhs: *Expression) ParserError!Expression {
    const token_data = try self.readAndAdvance();
    const operator = Operator.fromTokenData(token_data) catch return error.UnexpectedToken;
    const parsed = try self.parseExpression(Precedence.fromToken(token_data.token));
    const rhs = try self.moveToHeap(parsed);

    return .{ .binary = .{ .lhs = lhs, .operator = operator, .rhs = rhs } };
}

fn parseBuiltinExpression(self: *Self, raw_name: []const u8) ParserError!Expression {
    const parameters = try self.parseExpressionList(.left_paren, .right_paren);
    return .{ .builtin = .{ .name = raw_name[1..], .arguments = parameters } };
}

fn parseExpressionList(self: *Self, start: TokenTag, sentinel: TokenTag) ParserError![]const Expression {
    try self.expectCurrentAndAdvance(start);
    var parameters = std.ArrayList(Expression).init(self.allocator());
    if (self.currentIs(sentinel)) {
        self.cursor.advance();
        return parameters.toOwnedSlice();
    }

    parameters.append(try self.parseExpression(.lowest)) catch unreachable;
    while (self.currentIs(.comma)) {
        self.cursor.advance();
        // break out for trailing commas
        if (self.currentIs(sentinel)) {
            break;
        }
        parameters.append(try self.parseExpression(.lowest)) catch unreachable;
    }
    try self.expectCurrentAndAdvance(sentinel);
    return parameters.toOwnedSlice();
}

/// Reads the current token and advances the cursor.
fn readAndAdvance(self: *Self) ParserError!TokenData {
    return self.cursor.readAndAdvance() orelse return error.UnexpectedEOF;
}

/// Returns the current token data.
fn currentToken(self: *Self) Token {
    return self.cursor.current().token;
}

/// Returns the current token data.
fn peekToken(self: *Self) ParserError!Token {
    if (self.cursor.peek()) |data| {
        return data.token;
    }
    return error.UnexpectedEOF;
}

/// Returns true if the current token is the given tag.
inline fn currentIs(self: *Self, tag: TokenTag) bool {
    return self.cursor.canRead() and self.currentToken() == tag;
}

/// Returns true if the next token is the given tag.
inline fn peekIs(self: *Self, tag: TokenTag) bool {
    const token = self.peekToken() catch return false;
    return token == tag;
}

/// Throws an error if the current token is not the expected tag.
fn expectCurrentAndAdvance(self: *Self, tag: TokenTag) ParserError!void {
    if (!self.currentIs(tag)) {
        return error.ExpectedCurrentMismatch;
    }
    self.cursor.advance();
}

/// Throws an error if the current token is not the expected tag.
fn expectPeekAndAdvance(self: *Self, tag: TokenTag) ParserError!void {
    if (!self.peekIs(tag)) {
        return error.ExpectedPeekMismatch;
    }
    self.cursor.advance();
}

/// Returns the precedence of the current token.
fn currentPrecedence(self: *Self) Precedence {
    return Precedence.fromToken(self.currentToken());
}

/// Returns the precedence of the next token.
fn peekPrecedence(self: *Self) ParserError!Precedence {
    return Precedence.fromToken(try self.peekToken());
}

/// A helper function for testing the parser.
fn parseAndExpect(input: []const u8, test_func: *const fn (*Program) anyerror!void) anyerror!void {
    const ally = std.testing.allocator;
    const tokens = try honey.tokenize(input, ally);
    defer ally.free(tokens);
    var parser = init(tokens, ally);
    defer parser.deinit();
    var program = try parser.parse();
    defer program.deinit();
    try test_func(&program);
}

test "test simple parsing (1 + 2)" {
    try parseAndExpect("1 + 2", struct {
        pub fn func(program: *Program) anyerror!void {
            var lhs = Expression{ .number = 1.0 };
            var rhs = Expression{ .number = 2.0 };
            const expected = Statement{ .expression = .{
                .binary = .{
                    .lhs = &lhs,
                    .operator = .plus,
                    .rhs = &rhs,
                },
            } };
            const statements = try program.statements.toOwnedSlice();
            try std.testing.expectEqualDeep(expected, statements[0]);
        }
    }.func);
}

test "test parsing with precedence ((1 + 2) - 3)" {
    try parseAndExpect("1 + 2 - 3", struct {
        pub fn func(program: *Program) anyerror!void {
            // 1 + 2
            var lhs_1 = Expression{ .number = 1.0 };
            var rhs = Expression{ .number = 2.0 };
            var lhs_expr = Expression{ .binary = .{ .lhs = &lhs_1, .operator = .plus, .rhs = &rhs } };

            // 3
            var rhs_2 = Expression{ .number = 3.0 };

            // ((1 + 2) - 3)
            const expected = Statement{ .expression = .{
                .binary = .{
                    .lhs = &lhs_expr,
                    .operator = .minus,
                    .rhs = &rhs_2,
                },
            } };
            const statements = try program.statements.toOwnedSlice();
            try std.testing.expectEqualDeep(expected, statements[0]);
        }
    }.func);
}

test "test parsing let statement" {
    try parseAndExpect("let variable = 1;", struct {
        pub fn func(program: *Program) anyerror!void {
            const expected = Statement{ .let = .{
                .name = "variable",
                .expression = Expression{ .number = 1.0 },
            } };
            const statements = try program.statements.toOwnedSlice();
            try std.testing.expectEqualDeep(expected, statements[0]);
        }
    }.func);
}

test "test parsing builtin expression" {
    try parseAndExpect("@print(1, 2, 3)", struct {
        pub fn func(program: *Program) anyerror!void {
            const expected = Statement{ .expression = .{
                .builtin = .{
                    .name = "print",
                    .arguments = &.{
                        Expression{ .number = 1.0 },
                        Expression{ .number = 2.0 },
                        Expression{ .number = 3.0 },
                    },
                },
            } };
            const statements = try program.statements.toOwnedSlice();
            try std.testing.expectEqualDeep(expected, statements[0]);
        }
    }.func);
}

test "test trailing comma" {
    try parseAndExpect("@print(1, 2, 3, )", struct {
        pub fn func(program: *Program) anyerror!void {
            const expected = Statement{ .expression = .{
                .builtin = .{
                    .name = "print",
                    .arguments = &.{
                        Expression{ .number = 1.0 },
                        Expression{ .number = 2.0 },
                        Expression{ .number = 3.0 },
                    },
                },
            } };
            const statements = try program.statements.toOwnedSlice();
            try std.testing.expectEqualDeep(expected, statements[0]);
        }
    }.func);
}
