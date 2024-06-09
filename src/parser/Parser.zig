const std = @import("std");
const Lexer = @import("../lexer/Lexer.zig");
const Token = @import("../lexer/token.zig").Token;
const TokenTag = @import("../lexer/token.zig").TokenTag;
const TokenData = @import("../lexer/token.zig").TokenData;

const ast = @import("ast.zig");
const Program = ast.Program;
const Statement = ast.Statement;
const Expression = ast.Expression;
const Operator = ast.Operator;

const utils = @import("../utils/utils.zig");

pub const Precedence = enum {
    lowest,
    logical,
    equals,
    less_and_greater,
    sum,
    product,
    exponent,
    modulo,
    prefix,
    call,

    pub fn fromToken(token: Token) Precedence {
        return switch (token) {
            .@"or", .@"and" => .logical,
            .equal, .not_equal => .equals,
            .less_than, .greater_than, .less_than_equal, .greater_than_equal => .less_and_greater,
            .plus, .minus => .sum,
            .star, .slash => .product,
            .doublestar => .exponent,
            .modulo => .modulo,
            .bang => .prefix,
            .left_paren => .call,
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

pub const ParserError = error{
    UnexpectedToken,
    UnexpectedEOF,
    NoPrefixParseRule,
    NoInfixParseRule,
    OutOfMemory,
    NumberParseFailure,
    ExpectedCurrentMismatch,
    ExpectedPeekMismatch,
    ExpectedElseCondition,
    EncounteredErrors,
};

const ErrorData = struct {
    err: ParserError,
    msg: []const u8,
};

const Self = @This();

arena: std.heap.ArenaAllocator,
cursor: utils.Cursor(TokenData),
stderr: std.fs.File.Writer,
diagnostics: utils.Diagnostics,

const ParserOptions = struct {
    ally: std.mem.Allocator,
};

/// Initializes the parser.
pub fn init(tokens: []const TokenData, options: ParserOptions) Self {
    return .{
        .arena = std.heap.ArenaAllocator.init(options.ally),
        .cursor = utils.Cursor(TokenData).init(tokens),
        .stderr = std.io.getStdErr().writer(),
        .diagnostics = utils.Diagnostics.init(options.ally),
    };
}

/// Deinitializes the parser.
pub fn deinit(self: *Self) void {
    self.arena.deinit();
    self.diagnostics.deinit();
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

/// Reports any errors that have occurred during execution to stderr
pub fn report(self: *Self, error_writer: std.fs.File.Writer) void {
    if (!self.diagnostics.hasErrors()) {
        return;
    }
    error_writer.print("Encountered the following errors during execution:\n", .{}) catch unreachable;
    error_writer.print("--------------------------------------------------\n", .{}) catch unreachable;
    for (self.diagnostics.errors.items, 0..) |msg, index| {
        error_writer.print(" - {s}", .{msg}) catch unreachable;
        if (index < self.diagnostics.errors.items.len) {
            error_writer.print("\n", .{}) catch unreachable;
        }
    }
    error_writer.print("--------------------------------------------------\n", .{}) catch unreachable;
}

/// Parses the tokens into an AST.
pub fn parse(self: *Self) ParserError!Program {
    var program = Program.init(self.allocator());
    while (self.cursor.canRead()) {
        const statement = self.parseStatement() catch continue;
        if (statement) |stmt| {
            try program.add(stmt);
        }
    }

    if (self.diagnostics.hasErrors()) {
        return error.EncounteredErrors;
    }
    return program;
}

/// Parses a single statement from the tokens.
fn parseStatement(self: *Self) ParserError!?Statement {
    return switch (self.currentToken()) {
        .let, .@"const" => try self.parseVarDeclaration(),
        .@"fn" => try self.parseFunctionDeclaration(),
        .@"return" => blk: {
            self.cursor.advance();
            if (self.currentIs(.semicolon)) {
                try self.expectCurrentAndAdvance(.semicolon);
                break :blk .{ .@"return" = .{ .expression = null } };
            }
            const expression = try self.parseExpression(.lowest);
            try self.expectCurrentAndAdvance(.semicolon);
            break :blk .{ .@"return" = .{ .expression = expression } };
        },
        .left_brace => .{ .block = try self.parseBlockStatement() },
        // skip comments
        .comment => blk: {
            self.cursor.advance();
            break :blk null;
        },
        inline else => blk: {
            // try to parse an assignment statement
            if (self.cursor.hasNext() and self.currentIs(.identifier)) {
                const next = self.peekToken() catch unreachable;
                if (next.isAssignment()) {
                    break :blk try self.parseAssignmentStatement();
                }
            }
            break :blk try self.parseExpressionStatement();
        },
    };
}

/// Parses a let/const statement.
/// let x = 5; OR const x = 5;
fn parseVarDeclaration(self: *Self) ParserError!Statement {
    const type_token = self.currentToken();
    if (type_token != .let and type_token != .@"const") {
        return error.UnexpectedToken;
    }
    try self.expectPeekAndAdvance(.identifier);
    const identifier_token = self.currentToken();
    try self.expectPeekAndAdvance(.assignment);
    self.cursor.advance();
    const expression = try self.parseExpression(.lowest);
    try self.expectCurrentAndAdvance(.semicolon);
    return .{ .variable = .{
        .type = type_token,
        .name = identifier_token.identifier,
        .expression = expression,
    } };
}

fn parseAssignmentStatement(self: *Self) ParserError!Statement {
    const identifier_token = self.currentToken();
    try self.expectCurrentAndAdvance(.identifier);
    const assignment_token_data = try self.readAndAdvance();
    if (!assignment_token_data.token.isAssignment()) {
        return error.UnexpectedToken;
    }
    const expression = try self.parseExpression(.lowest);
    try self.expectCurrentAndAdvance(.semicolon);
    return .{ .assignment = .{ .name = identifier_token.identifier, .type = assignment_token_data.token, .expression = expression } };
}

fn parseFunctionDeclaration(self: *Self) ParserError!Statement {
    try self.expectPeekAndAdvance(.identifier);
    const identifier_data = try self.readAndAdvance();
    const parameters = try self.parseFunctionParameters();
    const body = try self.parseBlockStatement();
    return .{ .@"fn" = .{
        .name = identifier_data.token.identifier,
        .parameters = parameters,
        .body = body,
    } };
}

fn parseFunctionParameters(self: *Self) ParserError![]const Expression {
    const expressions = try self.parseExpressionList(.left_paren, .right_paren);
    for (expressions) |expr| {
        if (expr != .identifier) {
            self.diagnostics.report("expected identifier but got: {}", .{expr});
            return error.UnexpectedToken;
        }
    }
    return expressions;
}

fn parseBlockStatement(self: *Self) ParserError!ast.BlockStatement {
    try self.expectCurrentAndAdvance(.left_brace);
    var statements = std.ArrayList(Statement).init(self.allocator());
    while (!self.currentIs(.right_brace) and self.cursor.canRead()) {
        const statement = try self.parseStatement() orelse continue;
        statements.append(statement) catch return error.OutOfMemory;
    }
    try self.expectCurrentAndAdvance(.right_brace);
    return .{ .statements = statements.toOwnedSlice() catch return error.OutOfMemory };
}

fn parseIdentifier(self: *Self) ParserError!Expression {
    return .{ .identifier = try self.expectPeekAndRead(.identifier) };
}

/// Parses an expression as a statement.
fn parseExpressionStatement(self: *Self) ParserError!Statement {
    const expression = try self.parseExpression(.lowest);

    var terminated: bool = false;
    if (self.currentIs(.semicolon)) {
        self.cursor.advance();
        terminated = true;
    }
    return .{ .expression = .{ .expression = expression, .terminated = terminated } };
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

fn parseIfBody(self: *Self) ParserError!ast.IfExpression.Body {
    if (self.currentIs(.left_brace)) {
        const block = try self.parseBlockStatement();
        return .{ .block = block };
    }
    const parsed = try self.parseExpression(.lowest);
    const expression = try self.moveToHeap(parsed);
    return .{ .expression = expression };
}

fn parseIfExpression(self: *Self) ParserError!Expression {
    if (!self.currentIs(.@"if")) {
        self.diagnostics.report("expected if but got: {}", .{self.currentToken()});
        return error.UnexpectedToken;
    }

    var condition_list = std.ArrayList(ast.IfExpression.ConditionData).init(self.allocator());
    while (self.currentIs(.@"if")) {
        self.cursor.advance();
        try self.expectCurrentAndAdvance(.left_paren);
        const parsed = try self.parseExpression(.lowest);
        const condition_ptr = try self.moveToHeap(parsed);
        try self.expectCurrentAndAdvance(.right_paren);
        const body = try self.parseIfBody();
        condition_list.append(.{ .condition = condition_ptr, .body = body }) catch return error.OutOfMemory;
        // if we don't have an else if, break out of the loop
        if (!(self.currentIs(.@"else") and self.cursor.hasNext() and self.peekIs(.@"if"))) {
            break;
        }
    }
    const alternative = blk: {
        if (!self.currentIs(.@"else")) {
            break :blk null;
        }
        self.cursor.advance();
        break :blk try self.parseIfBody();
    };

    return .{ .if_expr = .{
        .condition_list = condition_list.toOwnedSlice() catch return error.OutOfMemory,
        .alternative = alternative,
    } };
}

fn parseWhileExpression(self: *Self) ParserError!Expression {
    try self.expectCurrentAndAdvance(.left_paren);
    const parsed = try self.parseExpression(.lowest);
    const condition_ptr = try self.moveToHeap(parsed);
    try self.expectCurrentAndAdvance(.right_paren);
    const body = try self.parseBlockStatement();
    return .{ .@"while" = .{ .condition = condition_ptr, .body = body } };
}

/// Parses an expression by attempting to parse it as a prefix.
fn parseExpressionAsPrefix(self: *Self) ParserError!Expression {
    const current = try self.readAndAdvance();
    return switch (current.token) {
        .plus, .minus, .bang => blk: {
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
        .null => .null,
        .identifier => |identifier| .{ .identifier = identifier },
        .@"if" => blk: {
            self.cursor.rewind() catch unreachable;
            break :blk try self.parseIfExpression();
        },
        .@"while" => try self.parseWhileExpression(),
        .builtin => |name| try self.parseBuiltinExpression(name),
        .left_paren => blk: {
            const parsed = try self.parseExpression(.lowest);
            try self.expectCurrentAndAdvance(.right_paren);
            break :blk parsed;
        },
        inline else => {
            self.diagnostics.report("no prefix parse rule for token: {}", .{current.token});
            return error.NoPrefixParseRule;
        },
    };
}

/// Parses an expression into an infix expression.
fn parseExpressionAsInfix(self: *Self, lhs: *Expression) ParserError!Expression {
    const token_data = try self.readAndAdvance();
    if (token_data.token == .left_paren) {
        return try self.parseCallExpression(lhs);
    }
    const operator = Operator.fromTokenData(token_data) catch return error.UnexpectedToken;
    const parsed = try self.parseExpression(Precedence.fromToken(token_data.token));
    const rhs = try self.moveToHeap(parsed);

    return .{ .binary = .{ .lhs = lhs, .operator = operator, .rhs = rhs } };
}

/// Parses a call expression using the given identifier as the name.
fn parseCallExpression(self: *Self, expr: *Expression) ParserError!Expression {
    if (expr.* != .identifier) {
        self.diagnostics.report("expected identifier but got: {}", .{expr});
        return error.UnexpectedToken;
    }
    // rewind the cursor to make sure we can use `parseExpressionList`
    self.cursor.rewind() catch unreachable;
    const arguments = try self.parseExpressionList(.left_paren, .right_paren);
    return .{ .call = .{ .name = expr.identifier, .arguments = arguments } };
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
    if (!self.cursor.canRead()) {
        self.diagnostics.report("expected current token: {} but got EOF", .{tag});
        return error.UnexpectedEOF;
    }
    if (!self.currentIs(tag)) {
        self.diagnostics.report("expected current token: {} but got: {}", .{ tag, self.currentToken() });
        return error.ExpectedCurrentMismatch;
    }
    self.cursor.advance();
}

/// Throws an error if the current token is not the expected tag.
fn expectPeekAndAdvance(self: *Self, tag: TokenTag) ParserError!void {
    if (!self.cursor.canRead()) {
        self.diagnostics.report("expected peek token: {} but got EOF", .{tag});
        return error.UnexpectedEOF;
    }
    if (!self.peekIs(tag)) {
        self.diagnostics.report("expected peek token: {} but got: {?}", .{ tag, self.peekToken() catch null });
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
    const honey = @import("../honey.zig");
    const ally = std.testing.allocator;
    const tokens = try honey.tokenize(input, ally);
    defer ally.free(tokens);
    var parser = init(tokens, .{ .ally = ally });
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
            const expected = ast.createBinaryStatement(&lhs, .plus, &rhs, false);
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
            const expected = ast.createBinaryStatement(&lhs_expr, .minus, &rhs_2, false);
            const statements = try program.statements.toOwnedSlice();
            try std.testing.expectEqualDeep(expected, statements[0]);
        }
    }.func);
}

test "test parsing let statement" {
    try parseAndExpect("let variable = 1;", struct {
        pub fn func(program: *Program) anyerror!void {
            const expected = Statement{ .variable = .{
                .name = "variable",
                .type = .let,
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
            const expected = ast.createBuiltinStatement("print", &.{
                Expression{ .number = 1.0 },
                Expression{ .number = 2.0 },
                Expression{ .number = 3.0 },
            }, false);
            const statements = try program.statements.toOwnedSlice();
            try std.testing.expectEqualDeep(expected, statements[0]);
        }
    }.func);
}

test "test trailing comma" {
    try parseAndExpect("@print(1, 2, 3, )", struct {
        pub fn func(program: *Program) anyerror!void {
            const expected = ast.createBuiltinStatement("print", &.{
                Expression{ .number = 1.0 },
                Expression{ .number = 2.0 },
                Expression{ .number = 3.0 },
            }, false);
            const statements = try program.statements.toOwnedSlice();
            try std.testing.expectEqualDeep(expected, statements[0]);
        }
    }.func);
}

test "test assignment types" {
    try parseAndExpect("value = 1;", struct {
        pub fn func(program: *Program) anyerror!void {
            const expected = ast.createAssignStatement(
                "value",
                .assignment,
                Expression{ .number = 1.0 },
            );
            const statements = try program.statements.toOwnedSlice();
            try std.testing.expectEqualDeep(expected, statements[0]);
        }
    }.func);
    try parseAndExpect("value += 1;", struct {
        pub fn func(program: *Program) anyerror!void {
            const expected = ast.createAssignStatement(
                "value",
                .plus_assignment,
                Expression{ .number = 1.0 },
            );
            const statements = try program.statements.toOwnedSlice();
            try std.testing.expectEqualDeep(expected, statements[0]);
        }
    }.func);
    try parseAndExpect("value -= 1;", struct {
        pub fn func(program: *Program) anyerror!void {
            const expected = ast.createAssignStatement(
                "value",
                .minus_assignment,
                Expression{ .number = 1.0 },
            );
            const statements = try program.statements.toOwnedSlice();
            try std.testing.expectEqualDeep(expected, statements[0]);
        }
    }.func);
    try parseAndExpect("value *= 1;", struct {
        pub fn func(program: *Program) anyerror!void {
            const expected = ast.createAssignStatement(
                "value",
                .star_assignment,
                Expression{ .number = 1.0 },
            );
            const statements = try program.statements.toOwnedSlice();
            try std.testing.expectEqualDeep(expected, statements[0]);
        }
    }.func);
    try parseAndExpect("value /= 1;", struct {
        pub fn func(program: *Program) anyerror!void {
            const expected = ast.createAssignStatement(
                "value",
                .slash_assignment,
                Expression{ .number = 1.0 },
            );
            const statements = try program.statements.toOwnedSlice();
            try std.testing.expectEqualDeep(expected, statements[0]);
        }
    }.func);
    try parseAndExpect("value %= 1;", struct {
        pub fn func(program: *Program) anyerror!void {
            const expected = ast.createAssignStatement(
                "value",
                .modulo_assignment,
                Expression{ .number = 1.0 },
            );
            const statements = try program.statements.toOwnedSlice();
            try std.testing.expectEqualDeep(expected, statements[0]);
        }
    }.func);
}

test "test parsing simple if expression" {
    try parseAndExpect("if (x < y) x else y", struct {
        pub fn func(program: *Program) anyerror!void {
            var if_condition_lhs = Expression{ .identifier = "x" };
            var if_condition_rhs = Expression{ .identifier = "y" };
            var if_condition = Expression{ .binary = .{
                .lhs = &if_condition_lhs,
                .operator = .less_than,
                .rhs = &if_condition_rhs,
            } };

            var x_identifier = Expression{ .identifier = "x" };
            var y_identifier = Expression{ .identifier = "y" };

            const expected = ast.createIfStatement(
                &[_]ast.IfExpression.ConditionData{.{ .condition = &if_condition, .body = .{ .expression = &x_identifier } }},
                .{ .expression = &y_identifier },
                false,
            );
            const statements = try program.statements.toOwnedSlice();
            try std.testing.expectEqualDeep(expected, statements[0]);
        }
    }.func);
}

test "test parsing simple while expression" {
    try parseAndExpect("while (x < y) { doSomething(); }", struct {
        pub fn func(program: *Program) anyerror!void {
            var while_condition_lhs = Expression{ .identifier = "x" };
            var while_condition_rhs = Expression{ .identifier = "y" };
            var while_condition = Expression{ .binary = .{
                .lhs = &while_condition_lhs,
                .operator = .less_than,
                .rhs = &while_condition_rhs,
            } };
            const expected = ast.createWhileStatement(
                &while_condition,
                ast.BlockStatement{ .statements = &[_]ast.Statement{
                    ast.createCallStatement("doSomething", &.{}, true),
                } },
                false,
            );
            const statements = try program.statements.toOwnedSlice();
            try std.testing.expectEqualDeep(expected, statements[0]);
        }
    }.func);
}
