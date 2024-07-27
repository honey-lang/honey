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
    index,

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
            .left_bracket => .index,
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
    const ptr = self.allocator().create(@TypeOf(value)) catch return ParserError.OutOfMemory;
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
        const statement = self.parseStatement(true) catch continue;
        if (statement) |stmt| {
            try program.add(stmt);
        }
    }

    if (self.diagnostics.hasErrors()) {
        return ParserError.EncounteredErrors;
    }
    return program;
}

/// Parses a single statement from the tokens.
fn parseStatement(self: *Self, needs_terminated: bool) ParserError!?Statement {
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
        .@"break" => blk: {
            self.cursor.advance();
            if (self.currentIs(.semicolon)) {
                try self.expectCurrentAndAdvance(.semicolon);
            }
            // todo: break values
            break :blk .@"break";
        },
        .@"continue" => blk: {
            self.cursor.advance();
            if (self.currentIs(.semicolon)) {
                try self.expectCurrentAndAdvance(.semicolon);
            }
            break :blk .@"continue";
        },
        .left_brace => .{ .block = try self.parseBlockStatement() },
        // skip comments
        .comment => blk: {
            self.cursor.advance();
            break :blk null;
        },
        inline else => blk: {
            const expression = try self.parseExpression(.lowest);
            // try to parse an assignment statement
            // todo: expand expressions to include dot operator (e.g., this.is.a.nested.identifier)
            if (self.cursor.hasNext() and self.currentToken().isAssignment() and (expression == .index or expression == .identifier)) {
                break :blk try self.parseAssignmentStatement(expression, needs_terminated);
            }

            var terminated: bool = false;
            if (self.currentIs(.semicolon)) {
                self.cursor.advance();
                terminated = true;
            }
            break :blk ast.createExpressionStatement(expression, terminated);
        },
    };
}

/// Parses a let/const statement.
/// let x: int = 5; OR const x = 5;
fn parseVarDeclaration(self: *Self) ParserError!Statement {
    const kind_token = self.currentToken();
    if (kind_token != .let and kind_token != .@"const") {
        return ParserError.UnexpectedToken;
    }
    try self.expectPeekAndAdvance(.identifier);
    const identifier_token = self.currentToken();
    const type_token = if (self.peekIs(.colon)) type_name: {
        self.cursor.advance();
        try self.expectPeekAndAdvance(.identifier);
        break :type_name self.currentToken();
    } else null;
    try self.expectPeekAndAdvance(.assignment);
    self.cursor.advance();
    const expression = try self.parseExpression(.lowest);
    try self.expectCurrentAndAdvance(.semicolon);
    return .{ .variable = .{
        .kind = kind_token,
        .name = identifier_token.identifier,
        .type = if (type_token) |token| token.identifier else null,
        .expression = expression,
    } };
}

fn parseAssignmentStatement(self: *Self, lhs: Expression, needs_terminated: bool) ParserError!Statement {
    if (lhs != .identifier and lhs != .index) {
        return ParserError.UnexpectedToken;
    }
    const assignment_token_data = try self.readAndAdvance();
    if (!assignment_token_data.token.isAssignment()) {
        return ParserError.UnexpectedToken;
    }
    const rhs = try self.parseExpression(.lowest);
    if (needs_terminated) {
        try self.expectCurrentAndAdvance(.semicolon);
    }
    return ast.createAssignStatement(lhs, assignment_token_data.token, rhs);
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
            return ParserError.UnexpectedToken;
        }
    }
    return expressions;
}

fn parseBlockStatement(self: *Self) ParserError!ast.BlockStatement {
    try self.expectCurrentAndAdvance(.left_brace);
    var statements = std.ArrayList(Statement).init(self.allocator());
    while (!self.currentIs(.right_brace) and self.cursor.canRead()) {
        const statement = try self.parseStatement(true) orelse continue;
        statements.append(statement) catch return ParserError.OutOfMemory;
    }
    try self.expectCurrentAndAdvance(.right_brace);
    return .{ .statements = statements.toOwnedSlice() catch return ParserError.OutOfMemory };
}

fn parseListExpression(self: *Self) ParserError!Expression {
    // rewind back to the bracket before parsing as an expression list
    self.cursor.rewind() catch return ParserError.UnexpectedEOF;
    const list = try self.parseExpressionList(.left_bracket, .right_bracket);
    return .{ .list = .{ .expressions = list } };
}

fn parseIdentifier(self: *Self) ParserError!Expression {
    const current = try self.readAndAdvance();
    if (current.token != .identifier) {
        self.diagnostics.report("expected identifier but got: {}", .{current.token});
        return ParserError.UnexpectedToken;
    }
    return .{ .identifier = current.token.identifier };
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
        return ParserError.UnexpectedToken;
    }

    var condition_list = std.ArrayList(ast.IfExpression.ConditionData).init(self.allocator());
    while (self.currentIs(.@"if")) {
        self.cursor.advance();
        try self.expectCurrentAndAdvance(.left_paren);
        const parsed = try self.parseExpression(.lowest);
        const condition_ptr = try self.moveToHeap(parsed);
        try self.expectCurrentAndAdvance(.right_paren);
        const body = try self.parseIfBody();
        condition_list.append(.{ .condition = condition_ptr, .body = body }) catch return ParserError.OutOfMemory;
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
        .condition_list = condition_list.toOwnedSlice() catch return ParserError.OutOfMemory,
        .alternative = alternative,
    } };
}

fn parseWhileExpression(self: *Self) ParserError!Expression {
    try self.expectCurrentAndAdvance(.left_paren);
    const parsed = try self.parseExpression(.lowest);
    const condition_ptr = try self.moveToHeap(parsed);
    try self.expectCurrentAndAdvance(.right_paren);
    // encountered a post statement
    // while (true) : (i += 1) {}
    var post_stmt: ?Statement = null;
    if (self.currentIs(.colon)) {
        self.cursor.advance();
        try self.expectCurrentAndAdvance(.left_paren);
        post_stmt = try self.parseStatement(false);
        try self.expectCurrentAndAdvance(.right_paren);
    }
    const body = try self.parseStatement(false) orelse {
        self.diagnostics.report("expected statement but got: {}", .{self.currentToken()});
        return ParserError.UnexpectedToken;
    };

    return .{ .while_expr = .{
        .condition = condition_ptr,
        .body = try self.moveToHeap(body),
        .post_stmt = if (post_stmt) |stmt| try self.moveToHeap(stmt) else null,
    } };
}

fn parseForExpression(self: *Self) ParserError!Expression {
    // for (0..10)
    try self.expectCurrentAndAdvance(.left_paren);
    const expr = switch (self.currentToken()) {
        .number => try self.parseRange(),
        inline else => try self.parseExpression(.lowest),
    };
    switch (expr) {
        .identifier, .list, .range => {},
        inline else => {
            self.diagnostics.report("expected identifier, list, or range but got {s}", .{expr});
            return ParserError.UnexpectedToken;
        },
    }

    try self.expectCurrentAndAdvance(.right_paren);

    // |i|
    try self.expectCurrentAndAdvance(.pipe);
    const capture = try self.parseExpression(.lowest);
    if (capture != .identifier) {
        self.diagnostics.report("expected identifier but got: {s}", .{capture});
        return ParserError.UnexpectedToken;
    }
    const capture_ptr = try self.moveToHeap(capture);
    try self.expectCurrentAndAdvance(.pipe);
    // { ... }
    const body = try self.parseStatement(false) orelse {
        self.diagnostics.report("expected statement but got: {}", .{self.currentToken()});
        return ParserError.UnexpectedToken;
    };

    return .{ .for_expr = .{
        .expr = try self.moveToHeap(expr),
        .capture = capture_ptr.identifier,
        .body = try self.moveToHeap(body),
    } };
}

fn parseRange(self: *Self) ParserError!Expression {
    const start = try self.parseExpression(.lowest);
    const start_ptr = try self.moveToHeap(start);

    const inclusive = switch (self.currentToken()) {
        .inclusive_range => true,
        .exclusive_range => false,
        inline else => {
            self.diagnostics.report("expected range operator but got: {}", .{self.currentToken()});
            return ParserError.UnexpectedToken;
        },
    };
    self.cursor.advance();

    const end = try self.parseExpression(.lowest);
    const end_ptr = try self.moveToHeap(end);

    return .{ .range = .{ .start = start_ptr, .end = end_ptr, .inclusive = inclusive } };
}

/// Parses an expression by attempting to parse it as a prefix.
fn parseExpressionAsPrefix(self: *Self) ParserError!Expression {
    const current = try self.readAndAdvance();
    return switch (current.token) {
        .plus, .minus, .bang => blk: {
            const operator = Operator.fromTokenData(current) catch return ParserError.UnexpectedToken;
            const parsed = try self.parseExpression(.prefix);
            const rhs = try self.moveToHeap(parsed);
            break :blk .{ .prefix = .{ .operator = operator, .rhs = rhs } };
        },
        .number => |inner| .{
            .number = std.fmt.parseFloat(f64, inner) catch return ParserError.NumberParseFailure,
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
        .@"for" => try self.parseForExpression(),
        .left_bracket => try self.parseListExpression(),
        .builtin => |name| try self.parseBuiltinExpression(name),
        .left_paren => blk: {
            const parsed = try self.parseExpression(.lowest);
            try self.expectCurrentAndAdvance(.right_paren);
            break :blk parsed;
        },
        inline else => {
            self.diagnostics.report("no prefix parse rule for token: {}", .{current.token});
            return ParserError.NoPrefixParseRule;
        },
    };
}

/// Parses an expression into an infix expression.
fn parseExpressionAsInfix(self: *Self, lhs: *Expression) ParserError!Expression {
    const token_data = try self.readAndAdvance();
    switch (token_data.token) {
        .left_paren => return try self.parseCallExpression(lhs),
        .left_bracket => return try self.parseIndexExpression(lhs),
        else => {},
    }
    const operator = Operator.fromTokenData(token_data) catch return ParserError.UnexpectedToken;
    const parsed = try self.parseExpression(Precedence.fromToken(token_data.token));
    const rhs = try self.moveToHeap(parsed);

    return .{ .binary = .{ .lhs = lhs, .operator = operator, .rhs = rhs } };
}

/// Parses a call expression using the given identifier as the name.
fn parseCallExpression(self: *Self, expr: *Expression) ParserError!Expression {
    if (expr.* != .identifier) {
        self.diagnostics.report("expected identifier but got: {}", .{expr});
        return ParserError.UnexpectedToken;
    }
    // rewind the cursor to make sure we can use `parseExpressionList`
    self.cursor.rewind() catch unreachable;
    const arguments = try self.parseExpressionList(.left_paren, .right_paren);
    return .{ .call = .{ .name = expr.identifier, .arguments = arguments } };
}

/// Parses an index expression given the LHS
fn parseIndexExpression(self: *Self, lhs: *Expression) ParserError!Expression {
    const index = try self.parseExpression(.lowest);
    const index_ptr = try self.moveToHeap(index);
    try self.expectCurrentAndAdvance(.right_bracket);
    return .{ .index = .{ .lhs = lhs, .index = index_ptr } };
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
    return self.cursor.readAndAdvance() orelse return ParserError.UnexpectedEOF;
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
    return ParserError.UnexpectedEOF;
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
        return ParserError.UnexpectedEOF;
    }
    if (!self.currentIs(tag)) {
        self.diagnostics.report("expected current token: {} but got: {}", .{ tag, self.currentToken() });
        return ParserError.ExpectedCurrentMismatch;
    }
    self.cursor.advance();
}

/// Throws an error if the current token is not the expected tag.
fn expectPeekAndAdvance(self: *Self, tag: TokenTag) ParserError!void {
    if (!self.cursor.canRead()) {
        self.diagnostics.report("expected peek token: {} but got EOF", .{tag});
        return ParserError.UnexpectedEOF;
    }
    if (!self.peekIs(tag)) {
        self.diagnostics.report("expected peek token: {} but got: {?}", .{ tag, self.peekToken() catch null });
        return ParserError.ExpectedPeekMismatch;
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
    var program = parser.parse() catch |err| {
        parser.diagnostics.dump(std.io.getStdErr().writer());
        return err;
    };
    defer program.deinit();

    // std.debug.print("Statement: {s}\n", .{program.statements.items[0]});

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
                .{ .identifier = "value" },
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
                .{ .identifier = "value" },
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
                .{ .identifier = "value" },
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
                .{ .identifier = "value" },
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
                .{ .identifier = "value" },
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
                .{ .identifier = "value" },
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
            var block = ast.createBlockStatement(&[_]ast.Statement{
                ast.createCallStatement("doSomething", &.{}, true),
            });
            const expected = ast.createWhileStatement(
                &while_condition,
                &block,
                null,
                false,
            );
            const statements = try program.statements.toOwnedSlice();
            try std.testing.expectEqualDeep(expected, statements[0]);
        }
    }.func);
}

test "test parsing simple for expression" {
    try parseAndExpect("for (0..10) |i| { doSomething(); }", struct {
        pub fn func(program: *Program) anyerror!void {
            var for_start = Expression{ .number = 0 };
            var for_end = Expression{ .number = 10 };
            var for_range = Expression{ .range = .{
                .start = &for_start,
                .end = &for_end,
                .inclusive = false,
            } };

            var block = ast.createBlockStatement(&[_]ast.Statement{
                ast.createCallStatement("doSomething", &.{}, true),
            });
            const expected = ast.createForStatement(
                &for_range,
                "i",
                &block,
                false,
            );
            const statements = try program.statements.toOwnedSlice();
            try std.testing.expectEqualDeep(expected, statements[0]);
        }
    }.func);
}

test "test parsing simple list expression" {
    try parseAndExpect("[\"a\", 2, 3.5, true]", struct {
        pub fn func(program: *Program) anyerror!void {
            const a_expr = ast.Expression{ .string = "a" };
            const two_expr = ast.Expression{ .number = 2 };
            const three_float_expr = ast.Expression{ .number = 3.5 };
            const true_expr = ast.Expression{ .boolean = true };

            const list = ast.Expression{ .list = .{ .expressions = &.{
                a_expr,
                two_expr,
                three_float_expr,
                true_expr,
            } } };
            const expected = ast.createExpressionStatement(list, false);
            const statements = try program.statements.toOwnedSlice();
            try std.testing.expectEqualDeep(expected, statements[0]);
        }
    }.func);
}

test "test parsing simple index expression" {
    try parseAndExpect("list[1]", struct {
        pub fn func(program: *Program) anyerror!void {
            var list_expr = ast.Expression{ .identifier = "list" };
            var index_expr = ast.Expression{ .number = 1 };

            const expected = ast.createIndexStatement(&list_expr, &index_expr, false);
            const statements = try program.statements.toOwnedSlice();
            try std.testing.expectEqualDeep(expected, statements[0]);
        }
    }.func);
}
