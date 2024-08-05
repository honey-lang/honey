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
            .left_bracket, .dot => .index,
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
/// The main data compiled from the lexer
lex_data: Lexer.Data,
/// A cursor that iterates through the tokens
cursor: utils.Cursor(TokenData),
/// Spans that consist of the lines
line_data: []const utils.Span,
error_writer: std.io.AnyWriter,
diagnostics: utils.Diagnostics,

const ParserOptions = struct {
    ally: std.mem.Allocator,
    error_writer: std.io.AnyWriter,
};

/// Initializes the parser.
pub fn init(data: Lexer.Data, options: ParserOptions) Self {
    return .{
        .arena = std.heap.ArenaAllocator.init(options.ally),
        .lex_data = data,
        .cursor = utils.Cursor(TokenData).init(data.tokens.items),
        .line_data = data.line_data.items,
        .error_writer = options.error_writer,
        .diagnostics = utils.Diagnostics.init(options.ally),
    };
}

/// Deinitializes the parser.
pub fn deinit(self: *Self) void {
    self.arena.deinit();
    self.diagnostics.deinit();
    self.lex_data.deinit();
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
pub fn report(self: *Self) void {
    if (!self.diagnostics.hasErrors()) {
        return;
    }

    const msg_data = self.diagnostics.errors.items(.msg);
    const token_data = self.diagnostics.errors.items(.token_data);
    for (msg_data, token_data, 0..) |msg, token_datum, index| {
        self.printErrorAtToken(token_datum, msg) catch unreachable;
        if (index < self.diagnostics.errors.len) {
            self.error_writer.writeByte('\n') catch unreachable;
        }
    }
}

/// Attempts to match a token to the line that it exists on
pub fn findLineIndex(self: *Self, token_data: TokenData) ?usize {
    return std.sort.binarySearch(utils.Span, token_data.position, self.line_data, {}, struct {
        pub fn find(_: void, key: utils.Span, mid_item: utils.Span) std.math.Order {
            if (key.start >= mid_item.start and key.end <= mid_item.end) {
                return .eq;
            } else if (key.end < mid_item.start) {
                return .lt;
            }
            return .gt;
        }
    }.find);
}

/// Attempts to match a token to the line that it exists on
pub fn findLine(self: *Self, token_data: TokenData) ?utils.Span {
    const result = self.findLineIndex(token_data);
    return if (result) |found| self.line_data[found] else null;
}

/// Prints
pub fn printErrorAtToken(self: *Self, token_data: TokenData, msg: []const u8) !void {
    const line_index = self.findLineIndex(token_data) orelse return ParserError.UnexpectedToken;
    const line = self.line_data[line_index];

    const column_index = token_data.position.start - line.start;

    // todo: move to terminal
    // we offset the line & column by one for one-indexing
    try self.error_writer.print("[{s}:{d}:{d}] error: {s}\n", .{ self.lex_data.source_name, line_index + 1, column_index + 1, msg });

    try self.error_writer.writeByte('\t');
    _ = try self.error_writer.write(self.lex_data.getLineBySpan(line));
    try self.error_writer.writeByte('\n');

    try self.error_writer.writeByte('\t');
    // pad up to the token
    try self.error_writer.writeByteNTimes(' ', column_index);
    // arrows to indicate token highlighted

    const token_len = token_data.position.end - token_data.position.start;
    try self.error_writer.writeByteNTimes('~', token_len + 1);
    try self.error_writer.writeByte('\n');
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
                self.cursor.advance();
                break :blk .{ .@"return" = .{ .expression = null } };
            }
            const expression = try self.parseExpression(.lowest);
            if (needs_terminated) {
                try self.expectSemicolon();
            }
            break :blk .{ .@"return" = .{ .expression = expression } };
        },
        .@"break" => blk: {
            self.cursor.advance();
            if (needs_terminated) {
                try self.expectSemicolon();
            }
            // todo: break values
            break :blk .@"break";
        },
        .@"continue" => blk: {
            self.cursor.advance();
            if (needs_terminated) {
                try self.expectSemicolon();
            }
            break :blk .@"continue";
        },
        // skip comments
        .comment => blk: {
            self.cursor.advance();
            break :blk null;
        },
        inline else => blk: {
            // check for block statements before falling through to expression parsing
            if (self.currentIs(.left_brace)) {
                // if we encounter a left brace, we should check if it is a block statement or a dictionary
                const encountered_key = self.peekIs(.identifier) or self.peekIs(.string);

                // check for separator
                const potential_sep = self.cursor.peekAhead(2);
                const encountered_sep = potential_sep != null and potential_sep.?.token == .colon;
                // if we didn't encounter a key or a separator, we should parse a block statement
                if (!(encountered_key and encountered_sep)) {
                    break :blk .{ .block = try self.parseBlockStatement() };
                }
            }

            const expression = try self.parseExpression(.lowest);

            // try to parse an assignment statement
            // todo: expand expressions to include dot operator (e.g., this.is.a.nested.identifier)
            if (self.cursor.hasNext() and self.currentToken().isAssignment() and expression.canAssign()) {
                break :blk try self.parseAssignmentStatement(expression, needs_terminated);
            }

            var terminated: bool = false;

            switch (expression) {
                .for_expr, .while_expr, .if_expr => if (self.currentIs(.semicolon)) {
                    self.cursor.advance();
                    terminated = true;
                },
                inline else => if (needs_terminated) {
                    try self.expectSemicolon();
                    terminated = true;
                },
            }
            break :blk ast.createExpressionStatement(expression, terminated);
        },
    };
}

/// Reports an error if a semi-colon is missing
fn expectSemicolon(self: *Self) ParserError!void {
    if (self.currentIs(.semicolon)) {
        self.cursor.advance();
        return;
    }

    // todo: is there a better way to implement this?
    const prev = self.cursor.previous().?;
    self.diagnostics.report("expected ';' after statement", .{}, .{
        .token = prev.token,
        .position = .{ .start = prev.position.end + 1, .end = prev.position.end + 1 },
    });
    return ParserError.UnexpectedToken;
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

    // if the next token is a colon, we should expect a type name to be given
    const type_token = if (self.peekIs(.colon)) token: {
        self.cursor.advance();
        try self.expectPeekAndAdvance(.identifier);
        break :token self.currentToken();
    } else null;

    // we aren't storing the assignment token so we can skip past it once we peek at it
    try self.expectPeekAndAdvance(.assignment);
    self.cursor.advance();

    const expression = try self.parseExpression(.lowest);
    try self.expectSemicolon();
    return .{ .variable = .{
        .kind = kind_token,
        .name = identifier_token.identifier,
        .type = if (type_token) |token| token.identifier else null,
        .expression = expression,
    } };
}

fn parseAssignmentStatement(self: *Self, lhs: Expression, needs_terminated: bool) ParserError!Statement {
    if (!lhs.canAssign()) {
        return ParserError.UnexpectedToken;
    }
    const assignment_token_data = try self.readAndAdvance();
    if (!assignment_token_data.token.isAssignment()) {
        return ParserError.UnexpectedToken;
    }
    const rhs = try self.parseExpression(.lowest);
    if (needs_terminated) {
        try self.expectSemicolon();
    }

    // std.debug.print("Assignment: {s} {s} {s}\n", .{ lhs, assignment_token_data.token, rhs });
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
            self.diagnostics.report("expected identifier but got: {}", .{expr}, self.cursor.current());
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

fn parseDictExpression(self: *Self) ParserError!Expression {
    var keys = std.ArrayList(Expression).init(self.allocator());
    var values = std.ArrayList(Expression).init(self.allocator());
    errdefer keys.deinit();
    errdefer values.deinit();

    // parse as empty dictionary
    if (self.currentIs(.right_brace)) {
        self.cursor.advance();
        return .{ .dict = .{
            .keys = keys.toOwnedSlice() catch return ParserError.OutOfMemory,
            .values = values.toOwnedSlice() catch return ParserError.OutOfMemory,
        } };
    }

    while (self.cursor.canRead()) {
        const key = try self.parseExpression(.lowest);
        if (key != .identifier and key != .string) {
            self.diagnostics.report("expected identifier or string but got: {}", .{key}, self.cursor.current());
            return ParserError.UnexpectedToken;
        }
        try self.expectCurrentAndAdvance(.colon);
        const value = try self.parseExpression(.lowest);
        // if we encounter a comma, we should expect another key-value pair
        if (self.currentIs(.comma)) {
            self.cursor.advance();
        }

        keys.append(key) catch return ParserError.OutOfMemory;
        values.append(value) catch return ParserError.OutOfMemory;

        if (self.currentIs(.right_brace)) {
            self.cursor.advance();
            break;
        }
    }
    return .{ .dict = .{
        .keys = keys.toOwnedSlice() catch return ParserError.OutOfMemory,
        .values = values.toOwnedSlice() catch return ParserError.OutOfMemory,
    } };
}

fn parseIdentifier(self: *Self) ParserError!Expression {
    const current = try self.readAndAdvance();
    if (current.token != .identifier) {
        self.diagnostics.report("expected identifier but got: {}", .{current.token}, self.cursor.current());
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
        self.diagnostics.report("expected if but got: {}", .{self.currentToken()}, self.cursor.current());
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
        self.diagnostics.report("expected statement but got: {}", .{self.currentToken()}, self.cursor.current());
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

    try self.expectCurrentAndAdvance(.right_paren);

    // |i|
    const captures = try self.parseExpressionList(.pipe, .pipe);
    for (captures) |capture_ptr| {
        if (capture_ptr != .identifier) {
            self.diagnostics.report("expected identifier but got: {}", .{capture_ptr}, self.cursor.current());
            return ParserError.UnexpectedToken;
        }
    }
    // { ... }
    const body = try self.parseStatement(false) orelse {
        self.diagnostics.report("expected statement but got: {}", .{self.currentToken()}, self.cursor.current());
        return ParserError.UnexpectedToken;
    };

    return .{ .for_expr = .{
        .expr = try self.moveToHeap(expr),
        .captures = captures,
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
            self.diagnostics.report("expected range operator but got: {}", .{self.currentToken()}, self.cursor.current());
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
        .left_brace => try self.parseDictExpression(),
        .builtin => |name| try self.parseBuiltinExpression(name),
        .left_paren => blk: {
            const parsed = try self.parseExpression(.lowest);
            try self.expectCurrentAndAdvance(.right_paren);
            break :blk parsed;
        },
        inline else => {
            self.diagnostics.report("unable to parse '{s}' as prefix", .{current.token}, current);
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
        .dot => return try self.parseMemberExpression(lhs),
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
        self.diagnostics.report("expected identifier but got: {}", .{expr}, self.cursor.current());
        return ParserError.UnexpectedToken;
    }
    // rewind the cursor to make sure we can use `parseExpressionList`
    self.cursor.rewind() catch unreachable;
    const arguments = try self.parseExpressionList(.left_paren, .right_paren);
    return .{ .call = .{ .name = expr.identifier, .arguments = arguments } };
}

/// Parses a member expression given the LHS
fn parseMemberExpression(self: *Self, lhs: *Expression) ParserError!Expression {
    if (!self.currentIs(.identifier)) {
        self.diagnostics.report("expected identifier but got: {}", .{self.currentToken()}, self.cursor.current());
        return ParserError.UnexpectedToken;
    }
    const member = try self.readAndAdvance();
    return .{ .member = .{ .lhs = lhs, .member = member.token.identifier } };
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
        self.diagnostics.report("expected '{s}' but got EOF", .{tag.name()}, self.cursor.current());
        return ParserError.UnexpectedEOF;
    }
    if (!self.currentIs(tag)) {
        // todo: we should
        self.diagnostics.report("expected '{s}' but got '{s}'", .{ tag.name(), self.currentToken() }, self.cursor.current());
        return ParserError.ExpectedCurrentMismatch;
    }
    self.cursor.advance();
}

/// Throws an error if the current token is not the expected tag.
fn expectPeekAndAdvance(self: *Self, tag: TokenTag) ParserError!void {
    const peek = self.cursor.peek() orelse {
        self.diagnostics.report("expected token '{s}' but got EOF", .{tag.name()}, self.cursor.current());
        return ParserError.UnexpectedEOF;
    };
    if (!self.peekIs(tag)) {
        self.diagnostics.report("expected token: '{s}' but got '{s}'", .{ tag.name(), peek }, peek);
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
    var data = try honey.tokenize(input, ally);
    errdefer data.deinit();
    var parser = init(data, .{
        .ally = ally,
        .error_writer = std.io.getStdErr().writer().any(),
    });
    defer parser.deinit();
    var program = parser.parse() catch |err| {
        parser.report();
        return err;
    };
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
                .kind = .let,
                .type = null,
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

            const capture_i = ast.Expression{ .identifier = "i" };
            const expected = ast.createForStatement(
                &for_range,
                &.{capture_i},
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

test "test parsing simple dict expression" {
    try parseAndExpect("{ a: 1, b: \"test\", \"c\": false }", struct {
        pub fn func(program: *Program) anyerror!void {
            const a_key = ast.Expression{ .identifier = "a" };
            const b_key = ast.Expression{ .identifier = "b" };
            const c_key = ast.Expression{ .string = "c" };
            const keys = [_]ast.Expression{ a_key, b_key, c_key };

            const a_expr = ast.Expression{ .number = 1.0 };
            const b_expr = ast.Expression{ .string = "test" };
            const c_expr = ast.Expression{ .boolean = false };
            const values = [_]ast.Expression{ a_expr, b_expr, c_expr };

            const dict = ast.Expression{ .dict = .{ .keys = &keys, .values = &values } };
            const expected = ast.createExpressionStatement(dict, false);
            const statements = try program.statements.toOwnedSlice();
            try std.testing.expectEqualDeep(expected, statements[0]);
        }
    }.func);
}

test "test parsing empty dict expression" {
    try parseAndExpect("dict = {};", struct {
        pub fn func(program: *Program) anyerror!void {
            const dict = ast.Expression{ .dict = .{
                .keys = &.{},
                .values = &.{},
            } };

            const statements = try program.statements.toOwnedSlice();
            try std.testing.expectEqualDeep(dict, statements[0].assignment.rhs);
        }
    }.func);
}

test "test parsing simple indexing of list expression" {
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

test "test parsing simple indexing of dict expression" {
    try parseAndExpect("dict[\"key\"]", struct {
        pub fn func(program: *Program) anyerror!void {
            var list_expr = ast.Expression{ .identifier = "dict" };
            var index_expr = ast.Expression{ .string = "key" };

            const expected = ast.createIndexStatement(&list_expr, &index_expr, false);
            const statements = try program.statements.toOwnedSlice();
            try std.testing.expectEqualDeep(expected, statements[0]);
        }
    }.func);
}

test "test parsing simple member indexing of dict expression" {
    try parseAndExpect("dict.key", struct {
        pub fn func(program: *Program) anyerror!void {
            var list_expr = ast.Expression{ .identifier = "dict" };

            const expected = ast.createMemberStatement(&list_expr, "key", false);
            const statements = try program.statements.toOwnedSlice();
            try std.testing.expectEqualDeep(expected, statements[0]);
        }
    }.func);
}

test "test parsing nested member indexing of dict expression" {
    try parseAndExpect("a.b.c", struct {
        pub fn func(program: *Program) anyerror!void {
            var a_expr = ast.Expression{ .identifier = "a" };
            var b_expr = ast.Expression{ .member = .{ .lhs = &a_expr, .member = "b" } };

            const expected = ast.createMemberStatement(&b_expr, "c", false);
            const statements = try program.statements.toOwnedSlice();
            try std.testing.expectEqualDeep(expected, statements[0]);
        }
    }.func);
}

test "test span finding" {
    const honey = @import("../honey.zig");
    const source =
        \\const a = 1;
        \\const b = true;
        \\const c = "false";
        \\const d = null;
    ;
    const ally = std.testing.allocator;
    const data = try honey.tokenize(source, ally);
    var parser = init(data, .{
        .ally = ally,
        .error_writer = std.io.getStdErr().writer().any(),
    });
    defer parser.deinit();

    const true_token_data = data.tokens.items[8];
    try std.testing.expectEqual(.true, true_token_data.token.tag());

    const found_line_data = parser.findLine(true_token_data);
    try std.testing.expect(found_line_data != null);
    try std.testing.expectEqualDeep(utils.Span{ .start = 13, .end = 28 }, found_line_data.?);
}
