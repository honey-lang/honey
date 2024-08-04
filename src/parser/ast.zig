const std = @import("std");
const Token = @import("../lexer/token.zig").Token;
const TokenData = @import("../lexer/token.zig").TokenData;

pub const Operator = enum {
    /// `plus` is the `+` operator. It adds two numbers together.
    plus,
    /// `minus` is the `-` operator. It subtracts two numbers.
    minus,
    /// `star` is the `*` operator. It multiplies two numbers.
    star,
    /// `slash` is the `/` operator. It divides two numbers.
    slash,
    /// `modulo` is the `%` operator. It gets the remainder of two numbers.
    modulo,
    /// `doublestar` is the `**` operator. It raises a number to the power of another number.
    doublestar,
    /// `or` is the `or` operator.
    /// If either the first or second operand is true, the result is true.
    @"or",
    /// `and` is the `and` operator.
    /// If both the first and second operand are true, the result is true.
    @"and",
    /// `equal` is the `==` operator. It checks if two values are equal.
    equal,
    /// `not_equal` is the `!=` operator. It checks if two values are not equal.
    not_equal,
    /// `greater_than` is the `>` operator. It checks if the first value is greater than the second value.
    greater_than,
    /// `greater_than_equal` is the `>=` operator. It checks if the first value is greater than or equal to the second value.
    greater_than_equal,
    /// `less_than` is the `<` operator. It checks if the first value is less than the second value.
    less_than,
    /// `less_than_equal` is the `<=` operator. It checks if the first value is less than or equal to the second value.
    less_than_equal,
    /// `not` is the `!` operator. It negates a boolean value.
    not,

    pub fn fromTokenData(data: TokenData) !Operator {
        return switch (data.token) {
            .plus => .plus,
            .minus => .minus,
            .star => .star,
            .slash => .slash,
            .modulo => .modulo,
            .doublestar => .doublestar,
            .@"or" => .@"or",
            .@"and" => .@"and",
            .equal => .equal,
            .not_equal => .not_equal,
            .greater_than => .greater_than,
            .greater_than_equal => .greater_than_equal,
            .less_than => .less_than,
            .less_than_equal => .less_than_equal,
            .bang => .not,
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
            .@"or" => try writer.writeAll("or"),
            .@"and" => try writer.writeAll("and"),
            .equal => try writer.writeAll("=="),
            .not_equal => try writer.writeAll("!="),
            .greater_than => try writer.writeAll(">"),
            .greater_than_equal => try writer.writeAll(">="),
            .less_than => try writer.writeAll("<"),
            .less_than_equal => try writer.writeAll("<="),
            .not => try writer.writeAll("!"),
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
    expression: ExpressionStatement,
    variable: VariableStatement,
    assignment: AssignmentStatement,
    @"fn": FunctionDeclaration,
    block: BlockStatement,
    @"return": ReturnStatement,
    @"break": void,
    @"continue": void,

    pub fn format(self: Statement, _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        return switch (self) {
            .@"break", .@"continue" => try writer.print("{s};", .{@tagName(self)}),
            inline else => |inner| writer.print("{s}", .{inner}),
        };
    }
};

pub const ExpressionStatement = struct {
    expression: Expression,
    terminated: bool,

    pub fn format(self: ExpressionStatement, _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        return writer.print("{s}{s}", .{ self.expression, if (self.terminated) ";" else "" });
    }
};

pub const VariableStatement = struct {
    kind: Token,
    name: []const u8,
    type: ?[]const u8 = null,
    expression: Expression,

    pub fn isConst(self: VariableStatement) bool {
        return self.kind == .@"const";
    }

    pub fn hasType(self: VariableStatement) bool {
        return self.type != null;
    }

    pub fn format(self: VariableStatement, _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        return writer.print("{s} {s}{s}{s} = {s};", .{
            if (self.kind == .let) "let" else "const",
            self.name,
            if (self.type != null) ": " else "",
            if (self.type) |type_name| type_name else "",
            self.expression,
        });
    }
};

pub const AssignmentStatement = struct {
    lhs: Expression,
    type: Token,
    rhs: Expression,

    pub fn format(self: AssignmentStatement, _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        return writer.print("{s} {s} {s};", .{ self.lhs, self.type, self.rhs });
    }

    /// Returns true if the assignment is a simple assignment.
    pub fn isSimple(self: AssignmentStatement) bool {
        return self.type == .assignment;
    }
};

pub const FunctionDeclaration = struct {
    name: []const u8,
    parameters: []const Expression,
    body: BlockStatement,

    pub fn format(self: FunctionDeclaration, _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("fn {s}(", .{self.name});
        for (self.parameters, 0..) |parameter, index| {
            try writer.print("{s}", .{parameter});
            if (index + 1 < self.parameters.len) {
                try writer.writeAll(", ");
            }
        }
        try writer.writeAll(") ");
        try writer.print("{s}", .{self.body});
    }
};

pub const BlockStatement = struct {
    statements: []const Statement,

    pub fn format(self: BlockStatement, _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.writeAll("{\n");
        for (self.statements) |statement| {
            try writer.print("{s}\n", .{statement});
        }
        try writer.writeAll("}\n");
    }
};

pub const ReturnStatement = struct {
    expression: ?Expression,

    pub fn format(self: ReturnStatement, _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        if (self.expression) |expr| {
            try writer.print("return {s};", .{expr});
        } else {
            try writer.writeAll("return;");
        }
    }
};

/// A prefix expression is an expression that has one operand.
/// For example, `-1` is a prefix expression.
pub const PrefixExpression = struct {
    operator: Operator,
    rhs: *Expression,
};

/// A binary expression is an expression that has two operands.
/// For example, `1 + 2` is a binary expression.
pub const BinaryExpression = struct {
    lhs: *Expression,
    operator: Operator,
    rhs: *Expression,
};

/// An if expression is a conditional expression.
/// For example, `if (true) 1 else 2` is an if expression.
/// If expressions can have multiple conditions, such as `if (true) 1 else if (false) 2 else 3`.
/// They can be blocks or expresssions, such as `if (true) { return 1; } else { return 2; }` or `if (true) 1 else 2`.
pub const IfExpression = struct {
    pub const Body = union(enum) {
        block: BlockStatement,
        expression: *Expression,

        pub fn format(self: Body, _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            return switch (self) {
                inline else => |inner| writer.print("{s}", .{inner}),
            };
        }
    };
    pub const ConditionData = struct { condition: *Expression, body: Body };

    condition_list: []const ConditionData,
    alternative: ?Body,
};

/// A while expression is a loop expression.
/// For example, `while (true) { doSomething(); }` is a while expression.
/// While expressions can also contain a post-statement that is executed after the loop body is executed.
/// For example, `while (true): (i += 1) { doSomething(); }` is a while expression with a post-statement.
pub const WhileExpression = struct {
    condition: *Expression,
    body: *Statement,
    post_stmt: ?*Statement,
};

/// A range expression is an expression that represents a range of values.
/// Exclusive: for(0..10) |i| {}
/// Inclusive: for(0...10) |i| {}
pub const RangeExpression = struct {
    start: *Expression,
    end: *Expression,
    inclusive: bool,
};

/// A list expression is an expression that represents a list of values.
/// For example, `["a", 2, true, var_name, null]
pub const ListExpression = struct {
    expressions: []const Expression,
};

/// A dictionary expression is an expression that represents a dictionary of key-value pairs.
/// For example, `{"a": 1, "b": 2, "c": 3}`
pub const DictExpression = struct {
    keys: []const Expression,
    values: []const Expression,
};

/// An index expression is a list access operation that takes an expression and an index
pub const IndexExpression = struct {
    lhs: *Expression,
    index: *Expression,
};

/// A member expression is an expression that accesses a member of a class or dictionary.
/// For example, `foo.bar` is a member expression.
pub const MemberExpression = struct {
    lhs: *Expression,
    member: []const u8,
};

/// A for expression is a loop expression that iterates over a range of values or a collection.
/// For example, `for(0..10) |i| { doSomething(i); }` is a for expression.
pub const ForExpression = struct {
    expr: *Expression,
    captures: []const Expression,
    body: *Statement,
};

/// Builtins are functions that are built into the language.
/// They are not user-defined and are used to provide basic functionality.
/// For example, `@print("Hello, world!")` is a builtin statement.
pub const BuiltinExpression = struct {
    name: []const u8,
    arguments: []const Expression,
};

/// A function call expression is a call to a function.
/// For example, `foo(1, 2, 3)` is a function call expression.
pub const FunctionCallExpression = struct {
    name: []const u8,
    arguments: []const Expression,
};

/// A callback expression is a function that is defined inline.
/// For example, `fn (a, b) { return a + b; }` is a callback expression.
pub const CallbackExpression = struct {
    name: []const u8,
    parameters: []const Expression,
    body: BlockStatement,
};

pub const Expression = union(enum) {
    /// A number literal, such as `1`, `2.5`, or `3.14159`.
    number: f64,
    /// An identifier, such as `foo`, `bar`, or `baz`.
    identifier: []const u8,
    /// A string literal, such as `"Hello, world!"`.
    string: []const u8,
    /// A boolean literal, either `true` or `false`.
    boolean: bool,
    /// A null literal
    null: void,
    /// A prefix expression, such as `-1`, `!true`, or `~0`.
    prefix: PrefixExpression,
    /// A binary expression, such as `1 + 2`, `3 * 4`, or `5 / 6`.
    binary: BinaryExpression,
    /// A range expression, such as `0..10` or `0...10`.
    range: RangeExpression,
    /// A list expression, such as `[1, 2, 3]` or `["a", 2, "c"]
    list: ListExpression,
    /// A dictionary expression, such as `{"a": 1, "b": 2, "c": 3}`.
    dict: DictExpression,
    /// An index expression, such as `list[0]` or `[0, 1, 2, 3][2]
    index: IndexExpression,
    /// A member expression, such as `foo.bar` or `baz.qux`.
    member: MemberExpression,
    /// An if expression, such as `if (true) { 1 } else { 2 }`.
    /// TODO: Rename back to @"if" when ZLS fixes the bug with @"" identifiers.
    if_expr: IfExpression,
    /// A while expression, such as `while (true) { doSomething(); }`.
    while_expr: WhileExpression,
    /// A for expression, such as `for(0..10) |i| { doSomething(i); }`.
    for_expr: ForExpression,
    /// A builtin expression, such as `@print("Hello, world!")`.
    builtin: BuiltinExpression,
    /// A function call expression, such as `foo(1, 2, 3)`.
    call: FunctionCallExpression,
    /// A callback expression, such as `fn (a, b) { return a + b; }`.
    callback: CallbackExpression,
    /// A reference to a function, such as `foo` or `bar`. Written as `foo(...)` or `bar(...)`.
    fn_ref: []const u8,

    /// Returns true if the expression can be on the LHS of an assignment.
    pub fn canAssign(self: Expression) bool {
        return self == .identifier or self == .index or self == .member;
    }

    pub fn format(self: Expression, _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        return switch (self) {
            .number => |inner| writer.print("{d}", .{inner}),
            .identifier => |inner| writer.print("{s}", .{inner}),
            .string => |inner| writer.print("\"{s}\"", .{inner}),
            .boolean => |inner| writer.writeAll(if (inner) "true" else "false"),
            .null => writer.writeAll("null"),
            .prefix => |inner| writer.print("({s}{s})", .{ inner.operator, inner.rhs }),
            .binary => |inner| writer.print("({s} {s} {s})", .{ inner.lhs, inner.operator, inner.rhs }),
            .range => |inner| writer.print("{s}..{s}{s}", .{ inner.start, if (inner.inclusive) "." else "", inner.end }),
            .list => |inner| {
                try writer.writeAll("[");
                for (inner.expressions, 0..) |expr, index| {
                    try writer.print("{s}", .{expr});
                    if (index + 1 < inner.expressions.len) {
                        try writer.writeAll(", ");
                    }
                }
                try writer.writeAll("]");
            },
            .dict => |inner| {
                try writer.writeAll("{");
                for (inner.keys, inner.values, 0..) |key, value, index| {
                    try writer.print("{s}: {s}", .{ key, value });
                    if (index + 1 < inner.keys.len) {
                        try writer.writeAll(", ");
                    }
                }
                try writer.writeAll("}");
            },
            .index => |inner| {
                try writer.print("{s}[{s}]", .{ inner.lhs, inner.index });
            },
            .member => |inner| {
                try writer.print("{s}.{s}", .{ inner.lhs, inner.member });
            },
            .if_expr => |inner| {
                try writer.writeAll("if (");
                for (inner.condition_list, 0..) |condition, index| {
                    try writer.print("{s}) {s}", .{ condition.condition, condition.body });
                    if (index + 1 < inner.condition_list.len) {
                        try writer.writeAll(" else if ");
                    }
                }
                if (inner.alternative) |alternative| {
                    try writer.print(" else {s}", .{alternative});
                }
            },
            .while_expr => |inner| writer.print("while ({s}) {s}", .{ inner.condition, inner.body }),
            .for_expr => |inner| writer.print("for ({s}) {s}", .{ inner.expr, inner.body }),
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
            .call => |inner| {
                try writer.print("{s}(", .{inner.name});
                for (inner.arguments, 0..) |argument, index| {
                    try writer.print("{s}", .{argument});
                    if (index + 1 < inner.arguments.len) {
                        try writer.writeAll(", ");
                    }
                }
                try writer.writeAll(")");
            },
            .callback => |inner| {
                try writer.writeAll("fn (");
                for (inner.parameters, 0..) |parameter, index| {
                    try writer.print("{s}", .{parameter});
                    if (index + 1 < inner.parameters.len) {
                        try writer.writeAll(", ");
                    }
                }
                try writer.print(") {s}", .{inner.body});
            },
            .fn_ref => |inner| writer.print("{s}(...)", .{inner}),
        };
    }
};

pub fn createExpressionStatement(expression: Expression, terminated: bool) Statement {
    return .{ .expression = .{ .expression = expression, .terminated = terminated } };
}

pub fn createPrefixStatement(operator: Operator, rhs: *Expression, terminated: bool) Statement {
    return .{ .expression = .{ .expression = .{ .prefix = .{ .operator = operator, .rhs = rhs } }, .terminated = terminated } };
}

pub fn createBuiltinStatement(name: []const u8, arguments: []const Expression, terminated: bool) Statement {
    return .{ .expression = .{ .expression = .{ .builtin = .{ .name = name, .arguments = arguments } }, .terminated = terminated } };
}

pub fn createBinaryStatement(lhs: *Expression, operator: Operator, rhs: *Expression, terminated: bool) Statement {
    return .{ .expression = .{ .expression = .{ .binary = .{ .lhs = lhs, .operator = operator, .rhs = rhs } }, .terminated = terminated } };
}

pub fn createIfStatement(condition_list: []const IfExpression.ConditionData, alternative: ?IfExpression.Body, terminated: bool) Statement {
    return .{ .expression = .{ .expression = .{ .if_expr = .{ .condition_list = condition_list, .alternative = alternative } }, .terminated = terminated } };
}

pub fn createWhileStatement(condition: *Expression, body: *Statement, post_stmt: ?*Statement, terminated: bool) Statement {
    return .{ .expression = .{ .expression = .{ .while_expr = .{ .condition = condition, .body = body, .post_stmt = post_stmt } }, .terminated = terminated } };
}

pub fn createForStatement(expr: *Expression, captures: []const Expression, body: *Statement, terminated: bool) Statement {
    return .{ .expression = .{ .expression = .{ .for_expr = .{ .expr = expr, .captures = captures, .body = body } }, .terminated = terminated } };
}

pub fn createIndexStatement(expr: *Expression, index_expr: *Expression, terminated: bool) Statement {
    return .{ .expression = .{ .expression = .{ .index = .{ .lhs = expr, .index = index_expr } }, .terminated = terminated } };
}

pub fn createMemberStatement(expr: *Expression, member: []const u8, terminated: bool) Statement {
    return .{ .expression = .{ .expression = .{ .member = .{ .lhs = expr, .member = member } }, .terminated = terminated } };
}

pub fn createCallStatement(name: []const u8, arguments: []const Expression, terminated: bool) Statement {
    return .{ .expression = .{ .expression = .{ .call = .{ .name = name, .arguments = arguments } }, .terminated = terminated } };
}

pub fn createCallbackStatement(name: []const u8, parameters: []const Expression, body: BlockStatement, terminated: bool) Statement {
    return .{ .expression = .{ .expression = .{ .callback = .{ .name = name, .parameters = parameters, .body = body } }, .terminated = terminated } };
}

pub fn createAssignStatement(lhs: Expression, @"type": Token, rhs: Expression) Statement {
    return .{ .assignment = .{ .lhs = lhs, .type = @"type", .rhs = rhs } };
}

pub fn createVariableStatement(kind: Token, name: []const u8, @"type": ?[]const u8, expression: Expression) Statement {
    return .{ .variable = .{ .kind = kind, .name = name, .type = @"type", .expression = expression } };
}

pub fn createFunctionStatement(name: []const u8, parameters: []const Expression, body: BlockStatement) Statement {
    return .{ .@"fn" = .{ .name = name, .parameters = parameters, .body = body } };
}

pub fn createReturnStatement(expression: ?Expression) Statement {
    return .{ .@"return" = .{ .expression = expression } };
}

pub fn createBlockStatement(statements: []const Statement) Statement {
    return .{ .block = .{ .statements = statements } };
}
