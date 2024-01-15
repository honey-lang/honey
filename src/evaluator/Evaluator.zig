const std = @import("std");
const Token = @import("../lexer/token.zig").Token;
const ast = @import("../parser/ast.zig");
const Program = ast.Program;
const Statement = ast.Statement;
const Expression = ast.Expression;
const Store = @import("../utils/store.zig").Store;

pub const Value = @import("value.zig").Value;
pub const Function = @import("Function.zig");

const Self = @This();

pub const Environment = struct {
    variables: Store(Value),
    constants: Store(Value),
    functions: Store(Function),
    parent: ?*Environment = null,

    /// Initializes an environment without a parent.
    pub fn init(ally: std.mem.Allocator) Environment {
        return .{
            .variables = Store(Value).init(ally),
            .constants = Store(Value).init(ally),
            .functions = Store(Function).init(ally),
        };
    }

    /// Initializes an environment with a parent.
    pub fn initWithParent(ally: std.mem.Allocator, parent: *Environment) Environment {
        var self = Environment.init(ally);
        self.parent = parent;
        return self;
    }

    pub fn functionExists(self: *Environment, name: []const u8) bool {
        return self.functions.contains(name) or if (self.parent) |parent| parent.hasFunction(name) else false;
    }

    pub fn getFunction(self: *Environment, name: []const u8) ?Function {
        if (self.functions.contains(name)) {
            return self.functions.load(name);
        } else if (self.parent) |parent| {
            return parent.getFunction(name);
        }
        return null;
    }

    pub fn getFunctionOrError(self: *Environment, name: []const u8) !Function {
        if (self.functions.contains(name)) {
            return self.functions.load(name) orelse return error.ExpectedValue;
        } else if (self.parent) |parent| {
            return parent.getFunctionOrError(name);
        }
        return error.FunctionDoesNotExist;
    }

    pub fn declareFunction(self: *Environment, name: []const u8, parameters: []const Expression, body: ast.BlockStatement) !void {
        if (self.functions.contains(name)) {
            return error.FunctionAlreadyExists;
        }
        self.functions.store(name, .{
            .name = name,
            .parameters = parameters,
            .body = body,
        }) catch return error.OutOfMemory;
    }

    /// Checks if a value exists in the environment.
    pub fn exists(self: *Environment, name: []const u8) bool {
        return self.variables.contains(name) or self.constants.contains(name);
    }

    /// Checks if a value exists in the environment or its parent.
    pub fn existsInAnyScope(self: *Environment, name: []const u8) bool {
        if (self.variables.contains(name)) {
            return true;
        } else if (self.constants.contains(name)) {
            return true;
        } else if (self.parent) |parent| {
            return parent.existsInAnyScope(name);
        }
        return false;
    }

    /// Returns true if a declared variable is a constant
    pub fn isConst(self: *Environment, name: []const u8) bool {
        return self.constants.contains(name) or if (self.parent) |parent| parent.isConst(name) else false;
    }

    /// Loads a value from the environment or its parent.
    pub fn load(self: *Environment, name: []const u8) ?Value {
        if (self.variables.contains(name)) {
            return self.variables.load(name);
        } else if (self.constants.contains(name)) {
            return self.constants.load(name);
        } else if (self.parent) |parent| {
            return parent.load(name);
        }
        return null;
    }

    pub fn loadOrError(self: *Environment, name: []const u8) EvaluatorError!Value {
        if (self.variables.contains(name)) {
            return self.variables.load(name) orelse return error.ExpectedValue;
        } else if (self.constants.contains(name)) {
            return self.constants.load(name) orelse return error.ExpectedValue;
        } else if (self.parent) |parent| {
            return parent.loadOrError(name);
        }
        return error.VariableDoesNotExist;
    }

    /// Stores a value in the environment.
    pub fn store(self: *Environment, name: []const u8, value: Value, container_type: Token) EvaluatorError!void {
        var type_store = switch (container_type) {
            .let => &self.variables,
            .@"const" => &self.constants,
            inline else => unreachable,
        };
        type_store.store(name, value) catch return error.OutOfMemory;
    }

    pub fn storeConst(self: *Environment, name: []const u8, value: Value) EvaluatorError!void {
        if (self.constants.contains(name)) {
            return error.VariableAlreadyExists;
        }
        self.constants.store(name, value) catch return error.OutOfMemory;
    }

    pub fn reassign(self: *Environment, name: []const u8, value: Value) EvaluatorError!void {
        if (self.variables.contains(name)) {
            self.variables.store(name, value) catch return error.OutOfMemory;
            return;
        } else if (self.parent) |parent| {
            try parent.reassign(name, value);
            return;
        }
        return error.VariableDoesNotExist;
    }

    /// Deinitializes the environment.
    pub fn deinit(self: *Environment) void {
        self.variables.deinit();
        self.constants.deinit();
        self.functions.deinit();
    }
};

pub const BuiltinFn = *const fn (*Self, []const Value) anyerror!?Value;

const Diagnostics = struct {
    arena: std.heap.ArenaAllocator,

    pub fn init(ally: std.mem.Allocator) Diagnostics {
        return .{ .arena = std.heap.ArenaAllocator.init(ally) };
    }

    pub fn deinit(self: *Diagnostics) void {
        self.arena.deinit();
    }
};

const EvaluatorError = error{
    OutOfMemory,
    VariableAlreadyExists,
    VariableDoesNotExist,
    FunctionDoesNotExist,
    ExpectedValue,
    BuiltinDoesNotExist,
    BuiltinRunError,
};

const RunOptions = struct {
    environment: *Environment,
    diagnostics: *Diagnostics,

    pub fn scopedEnv(self: RunOptions, inner: *Environment) RunOptions {
        return .{ .environment = inner, .diagnostics = self.diagnostics };
    }
};

arena: std.heap.ArenaAllocator,
environment: *Environment,
builtins: std.StringHashMap(BuiltinFn),
stdout: std.fs.File.Writer,
stdin: std.fs.File.Reader,

pub fn init(ally: std.mem.Allocator, env: *Environment) Self {
    var self = Self{
        .arena = std.heap.ArenaAllocator.init(ally),
        .environment = env,
        .stdout = std.io.getStdOut().writer(),
        .stdin = std.io.getStdIn().reader(),
        .builtins = std.StringHashMap(BuiltinFn).init(ally),
    };
    self.addBuiltinLibrary(@import("builtins.zig"));
    return self;
}

pub fn allocator(self: *Self) std.mem.Allocator {
    return self.arena.allocator();
}

pub fn deinit(self: *Self) void {
    self.builtins.deinit();
}

pub fn addBuiltinLibrary(self: *Self, comptime import: type) void {
    const decls = @typeInfo(import).Struct.decls;
    inline for (decls) |decl| {
        self.builtins.put(decl.name, @field(import, decl.name)) catch unreachable;
    }
}

pub fn run(self: *Self, program: Program) anyerror!?Value {
    var output: ?Value = null;
    var diagnostics = Diagnostics.init(self.allocator());
    for (program.statements.items) |statement| {
        output = try self.runStatement(statement, .{ .environment = self.environment, .diagnostics = &diagnostics });
    }
    return output;
}

fn runStatement(self: *Self, statement: Statement, options: RunOptions) anyerror!?Value {
    return switch (statement) {
        .variable => |inner| {
            try self.runVariableStatement(inner, options);
            return null;
        },
        .assignment => |inner| {
            try self.runAssignmentStatement(inner, options);
            return null;
        },
        .@"fn" => |inner| {
            try self.runFunctionDeclaration(inner, options);
            return null;
        },
        .block => |inner| {
            _ = try self.runBlockStatement(inner, options);
            return null;
        },
        .@"return" => |inner| if (inner.expression) |expr| try self.runNonVoidExpression(expr, options) else Value.Void,
        .expression => |inner| try self.runExpression(inner.expression, options),
    };
}

fn runVariableStatement(self: *Self, statement: ast.VariableStatement, options: RunOptions) anyerror!void {
    // TODO: throw error if variable already exists
    if (options.environment.exists(statement.name)) {
        return error.VariableAlreadyExists;
    }
    const value = try self.runExpression(statement.expression, options);
    options.environment.store(statement.name, value, statement.type) catch return error.OutOfMemory;
}

fn runAssignmentStatement(self: *Self, statement: ast.AssignmentStatement, options: RunOptions) anyerror!void {
    if (options.environment.isConst(statement.name)) {
        return error.UnableToReassignConstant;
    } else if (!options.environment.existsInAnyScope(statement.name)) {
        return error.VariableDoesNotExist;
    }

    const value = try self.runExpression(statement.expression, options);
    try self.environment.reassign(statement.name, switch (statement.type) {
        .plus_assignment => blk: {
            const old_value = try self.environment.loadOrError(statement.name);
            break :blk try old_value.plus(value, self.allocator());
        },
        .minus_assignment => blk: {
            const old_value = try self.environment.loadOrError(statement.name);
            break :blk try old_value.minus(value);
        },
        .star_assignment => blk: {
            const old_value = try self.environment.loadOrError(statement.name);
            break :blk try old_value.multiply(value);
        },
        .slash_assignment => blk: {
            const old_value = try self.environment.loadOrError(statement.name);
            break :blk try old_value.divide(value);
        },
        .assignment => value,
        inline else => unreachable,
    });
}

fn runFunctionDeclaration(_: *Self, statement: ast.FunctionDeclaration, options: RunOptions) !void {
    try options.environment.declareFunction(statement.name, statement.parameters, statement.body);
}

fn runBlockStatement(self: *Self, block: ast.BlockStatement, options: RunOptions) anyerror!Value {
    var env = Environment.initWithParent(self.allocator(), options.environment);
    defer env.deinit();

    var result: Value = Value.Void;
    for (block.statements, 0..) |statement, index| {
        const output = try self.runStatement(statement, options.scopedEnv(&env));
        if (statement == .@"return" or (statement == .expression and !statement.expression.terminated and index == block.statements.len - 1)) {
            result = output orelse Value.Void;
            break;
        }
    }
    return result;
}

fn runExpression(self: *Self, expression: Expression, options: RunOptions) anyerror!Value {
    return switch (expression) {
        .binary => |inner| try self.runBinaryExpression(inner, options),
        .prefix => |inner| try self.runPrefixExpression(inner, options),
        .identifier => |name| try options.environment.loadOrError(name),
        .number => |value| .{ .number = value },
        .string => |value| .{ .string = value },
        .boolean => |value| if (value) Value.True else Value.False,
        .null => Value.Null,
        .@"if" => |inner| try self.runIfExpression(inner, options),
        .@"while" => |inner| try self.runWhileExpression(inner, options),
        .builtin => |inner| try self.runBuiltinExpression(inner, options),
        .call => |inner| try self.runCallExpression(inner, options),
        .fn_ref => unreachable,
        .callback => unreachable,
    };
}

fn runNonVoidExpression(self: *Self, expression: Expression, options: RunOptions) anyerror!Value {
    const value = try self.runExpression(expression, options);
    if (value.isVoid()) {
        return error.ExpectedValue;
    }
    return value;
}

fn runBinaryExpression(self: *Self, expression: ast.BinaryExpression, options: RunOptions) anyerror!Value {
    const left = try self.runNonVoidExpression(expression.lhs.*, options);
    const right = try self.runNonVoidExpression(expression.rhs.*, options);
    return switch (expression.operator) {
        .plus => try left.plus(right, self.allocator()),
        .minus => try left.minus(right),
        .star => try left.multiply(right),
        .slash => try left.divide(right),
        .modulo => try left.modulo(right),
        .doublestar => try left.power(right),
        .@"or" => try left.@"or"(right),
        .@"and" => try left.@"and"(right),
        .equal => .{ .boolean = left.equal(right) },
        .not_equal => .{ .boolean = !left.equal(right) },
        .greater_than => try left.greaterThan(right),
        .greater_than_equal => try left.greaterThanEqual(right),
        .less_than => try left.lessThan(right),
        .less_than_equal => try left.lessThanEqual(right),
    };
}

fn runPrefixExpression(self: *Self, expression: ast.PrefixExpression, options: RunOptions) anyerror!Value {
    const right = try self.runNonVoidExpression(expression.rhs.*, options);
    return switch (expression.operator) {
        .plus => .{ .number = right.number },
        .minus => .{ .number = -right.number },
        inline else => @panic("invalid prefix operator"),
    };
}

fn runIfExpression(self: *Self, expression: ast.IfExpression, options: RunOptions) anyerror!Value {
    for (expression.condition_list) |current| {
        const result = try self.runNonVoidExpression(current.condition.*, options);
        if (result != .boolean) {
            return error.ExpectedBoolean;
        }

        if (result.boolean) {
            return try self.runIfBody(current.body, options);
        }
    }
    if (expression.alternative) |alternative| {
        return try self.runIfBody(alternative, options);
    }
    return Value.Void;
}

fn runIfBody(self: *Self, body: ast.IfExpression.Body, options: RunOptions) anyerror!Value {
    return switch (body) {
        .block => |block| try self.runBlockStatement(block, options),
        .expression => |expr| return try self.runNonVoidExpression(expr.*, options),
    };
}

fn runWhileExpression(self: *Self, expression: ast.WhileExpression, options: RunOptions) anyerror!Value {
    while (true) {
        const result = try self.runNonVoidExpression(expression.condition.*, options);
        if (result != .boolean) {
            return error.ExpectedBoolean;
        }

        if (!result.boolean) {
            break;
        }
        _ = try self.runBlockStatement(expression.body, options);
    }
    return Value.Void;
}

fn runBuiltinExpression(self: *Self, statement: ast.BuiltinExpression, options: RunOptions) anyerror!Value {
    const run_func = self.builtins.get(statement.name) orelse return error.BuiltinDoesNotExist;
    var args = std.ArrayList(Value).init(self.allocator());
    defer args.deinit();
    for (statement.arguments) |argument| {
        const value = try self.runNonVoidExpression(argument, options);
        args.append(value) catch return error.OutOfMemory;
    }
    const output = run_func(self, args.items) catch return error.BuiltinRunError;
    return if (output) |value| value else Value.Void;
}

fn runCallExpression(self: *Self, expression: ast.FunctionCallExpression, options: RunOptions) anyerror!Value {
    const function = try options.environment.getFunctionOrError(expression.name);
    var env = Environment.initWithParent(self.allocator(), options.environment);
    defer env.deinit();
    for (function.parameters, 0..) |parameter, index| {
        const value = try self.runNonVoidExpression(expression.arguments[index], options);
        env.storeConst(parameter.identifier, value) catch return error.OutOfMemory;
    }
    return try self.runBlockStatement(function.body, options.scopedEnv(&env));
}

fn runAndExpect(input: []const u8, test_func: *const fn (?Value) anyerror!void) anyerror!void {
    const honey = @import("../honey.zig");
    const ally = std.testing.allocator;
    var env = Environment.init(ally);
    defer env.deinit();
    const output = try honey.run(input, ally, &env);
    try test_func(output);
}

test "test simple (1 + 2) evaluation" {
    try runAndExpect("1 + 2", struct {
        fn func(output: ?Value) anyerror!void {
            try std.testing.expect(output != null);
            try std.testing.expectEqual(@as(f64, @floatCast(3.0)), output.?.number);
        }
    }.func);
}

test "test simple @rand builtin" {
    try runAndExpect("@rand()", struct {
        fn func(output: ?Value) anyerror!void {
            try std.testing.expect(output != null);
            try std.testing.expect(output.?.number >= 0.0);
            try std.testing.expect(output.?.number < 1.0);
        }
    }.func);
}

test "test simple boolean evaluation" {
    try runAndExpect("true", struct {
        fn func(output: ?Value) anyerror!void {
            try std.testing.expect(output != null);
            try std.testing.expect(output.?.boolean);
        }
    }.func);
}
