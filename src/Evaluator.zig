const std = @import("std");
const honey = @import("honey.zig");
const ast = @import("ast.zig");
const Program = ast.Program;
const Statement = ast.Statement;
const Expression = ast.Expression;

const Self = @This();

const VariableStore = struct {
    keys: std.BufSet,
    values: std.StringHashMap(Value),

    pub fn init(ally: std.mem.Allocator) VariableStore {
        return .{
            .keys = std.BufSet.init(ally),
            .values = std.StringHashMap(Value).init(ally),
        };
    }

    pub fn deinit(self: *VariableStore) void {
        self.keys.deinit();
        self.values.deinit();
    }

    pub fn delete(self: *VariableStore, name: []const u8) void {
        if (!self.values.contains(name)) {
            return;
        }
        self.keys.remove(name);
        self.values.remove(name);
    }

    pub fn store(self: *VariableStore, name: []const u8, value: Value) !void {
        const result = self.values.getOrPut(name) catch unreachable;
        if (!result.found_existing) {
            // we have to store the slices ourselves because the hashmap doesn't copy them
            try self.keys.insert(name);
            result.key_ptr.* = self.keys.hash_map.getKey(name).?;
        }
        result.value_ptr.* = value;
    }

    pub fn load(self: *VariableStore, name: []const u8) ?Value {
        return self.values.get(name);
    }
};

pub const Environment = struct {
    arena: std.heap.ArenaAllocator,
    globals: VariableStore,

    pub fn init(ally: std.mem.Allocator) Environment {
        return .{ .arena = std.heap.ArenaAllocator.init(ally), .globals = VariableStore.init(ally) };
    }

    pub fn deinit(self: *Environment) void {
        self.arena.deinit();
        self.globals.deinit();
    }

    fn allocator(self: *Environment) std.mem.Allocator {
        return self.arena.allocator();
    }
};

pub const BuiltinFn = *const fn (*Self, []const Value) ?Value;

pub const Value = union(enum) {
    const True = Value{ .boolean = true };
    const False = Value{ .boolean = false };
    const Null = Value{ .null = null };

    number: f64,
    string: []const u8,
    boolean: bool,
    null: void,

    pub fn format(self: Value, _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .number => |value| try writer.print("{d}", .{value}),
            .string => |value| try writer.writeAll(value),
            .boolean => |value| try writer.writeAll(if (value) "true" else "false"),
            .null => try writer.writeAll("null"),
        }
    }
};

allocator: std.mem.Allocator,
environment: *Environment,
builtins: std.StringHashMap(BuiltinFn),
stdout: std.fs.File.Writer,
stdin: std.fs.File.Reader,

pub fn init(ally: std.mem.Allocator, env: *Environment) Self {
    var self = Self{
        .allocator = ally,
        .environment = env,
        .stdout = std.io.getStdOut().writer(),
        .stdin = std.io.getStdIn().reader(),
        .builtins = std.StringHashMap(BuiltinFn).init(ally),
    };
    self.addBuiltinLibrary(@import("builtins.zig"));
    return self;
}

pub fn addBuiltinLibrary(self: *Self, comptime import: type) void {
    const decls = @typeInfo(import).Struct.decls;
    inline for (decls) |decl| {
        self.builtins.put(decl.name, @field(import, decl.name)) catch unreachable;
    }
}

pub fn deinit(self: *Self) void {
    self.builtins.deinit();
}

pub fn run(self: *Self, program: Program) ?Value {
    var output: ?Value = null;
    for (program.statements.items) |statement| {
        output = self.runStatement(statement);
    }
    return output;
}

fn runStatement(self: *Self, statement: Statement) ?Value {
    return switch (statement) {
        .let => |inner| self.runLetStatement(inner),
        .expression => |inner| self.runExpression(inner),
    };
}

fn runLetStatement(self: *Self, statement: ast.LetStatement) ?Value {
    // if (self.environment.globals.contains(statement.name)) {
    //     return null;
    // }
    const value = self.runExpression(statement.expression) orelse unreachable;
    self.environment.globals.store(statement.name, value) catch unreachable;
    return null;
}

fn runExpression(self: *Self, expression: Expression) ?Value {
    return switch (expression) {
        .binary => |inner| self.runBinaryExpression(inner),
        .identifier => |name| self.environment.globals.load(name),
        .prefix => |inner| self.runPrefixExpression(inner),
        .number => |value| .{ .number = value },
        .string => |value| .{ .string = value },
        .boolean => |value| if (value) Value.True else Value.False,
        .builtin => |inner| self.runBuiltinExpression(inner),
    };
}

fn runBinaryExpression(self: *Self, expression: ast.BinaryExpression) ?Value {
    const left = self.runExpression(expression.lhs.*) orelse unreachable;
    const right = self.runExpression(expression.rhs.*) orelse unreachable;
    return switch (expression.operator) {
        .plus => if (left == .string and right == .string) {
            return .{ .string = std.mem.concat(self.allocator, u8, &.{ left.string, right.string }) catch unreachable };
        } else if (left == .number and right == .number) {
            return .{ .number = left.number + right.number };
        } else {
            unreachable;
        },
        .minus => .{ .number = left.number - right.number },
        .star => .{ .number = left.number * right.number },
        .slash => .{ .number = left.number / right.number },
        .modulo => .{ .number = @mod(left.number, right.number) },
        .doublestar => .{ .number = std.math.pow(@TypeOf(left.number), left.number, right.number) },
    };
}

fn runPrefixExpression(self: *Self, expression: ast.PrefixExpression) ?Value {
    const right = self.runExpression(expression.rhs.*) orelse unreachable;
    return switch (expression.operator) {
        .plus => .{ .number = right.number },
        .minus => .{ .number = -right.number },
        inline else => unreachable,
    };
}

fn runBuiltinExpression(self: *Self, statement: ast.BuiltinExpression) ?Value {
    const run_func = self.builtins.get(statement.name) orelse return null;
    var args = std.ArrayList(Value).init(self.allocator);
    defer args.deinit();
    for (statement.arguments) |argument| {
        const value = self.runExpression(argument) orelse unreachable;
        args.append(value) catch unreachable;
    }
    return run_func(self, args.items);
}

fn runAndExpect(input: []const u8, test_func: *const fn (?Value) anyerror!void) anyerror!void {
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
