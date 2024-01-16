const std = @import("std");
const utils = @import("../utils/utils.zig");
const ast = @import("../parser/ast.zig");
const opcodes = @import("opcodes.zig");
const Opcode = opcodes.Opcode;
const Value = @import("value.zig").Value;
const Bytecode = @import("Bytecode.zig");

const Self = @This();

/// A simple arena allocator used when compiling into bytecode
arena: std.heap.ArenaAllocator,
/// The current program being compiled
program: ast.Program,
/// The instructions being generated
instructions: std.ArrayList(u8),
/// A list of constant expressions used in the program
constants: std.ArrayList(Value),

pub fn init(ally: std.mem.Allocator, program: ast.Program) Self {
    return .{
        .arena = std.heap.ArenaAllocator.init(ally),
        .program = program,
        .instructions = std.ArrayList(u8).init(ally),
        .constants = std.ArrayList(Value).init(ally),
    };
}

/// Deinitializes the compiler
pub fn deinit(self: *Self) void {
    self.arena.deinit();
    self.instructions.deinit();
    self.constants.deinit();
}

/// Adds an operation to the bytecode
fn addInstruction(self: *Self, instruction: opcodes.Instruction) !void {
    const writer = self.instructions.writer();
    // size of the opcode + size of the instruction
    const op: Opcode = std.meta.activeTag(instruction);
    try op.encode(writer);

    switch (instruction) {
        inline else => |value| try opcodes.encode(value, writer),
    }
}

/// Compiles the program into bytecode
pub fn compile(self: *Self) !Bytecode {
    for (self.program.statements.items) |statement| {
        try self.compileStatement(statement);
    }
    return .{ .instructions = self.instructions.items, .constants = self.constants.items };
}

/// Compiles a statement into bytecode
fn compileStatement(self: *Self, statement: ast.Statement) !void {
    switch (statement) {
        .expression => |inner| {
            try self.compileExpression(inner.expression);
            // pop the result of the statement off the stack after it's done
            try self.addInstruction(.pop);
        },
        .variable => {},
        else => @panic("unimplemented statement type"),
    }
}

inline fn resolvePrefixToInstr(operator: ast.Operator) opcodes.Instruction {
    return switch (operator) {
        .minus => .neg,
        inline else => utils.fmt.panicWithFormat("unexpected operator: {s}", .{operator}),
    };
}

/// Resolves an operator to its instruction equivalent
inline fn resolveInfixToInstr(operator: ast.Operator) opcodes.Instruction {
    return switch (operator) {
        .plus => .add,
        .minus => .sub,
        .star => .mul,
        .slash => .div,
        .modulo => .mod,
        .doublestar => .pow,
        .equal => .eql,
        .not_equal => .neql,
        .less_than => .lt,
        .less_than_equal => .lt_eql,
        .greater_than => .gt,
        .greater_than_equal => .gt_eql,
        inline else => utils.fmt.panicWithFormat("unexpected operator: {s}", .{operator}),
    };
}

/// Compiles an expression to opcodes
fn compileExpression(self: *Self, expression: ast.Expression) !void {
    switch (expression) {
        .binary => |inner| {
            try self.compileExpression(inner.lhs.*);
            try self.compileExpression(inner.rhs.*);
            try self.addInstruction(resolveInfixToInstr(inner.operator));
        },
        .prefix => |inner| {
            try self.compileExpression(inner.rhs.*);
            try self.addInstruction(resolvePrefixToInstr(inner.operator));
        },
        .number => |value| {
            try self.constants.append(.{ .number = value });
            try self.addInstruction(.{ .@"const" = @as(u16, @intCast(self.constants.items.len - 1)) });
        },
        .boolean => |inner| try self.addInstruction(if (inner) .true else .false),
        .null => try self.addInstruction(.null),
        inline else => utils.fmt.panicWithFormat("unexpected expression type: {s}", .{expression}),
    }
}

test "test simple addition compilation" {
    const ally = std.testing.allocator;

    var program = ast.Program.init(ally);
    defer program.deinit();

    // (1 + 2)
    var lhs = ast.Expression{ .number = 1 };
    var rhs = ast.Expression{ .number = 2 };
    try program.add(ast.createBinaryStatement(&lhs, .plus, &rhs, false));

    var compiler = Self.init(ally, program);
    defer compiler.deinit();

    const bytecode = try compiler.compile();
    try std.testing.expectEqualSlices(u8, opcodes.make(&.{
        .{ .@"const" = 0x00 },
        .{ .@"const" = 0x01 },
        .add,
        .pop,
    }), bytecode.instructions);
    try std.testing.expectEqualSlices(Value, &.{
        .{ .number = 1 },
        .{ .number = 2 },
    }, bytecode.constants);
}
