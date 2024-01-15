const std = @import("std");
const Cursor = @import("../utils/cursor.zig").Cursor;
const codec = @import("../utils/codec.zig");
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
        inline else => |value| try codec.encode(value, writer),
    }
}

/// Compiles the program into bytecode
pub fn compile(self: *Self) !Bytecode {
    for (self.program.statements.items) |statement| {
        try self.compileStatement(statement);
    }
    try self.addInstruction(.halt);
    return .{ .instructions = self.instructions.items, .constants = self.constants.items };
}

/// Compiles a statement into bytecode
fn compileStatement(self: *Self, statement: ast.Statement) !void {
    switch (statement) {
        .expression => |inner| try self.compileExpression(inner.expression),
        else => @panic("unimplemented statement type"),
    }
}

/// Resolves an operator to its instruction equivalent
inline fn resolveOpToInstr(operator: ast.Operator) opcodes.Instruction {
    return switch (operator) {
        .plus => .add,
        .minus => .sub,
        .star => .mul,
        .slash => .div,
        .modulo => .mod,
        .doublestar => .pow,
        inline else => @panic("Invalid operator"),
    };
}

/// Compiles an expression to opcodes
fn compileExpression(self: *Self, expression: ast.Expression) !void {
    switch (expression) {
        .binary => |inner| {
            try self.compileExpression(inner.lhs.*);
            try self.compileExpression(inner.rhs.*);
            try self.addInstruction(resolveOpToInstr(inner.operator));
        },
        .number => |value| {
            try self.constants.append(.{ .number = value });
            try self.addInstruction(.{ .@"const" = @as(u16, @intCast(self.constants.items.len - 1)) });
        },
        else => @panic("unimplemented expression type"),
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
        .halt,
    }), bytecode.instructions);
    try std.testing.expectEqualSlices(Value, &.{
        .{ .number = 1 },
        .{ .number = 2 },
    }, bytecode.constants);
}
