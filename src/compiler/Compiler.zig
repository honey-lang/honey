const std = @import("std");
const utils = @import("../utils/utils.zig");
const ast = @import("../parser/ast.zig");
const opcodes = @import("opcodes.zig");
const Opcode = opcodes.Opcode;
const Instruction = opcodes.Instruction;
const Value = @import("value.zig").Value;
const Bytecode = @import("Bytecode.zig");

/// This represents a compiled instruction that has been added to the bytecode
const CompiledInstruction = struct {
    /// The instruction's opcode
    opcode: Opcode,
    /// The index that the compiled instruction resides at in the bytecode
    index: usize,

    /// Returns the index of the (potential) next instruction given its index and its size
    pub fn nextInstructionIndex(self: CompiledInstruction) usize {
        return self.index + self.opcode.size();
    }
};

/// The maximum offset that can be used in a jump instruction. Used as a placeholder until replaced with the actual offset
const MaxOffset = std.math.maxInt(u16);

const Self = @This();

const Error = error{
    /// Occurs when the compiler encounters an invalid opcode
    InvalidInstruction,
    /// Occurs when an expected opcode isn't matched by the current opcode
    OpcodeReplaceMismatch,
    /// Occurs when an opcode's data can't be replaced
    OpcodeReplaceFailure,
    /// Occurs when an opcode fails to encode
    OpcodeEncodeFailure,
    /// Occurs when a statement that isn't supported in the compiler yet
    UnsupportedStatement,
    /// Occurs when an expression that isn't supported in the compiler yet
    UnsupportedExpression,
    /// Occurs when the compiler's allocator is out of memory
    OutOfMemory,
};

/// A simple arena allocator used when compiling into bytecode
arena: std.heap.ArenaAllocator,
/// The current program being compiled
program: ast.Program,
/// The instructions being generated
instructions: std.ArrayList(u8),
/// A list of constant expressions used in the program
constants: std.ArrayList(Value),
/// The last instruction that was added
last_compiled_instr: ?CompiledInstruction = null,
/// The instruction before the last instruction
penult_compiled_instr: ?CompiledInstruction = null,

/// Initializes the compiler
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
fn addInstruction(self: *Self, instruction: opcodes.Instruction) Error!void {
    const writer = self.instructions.writer();

    const index = self.instructions.items.len;
    try encode(writer, instruction);

    // set last as penultimate instr
    self.penult_compiled_instr = self.last_compiled_instr;

    self.last_compiled_instr = CompiledInstruction{
        .opcode = std.meta.activeTag(instruction),
        .index = index,
    };
}

/// Replaces the last instruction with a new instruction
/// The opcodes between the old and new instructions must match
fn replace(self: *Self, old: CompiledInstruction, new_instr: opcodes.Instruction) Error!void {
    const new_opcode = std.meta.activeTag(new_instr);
    if (old.opcode != new_opcode) {
        // std.debug.panic("Expected opcode to match but found {s} and {s}", .{ @tagName(old.opcode), @tagName(new_opcode) });
        return Error.OpcodeReplaceMismatch;
    }

    var stream = std.io.fixedBufferStream(self.instructions.items);
    const prev_index = stream.pos;
    stream.seekTo(old.index) catch return Error.OpcodeReplaceFailure;
    try encode(stream.writer(), new_instr);
    stream.seekTo(prev_index) catch return Error.OpcodeReplaceFailure;
}

/// Encodes an instruction into the given writer
fn encode(writer: anytype, instruction: opcodes.Instruction) Error!void {
    // size of the opcode + size of the instruction
    const op: Opcode = std.meta.activeTag(instruction);
    op.encode(writer) catch return Error.OpcodeEncodeFailure;

    switch (instruction) {
        inline else => |value| opcodes.encode(value, writer) catch {
            return Error.OpcodeEncodeFailure;
        },
    }
}

/// Returns the last instruction or panics if there is none
fn getLastInstruction(self: *Self) Error!CompiledInstruction {
    return self.last_compiled_instr orelse Error.InvalidInstruction;
}

/// Returns the previous instruction before the last instruction or panics if there is none
fn getPreviousInstruction(self: *Self) Error!CompiledInstruction {
    return self.penult_compiled_instr orelse Error.InvalidInstruction;
}

/// Compiles the program into bytecode
pub fn compile(self: *Self) !Bytecode {
    for (self.program.statements.items) |statement| {
        try self.compileStatement(statement);
    }

    return .{ .instructions = self.instructions.items, .constants = self.constants.items };
}

/// Compiles a statement into bytecode
fn compileStatement(self: *Self, statement: ast.Statement) Error!void {
    switch (statement) {
        .expression => |inner| {
            try self.compileExpression(inner.expression);
            // pop the result of the statement off the stack after it's done
            try self.addInstruction(.pop);
        },
        // .variable => |inner| {

        // },
        .block => |inner| {
            for (inner.statements) |stmt| {
                try self.compileStatement(stmt);
            }
        },
        inline else => return Error.UnsupportedStatement,
    }
}

/// Resolves a prefix operator its instruction equivalent
inline fn resolvePrefixToInstr(operator: ast.Operator) opcodes.Instruction {
    return switch (operator) {
        .minus => .neg,
        .not => .not,
        inline else => std.debug.panic("unexpected operator: {s}", .{operator}),
    };
}

/// Resolves an infix operator to its instruction equivalent
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
        .@"and" => .@"and",
        .@"or" => .@"or",
        inline else => std.debug.panic("unexpected infix operator: {s}", .{operator}),
    };
}

/// Attempts to add a constant to the list of constants and return its index
fn addConstant(self: *Self, value: Value) Error!u16 {
    self.constants.append(value) catch return Error.OutOfMemory;
    return @intCast(self.constants.items.len - 1);
}

/// Compiles an expression to opcodes
fn compileExpression(self: *Self, expression: ast.Expression) Error!void {
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
        .if_expr => |inner| {
            var jump_instr: CompiledInstruction = undefined;
            var jif_instr: CompiledInstruction = undefined;
            for (inner.condition_list) |condition_data| {
                // 1. compile condition
                // 2. check if condition is false
                try self.compileExpression(condition_data.condition.*);
                try self.addInstruction(.{ .jump_if_false = MaxOffset });
                jif_instr = try self.getLastInstruction();
                try self.compileIfBody(condition_data.body);
                try self.addInstruction(.{ .jump = MaxOffset });
                jump_instr = try self.getLastInstruction();
            }

            // 3. if false, jump to alternative block if it exists or end of block if it doesn't
            var jif_target: CompiledInstruction = try self.getLastInstruction();

            // compile the if body if it exists or a void instruction if it doesn't
            if (inner.alternative) |body| {
                try self.compileIfBody(body);
            } else {
                try self.addInstruction(.void);
            }

            const jump_target = try self.getLastInstruction();
            try self.replace(jump_instr, .{ .jump = @intCast(jump_target.nextInstructionIndex()) });
            try self.replace(jif_instr, .{ .jump_if_false = @intCast(jif_target.nextInstructionIndex()) });
        },
        .while_expr => |inner| {
            var jif_target: CompiledInstruction = undefined;
            try self.compileExpression(inner.condition.*);
            const loop_start_instr = try self.getLastInstruction();

            try self.addInstruction(.{ .jump_if_false = MaxOffset });
            jif_target = try self.getLastInstruction();
            for (inner.body.statements) |statement| {
                try self.compileStatement(statement);
            }
            try self.addInstruction(.{ .jump = @intCast(loop_start_instr.index) });

            const jump_target = try self.getLastInstruction();
            try self.replace(jif_target, .{ .jump_if_false = @intCast(jump_target.nextInstructionIndex()) });
        },
        .number => |value| {
            const index = try self.addConstant(.{ .number = value });
            try self.addInstruction(.{ .@"const" = index });
        },
        .string => |value| {
            const index = try self.addConstant(.{ .string = value });
            try self.addInstruction(.{ .@"const" = index });
        },
        .boolean => |inner| try self.addInstruction(if (inner) .true else .false),
        .null => try self.addInstruction(.null),
        inline else => std.debug.panic("unexpected expression type: {s}", .{expression}),
    }
}

/// Compiles an if body to opcodes
inline fn compileIfBody(self: *Self, body: ast.IfExpression.Body) Error!void {
    switch (body) {
        .block => |block| {
            for (block.statements) |statement| {
                try self.compileStatement(statement);
            }
            try self.addInstruction(.void);
        },
        .expression => |expr| try self.compileExpression(expr.*),
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

    const expected_bytes = opcodes.make(&[_]Instruction{
        .{ .@"const" = 0x00 },
        .{ .@"const" = 0x01 },
        .add,
        .pop,
    });
    try std.testing.expectEqualSlices(u8, expected_bytes, bytecode.instructions);
    try std.testing.expectEqualSlices(Value, &.{ .{ .number = 1 }, .{ .number = 2 } }, bytecode.constants);
}
