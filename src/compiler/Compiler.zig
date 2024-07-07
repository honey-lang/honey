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
/// The maximum number of local variables that can be used in a function
const MaxLocalVariables = std.math.maxInt(u8) + 1;

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
    /// Occurs when a variable already exists in either the current scope or a parent scope
    VariableAlreadyExists,
    /// Occurs when the compiler encounters an unexpected type
    UnexpectedType,
};

const Local = struct {
    /// The name of the local variable
    name: []const u8,
    /// The depth of the scope the local variable was declared in
    depth: u16,
};

/// A simple arena allocator used when compiling into bytecode
arena: std.heap.ArenaAllocator,
/// The current program being compiled
program: ast.Program,
/// The diagnostics used by the compiler
diagnostics: utils.Diagnostics,
/// The instructions being generated
instructions: std.ArrayList(u8),
/// A list of constant expressions used in the program
constants: std.ArrayList(Value),
/// The last instruction that was added
last_compiled_instr: ?CompiledInstruction = null,
/// The instruction before the last instruction
penult_compiled_instr: ?CompiledInstruction = null,
/// The local variables being tracked by the compiler
local_variables: [MaxLocalVariables]Local = undefined,
/// How many local variables are currently being used in the current scope
used_local_variables: u8 = 0,
/// The depth of the current scope
scope_depth: u16 = 0,

/// Initializes the compiler
pub fn init(ally: std.mem.Allocator, program: ast.Program) Self {
    return .{
        .arena = std.heap.ArenaAllocator.init(ally),
        .diagnostics = utils.Diagnostics.init(ally),
        .program = program,
        .instructions = std.ArrayList(u8).init(ally),
        .constants = std.ArrayList(Value).init(ally),
    };
}

/// Deinitializes the compiler
pub fn deinit(self: *Self) void {
    self.arena.deinit();
    self.diagnostics.deinit();
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
        self.diagnostics.report("Expected opcode to match but found {s} and {s}", .{ @tagName(old.opcode), @tagName(new_opcode) });
        return error.OpcodeReplaceMismatch;
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

/// Returns the last instruction or errors if there is none
fn getLastInstruction(self: *Self) Error!CompiledInstruction {
    return self.last_compiled_instr orelse Error.InvalidInstruction;
}

/// Returns the previous instruction before the last instruction or errors if there is none
fn getPreviousInstruction(self: *Self) Error!CompiledInstruction {
    return self.penult_compiled_instr orelse Error.InvalidInstruction;
}

/// Returns the local variables in the current scope
inline fn getLocals(self: *Self) []Local {
    return self.local_variables[0..self.used_local_variables];
}

/// Returns the last local variable
/// This will panic or have UB if there are no local variables
inline fn getLastLocal(self: *Self) Local {
    return self.getLocals()[self.used_local_variables - 1];
}

/// Returns true if there is a local variable with the given name
inline fn hasLocal(self: *Self, name: []const u8) bool {
    return self.findLocal(name) != null;
}

/// Finds a local variable by name and returns its index
inline fn findLocal(self: *Self, name: []const u8) ?u16 {
    for (self.getLocals(), 0..) |local, index| {
        if (std.mem.eql(u8, local.name, name)) {
            return @intCast(index);
        }
    }
    return null;
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
        .variable => |inner| {
            // declare a global variable if we're at the top level
            if (self.scope_depth <= 0) {
                const index = try self.addConstant(.{ .identifier = inner.name });
                try self.compileExpression(inner.expression);

                try self.addInstruction(.{ .declare_global = index });
                return;
            }

            // declare a local variable
            // if the variable name already exists in the current scope, throw an error
            for (self.getLocals()) |local| {
                if (local.depth <= self.scope_depth and std.mem.eql(u8, local.name, inner.name)) {
                    self.diagnostics.report("Variable {s} already exists in the current scope", .{inner.name});
                    return error.VariableAlreadyExists;
                }
            }

            _ = try self.addLocal(inner.name);
            try self.compileExpression(inner.expression);
        },
        .assignment => |inner| {
            try self.compileExpression(inner.expression);

            const instr: opcodes.Instruction = if (self.findLocal(inner.name)) |offset|
                .{ .set_local = offset }
            else global: {
                const index = try self.addConstant(.{ .identifier = inner.name });
                break :global .{ .set_global = index };
            };
            try self.addInstruction(instr);
        },
        .block => |inner| {

            // compile the block's statements & handle the scope depth
            {
                self.scope_depth += 1;
                defer self.scope_depth -= 1;
                for (inner.statements) |stmt| {
                    try self.compileStatement(stmt);
                }
            }

            // pop after the block is done
            while (self.used_local_variables > 0 and self.getLastLocal().depth > self.scope_depth) {
                try self.addInstruction(.pop);
                self.used_local_variables -= 1;
            }
        },
        inline else => return Error.UnsupportedStatement,
    }
}

/// Resolves a prefix operator its instruction equivalent
inline fn resolvePrefixToInstr(self: *Self, operator: ast.Operator) Error!opcodes.Instruction {
    return switch (operator) {
        .minus => .neg,
        .not => .not,
        inline else => {
            self.diagnostics.report("unexpected operator: {s}", .{operator});
            return error.InvalidInstruction;
        },
    };
}

/// Resolves an infix operator to its instruction equivalent
inline fn resolveInfixToInstr(self: *Self, operator: ast.Operator) Error!opcodes.Instruction {
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
        inline else => {
            self.diagnostics.report("unexpected infix operator: {s}", .{operator});
            return error.UnexpectedType;
        },
    };
}

/// Attempts to add a constant to the list of constants and return its index
fn addConstant(self: *Self, value: Value) Error!u16 {
    self.constants.append(value) catch return Error.OutOfMemory;
    return @intCast(self.constants.items.len - 1);
}

/// Attempts to add a local variable to the list of local variables and return its index
fn addLocal(self: *Self, name: []const u8) Error!u16 {
    defer self.used_local_variables += 1;
    self.local_variables[self.used_local_variables] = Local{ .name = name, .depth = self.scope_depth };
    return self.used_local_variables;
}

/// Compiles an expression to opcodes
fn compileExpression(self: *Self, expression: ast.Expression) Error!void {
    switch (expression) {
        .binary => |inner| {
            try self.compileExpression(inner.lhs.*);
            try self.compileExpression(inner.rhs.*);

            const instr = try self.resolveInfixToInstr(inner.operator);
            try self.addInstruction(instr);
        },
        .prefix => |inner| {
            try self.compileExpression(inner.rhs.*);

            const instr = try self.resolvePrefixToInstr(inner.operator);
            try self.addInstruction(instr);
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
            // the loop should start before the condition
            const loop_start_instr = try self.getLastInstruction();
            try self.compileExpression(inner.condition.*);

            try self.addInstruction(.{ .jump_if_false = MaxOffset });
            jif_target = try self.getLastInstruction();

            for (inner.body.statements) |statement| {
                try self.compileStatement(statement);
            }
            try self.addInstruction(.{ .jump = @intCast(loop_start_instr.nextInstructionIndex()) });
            const jump_target = try self.getLastInstruction();
            try self.replace(jif_target, .{ .jump_if_false = @intCast(jump_target.nextInstructionIndex()) });

            // todo: only add a void instr if the loop body is empty
            try self.addInstruction(.void);
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
        .identifier => |value| {
            if (self.findLocal(value)) |offset| {
                try self.addInstruction(.{ .get_local = offset });
            } else {
                const index = try self.addConstant(.{ .identifier = value });
                try self.addInstruction(.{ .get_global = index });
            }
        },
        .builtin => |inner| {
            const index = try self.addConstant(.{ .identifier = inner.name });
            for (inner.arguments) |arg| {
                try self.compileExpression(arg);
            }
            try self.addInstruction(.{ .call_builtin = .{ .constant_index = index, .arg_count = @as(u16, @intCast(inner.arguments.len)) } });
        },
        inline else => {
            self.diagnostics.report("Unsupported expression type: {s}", .{expression});
            return error.UnsupportedExpression;
        },
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

test "ensure compiler errors on existing variable" {
    const ally = std.testing.allocator;

    var program = ast.Program.init(ally);
    defer program.deinit();

    // declare a variable
    const variable_1 = ast.createVariableStatement(.let, "test", ast.Expression{ .number = 1 });
    // declare a variable with the same name
    const variable_2 = ast.createVariableStatement(.let, "test", ast.Expression{ .number = 2 });

    const statements = [_]ast.Statement{ variable_1, variable_2 };
    const block = ast.createBlockStatement(&statements);
    try program.add(block);

    var compiler = Self.init(ally, program);
    defer compiler.deinit();

    const err = compiler.compile();
    try std.testing.expectError(Error.VariableAlreadyExists, err);
}
