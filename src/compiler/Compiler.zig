const std = @import("std");
const utils = @import("../utils/utils.zig");
const token = @import("../lexer/token.zig");
const Token = token.Token;
const ast = @import("../parser/ast.zig");
const honey = @import("../honey.zig");
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

    pub fn endIndex(self: CompiledInstruction) usize {
        return self.index + self.opcode.width();
    }

    /// Returns the index of the (potential) next instruction given its index and its size
    pub fn nextInstructionIndex(self: CompiledInstruction) usize {
        return self.index + self.opcode.size();
    }
};

/// The maximum offset that can be used in a jump instruction. Used as a placeholder until replaced with the actual offset
const MaxOffset = std.math.maxInt(u16);
/// The maximum number of local variables that can be used in a function
const MaxLocalVariables = std.math.maxInt(u8) + 1;
/// The map type used to store global identifiers declared in the program
const GlobalIdentifierMap = std.StringArrayHashMap(void);

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
    /// Occurs when a user tries to access a local variable that is out of bounds
    LocalOutOfBounds,
    /// Occurs when a user tries to declare a local variable that already exists in scope
    LocalAlreadyExists,
    /// Occurs when a variable already exists in either the current scope or a parent scope
    VariableAlreadyExists,
    /// Occurs when the compiler encounters an unexpected type
    UnexpectedType,
    /// Occurs when the compiler attempts to exit a function that it is not in
    NotInFunction,
};

const Local = struct {
    /// The name of the local variable
    name: []const u8,
    /// The depth of the scope the local variable was declared in
    depth: u16,
    /// Whether the local variable is a constant
    is_const: bool,
};

const ScopeContext = struct {
    const LocalArray = std.BoundedArray(Local, MaxLocalVariables);

    // The number of temporary variables being tracked by the compiler
    temp_variable_count: u16 = 0,
    /// The local variables being tracked by the compiler
    local_variables: LocalArray = .{},
    /// The depth of the current scope
    current_depth: u16 = 0,
    /// The current loop that the compiler is in
    current_loop: ?ast.Expression = null,
    /// The current break instructions being tracked for the current loop
    break_statements: std.ArrayList(CompiledInstruction),
    /// The current continue instructions being tracked for the current loop
    continue_statements: std.ArrayList(CompiledInstruction),

    /// Initializes the scope context
    pub fn init(ally: std.mem.Allocator) ScopeContext {
        return .{
            .break_statements = std.ArrayList(CompiledInstruction).init(ally),
            .continue_statements = std.ArrayList(CompiledInstruction).init(ally),
        };
    }

    /// Deinitializes the scope context
    pub fn deinit(self: *ScopeContext) void {
        self.break_statements.deinit();
        self.continue_statements.deinit();
    }

    /// Begins a new loop
    pub fn beginLoop(self: *ScopeContext, loop: ast.Expression) void {
        self.break_statements.clearRetainingCapacity();
        self.continue_statements.clearRetainingCapacity();
        self.current_loop = loop;
    }

    /// Adds a continue statement to the current loop
    pub fn addBreak(self: *ScopeContext, instr: CompiledInstruction) !void {
        try self.break_statements.append(instr);
    }

    /// Adds a continue statement to the current loop
    pub fn addContinue(self: *ScopeContext, instr: CompiledInstruction) !void {
        try self.continue_statements.append(instr);
    }

    /// Patches all break and continue statements with the correct offsets
    pub fn patchLoop(self: *ScopeContext, compiler: *Self, post_loop_expr: CompiledInstruction, loop_end: CompiledInstruction) !void {
        for (self.break_statements.items) |instr| {
            try compiler.replace(instr, .{ .jump = @intCast(loop_end.endIndex() - instr.index) });
        }

        for (self.continue_statements.items) |instr| {
            try compiler.replace(instr, .{ .jump = @intCast(post_loop_expr.endIndex() - instr.nextInstructionIndex()) });
        }
    }

    /// Ends the current loop
    pub fn endLoop(self: *ScopeContext) void {
        self.break_statements.clearRetainingCapacity();
        self.continue_statements.clearRetainingCapacity();
        self.current_loop = null;
    }

    // Allocates a temp variable slot by incrementing the count
    inline fn allocateTemp(self: *ScopeContext) u16 {
        defer self.temp_variable_count += 1;
        return self.temp_variable_count;
    }

    // Deallocates a temp variable slot by decrementing the count
    inline fn deallocateTemp(self: *ScopeContext) void {
        self.temp_variable_count -= 1;
    }

    /// Returns the local variables in the current scope
    inline fn getLocals(self: *ScopeContext) []Local {
        return self.local_variables.slice();
    }

    /// Returns the number of local variables in the current scope
    inline fn getLocalsCount(self: *ScopeContext) usize {
        return self.local_variables.len;
    }

    /// Returns the last local variable in the current scope or null if there are no local variables
    inline fn popLocal(self: *ScopeContext) ?Local {
        return self.local_variables.popOrNull();
    }

    /// Returns a local variable by offset or errors if the index is out of bounds
    inline fn getLocal(self: *ScopeContext, offset: u16) Error!Local {
        return self.local_variables.get(@intCast(offset));
    }

    /// Returns the last local variable
    /// This will panic or have UB if there are no local variables
    inline fn getLastLocal(self: *ScopeContext) Local {
        return self.local_variables.get(self.local_variables.len - 1);
    }

    /// Returns true if there is a local variable with the given name
    inline fn hasLocal(self: *ScopeContext, name: []const u8) bool {
        return self.resolveLocalOffset(name) != null;
    }

    /// Finds a local variable by name and returns its offset
    inline fn resolveLocalOffset(self: *ScopeContext, name: []const u8) ?u16 {
        for (self.getLocals(), 0..) |local, offset| {
            if (std.mem.eql(u8, local.name, name)) {
                return @intCast(offset);
            }
        }
        return null;
    }

    /// Finds a local variable by name and returns it
    inline fn resolveLocal(self: *ScopeContext, name: []const u8) ?Local {
        for (self.getLocals()) |local| {
            if (std.mem.eql(u8, local.name, name)) {
                return local;
            }
        }
        return null;
    }

    /// Attempts to add a local variable to the list of local variables and return its index
    fn addLocal(self: *ScopeContext, name: []const u8, is_const: bool) Error!u16 {
        self.local_variables.append(Local{
            .name = name,
            .depth = self.current_depth,
            .is_const = is_const,
        }) catch return Error.OutOfMemory;
        return self.local_variables.len - 1;
    }

    /// Attempts to find a local variable by name. If found, it removes it from the list of local variables
    fn removeLocal(self: *ScopeContext, name: []const u8) Error!void {
        for (self.getLocals()) |local| {
            if (std.mem.eql(u8, local.name, name)) {
                _ = self.local_variables.popOrNull() orelse return Error.LocalOutOfBounds;
                return;
            }
        }
    }
};

/// A raw instruction list type
const InstructionList = std.ArrayList(u8);
pub const Function = struct {
    name: []const u8,
    param_count: u8,
    instructions: InstructionList,

    pub fn init(name: []const u8, param_count: u8, ally: std.mem.Allocator) Function {
        return .{
            .name = name,
            .param_count = param_count,
            .instructions = InstructionList.init(ally),
        };
    }

    pub fn deinit(self: *Function) void {
        self.instructions.deinit();
    }
};

/// A simple arena allocator used when compiling into bytecode
arena: std.heap.ArenaAllocator,
/// The current program being compiled
program: ast.Program,
/// The diagnostics used by the compiler
diagnostics: utils.Diagnostics,
/// The instructions being generated
instructions: InstructionList,
/// A list of constant expressions used in the program
constants: std.ArrayList(Value),
/// A list of global identifiers declared in the program
declared_global_identifiers: GlobalIdentifierMap,
/// The last instruction that was added
last_compiled_instr: ?CompiledInstruction = null,
/// The instruction before the last instruction
penult_compiled_instr: ?CompiledInstruction = null,
/// The current function to emit the bytecode into
current_func: ?Function = null,
/// The list of functions that have been declared in the program
declared_funcs: std.ArrayList(Function),
/// Context about the current scope
scope_context: ScopeContext,
/// The writer used for error reporting
error_writer: std.io.AnyWriter,

/// Initializes the compiler
pub fn init(ally: std.mem.Allocator, program: ast.Program, error_writer: std.io.AnyWriter) Self {
    return .{
        .arena = std.heap.ArenaAllocator.init(ally),
        .diagnostics = utils.Diagnostics.init(ally),
        .program = program,
        .instructions = InstructionList.init(ally),
        .constants = std.ArrayList(Value).init(ally),
        .declared_global_identifiers = GlobalIdentifierMap.init(ally),
        .declared_funcs = std.ArrayList(Function).init(ally),
        .scope_context = ScopeContext.init(ally),
        .error_writer = error_writer,
    };
}

/// Deinitializes the compiler
pub fn deinit(self: *Self) void {
    self.arena.deinit();
    self.diagnostics.deinit();
    self.instructions.deinit();
    self.constants.deinit();
    self.declared_global_identifiers.deinit();
    self.declared_funcs.deinit();
    self.scope_context.deinit();
}

fn enterFunction(self: *Self, name: []const u8, param_count: u8) void {
    self.current_func = Function.init(name, param_count, self.arena.allocator());
}

fn exitFunction(self: *Self) Error!void {
    if (self.current_func) |func| {
        self.declared_funcs.append(func) catch return Error.OutOfMemory;
        self.current_func = null;
        return;
    }
    return Error.NotInFunction;
}

/// Cleans the current scope by adding `pop` instructions and removing them from our internal context
fn cleanScope(self: *Self) Error!void {
    while (self.scope_context.getLocalsCount() > 0 and self.scope_context.getLastLocal().depth > self.scope_context.current_depth) {
        try self.addInstruction(.pop);
        _ = self.scope_context.popLocal();
    }
}

/// Adds an operation to the bytecode
fn addInstruction(self: *Self, instruction: opcodes.Instruction) Error!void {
    // if we're in a function, use our func instructions writer. otherwise, write to global program
    const writer: InstructionList.Writer = if (self.current_func) |*current|
        current.instructions.writer()
    else
        self.instructions.writer();

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
        self.reportError("Expected opcode to match but found {s} and {s}", .{ @tagName(old.opcode), @tagName(new_opcode) });
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

/// Returns true if there is a global variable/constant declared with the given name
fn hasGlobal(self: *Self, name: []const u8) bool {
    return self.declared_global_identifiers.contains(name);
}

/// Marks a global variable/constant as declared with the given name
fn markGlobal(self: *Self, name: []const u8) !void {
    try self.declared_global_identifiers.put(name, {});
}

const ReportedToken = token.TokenData{
    .token = .{ .invalid = '\x00' },
    .position = .{ .start = 0, .end = 0 },
};

pub fn reportError(self: *Self, comptime fmt: []const u8, args: anytype) void {
    self.diagnostics.report(fmt, args, ReportedToken);
}

pub fn report(self: *Self) void {
    if (!self.diagnostics.hasErrors()) {
        return;
    }
    const msg_data = self.diagnostics.errors.items(.msg);
    for (msg_data) |msg| {
        self.error_writer.print("error: {s}\n", .{msg}) catch unreachable;
    }
}

/// Compiles the program into bytecode
pub fn compile(self: *Self) !Bytecode {
    for (self.program.statements.items) |statement| {
        try self.compileStatement(statement);
        // std.log.info("Statement: {s}", .{statement});
    }
    // add return from body
    try self.addInstruction(.@"return");

    var func_map = std.StringArrayHashMap(usize).init(self.arena.allocator());
    // append all of our declared functions after the body of the main program
    for (self.declared_funcs.items) |*func| {
        // map our function names to their offset in the program
        try func_map.put(func.*.name, self.instructions.items.len);
        try self.instructions.appendSlice(func.*.instructions.items);
        func.deinit();
    }

    return .{
        .instructions = self.instructions.items,
        .constants = self.constants.items,
        .funcs = func_map,
    };
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
            if (self.scope_context.current_depth <= 0) {
                if (self.hasGlobal(inner.name)) {
                    self.reportError("Variable '{s}' already exists in the current scope", .{inner.name});
                    return Error.VariableAlreadyExists;
                }

                const index = try self.addConstant(.{ .identifier = inner.name });
                // compile the expression if it exists
                if (inner.expression) |expr| {
                    try self.compileExpression(expr);
                }

                try self.markGlobal(inner.name);
                try self.addInstruction(if (inner.kind == .@"const") .{ .declare_const = index } else .{ .declare_var = index });
                return;
            }

            // declare a local variable
            // if the variable name already exists in the current scope, throw an error
            for (self.scope_context.getLocals()) |local| {
                if (local.depth <= self.scope_context.current_depth and std.mem.eql(u8, local.name, inner.name)) {
                    self.reportError("Variable {s} already exists in the current scope", .{inner.name});
                    return Error.VariableAlreadyExists;
                }
            }

            _ = try self.scope_context.addLocal(inner.name, inner.kind == .@"const");
            if (inner.expression) |expr| {
                try self.compileExpression(expr);
            }
        },
        .assignment => |inner| switch (inner.lhs) {
            .identifier => |name| {
                if (self.scope_context.resolveLocalOffset(name)) |offset| {
                    // fetch local
                    const local = self.scope_context.getLocal(offset) catch |err| {
                        self.reportError("Local variable out of bounds: {s}", .{name});
                        return err;
                    };

                    // if local is a constant, error out
                    if (local.is_const) {
                        self.reportError("Cannot reassign constant variable: {s}", .{name});
                        return Error.VariableAlreadyExists;
                    }

                    if (!inner.isSimple()) {
                        try self.addInstruction(.{ .get_local = offset });
                        try self.compileExpression(inner.rhs);

                        const instr = try self.resolveAssignToInstr(inner.type);
                        try self.addInstruction(instr);
                    } else {
                        try self.compileExpression(inner.rhs);
                    }

                    try self.addInstruction(.{ .set_local = offset });
                } else {
                    const index = try self.addConstant(.{ .identifier = name });
                    if (!inner.isSimple()) {
                        try self.addInstruction(.{ .get_global = index });
                        try self.compileExpression(inner.rhs);

                        const instr = try self.resolveAssignToInstr(inner.type);
                        try self.addInstruction(instr);
                    } else {
                        try self.compileExpression(inner.rhs);
                    }

                    try self.addInstruction(.{ .set_global = index });
                }
            },
            .index => |index_expr| {
                if (index_expr.lhs.* != .identifier) {
                    self.reportError("Expected identifier for index assignment but got: {s}", .{index_expr.lhs});
                    return Error.UnexpectedType;
                }
                const index_identifier = index_expr.lhs.identifier;
                // fetch list, fetch index, update at index with new expression
                if (self.scope_context.resolveLocalOffset(index_identifier)) |offset| {
                    // get list
                    try self.addInstruction(.{ .get_local = offset });
                    // compile index & fetch
                    try self.compileExpression(index_expr.index.*);

                    try self.compileExpression(inner.rhs);
                    try self.addInstruction(.set_index);
                    try self.addInstruction(.{ .set_local = offset });
                } else {
                    const index = try self.addConstant(.{ .identifier = index_identifier });
                    try self.addInstruction(.{ .get_global = index });
                    // compile index & fetch
                    try self.compileExpression(index_expr.index.*);

                    try self.compileExpression(inner.rhs);
                    try self.addInstruction(.set_index);
                    try self.addInstruction(.{ .set_global = index });
                }
            },
            .member => |member| {
                try self.compileExpression(member.lhs.*);
                const member_index = try self.addConstant(.{ .string = member.member });
                try self.addInstruction(.{ .@"const" = member_index });
                try self.compileExpression(inner.rhs);
                try self.addInstruction(.set_member);
            },
            inline else => unreachable,
        },
        .block => |inner| {
            // compile the block's statements & handle the scope depth
            self.scope_context.current_depth += 1;
            for (inner.statements) |stmt| {
                try self.compileStatement(stmt);
            }
            self.scope_context.current_depth -= 1;

            // pop after the block is done
            while (self.scope_context.getLocalsCount() > 0 and self.scope_context.getLastLocal().depth > self.scope_context.current_depth) {
                try self.addInstruction(.pop);
                _ = self.scope_context.popLocal();
            }
        },
        .@"fn" => |inner| {
            var found_return = false;
            self.enterFunction(inner.name, @intCast(inner.parameters.len));
            // compile the block's statements & handle the scope depth
            self.scope_context.current_depth += 1;
            // Declare parameters as local values
            for (inner.parameters) |parameter| {
                if (self.scope_context.hasLocal(parameter.identifier)) {
                    self.reportError("Duplicate parameter name `{s}` in function `{s}`", .{ parameter.identifier, inner.name });
                    return error.LocalAlreadyExists;
                }
                _ = try self.scope_context.addLocal(parameter.identifier, true);
            }

            const fn_decl: ast.FunctionDeclaration = inner;
            for (fn_decl.body.statements) |stmt| {
                // if we find a return, we will clean up
                if (stmt == .@"return") {
                    self.scope_context.current_depth -= 1;
                    try self.compileStatement(stmt);
                    found_return = true;
                    break;
                }
                try self.compileStatement(stmt);
            }

            // if the function didn't have a return, we will clean our scope and add the instr
            if (!found_return) {
                self.scope_context.current_depth -= 1;
                try self.cleanScope();
                try self.addInstruction(.@"return");
            }

            try self.exitFunction();
        },
        .@"return" => |inner| {
            // if we have a return expression, compile it
            if (inner.expression) |expr| {
                try self.compileExpression(expr);
            } else {
                try self.addInstruction(.null);
            }

            // if we're in a current function, clean it up
            if (self.current_func != null) {
                const slot = self.scope_context.allocateTemp();
                try self.addInstruction(.{ .set_temp = slot });
                try self.cleanScope();
                try self.addInstruction(.{ .get_temp = slot });
                self.scope_context.deallocateTemp();
            }
            try self.addInstruction(.@"return");
        },
        .@"break" => {
            if (self.scope_context.current_loop) |_| {
                try self.addInstruction(.{ .jump = MaxOffset });
                try self.scope_context.addBreak(try self.getLastInstruction());
            } else {
                self.reportError("break statement outside of loop", .{});
                return Error.UnsupportedStatement;
            }
        },
        .@"continue" => {
            if (self.scope_context.current_loop) |_| {
                try self.addInstruction(.{ .jump = MaxOffset });
                try self.scope_context.addContinue(try self.getLastInstruction());
            } else {
                self.reportError("continue statement outside of loop", .{});
                return Error.UnsupportedStatement;
            }
        },
    }
}

inline fn resolveAssignToInstr(self: *Self, token_value: Token) Error!opcodes.Instruction {
    return switch (token_value) {
        .plus_assignment => .add,
        .minus_assignment => .sub,
        .star_assignment => .mul,
        .slash_assignment => .div,
        .modulo_assignment => .mod,
        .doublestar_assignment => .pow,
        inline else => {
            self.reportError("Unsupported assignment type: {s}", .{token_value});
            return error.UnsupportedStatement;
        },
    };
}

/// Resolves a prefix operator its instruction equivalent
inline fn resolvePrefixToInstr(self: *Self, operator: ast.Operator) Error!opcodes.Instruction {
    return switch (operator) {
        .minus => .neg,
        .not => .not,
        inline else => {
            self.reportError("unexpected operator: {s}", .{operator});
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
            self.reportError("unexpected infix operator: {s}", .{operator});
            return error.UnexpectedType;
        },
    };
}

/// Attempts to add a constant to the list of constants and return its index
fn addConstant(self: *Self, value: Value) Error!u16 {
    // if it already exists, return the index
    for (self.constants.items, 0..) |current, index| {
        if (current.equal(value)) {
            return @intCast(index);
        }
    }
    self.constants.append(value) catch return Error.OutOfMemory;
    return @intCast(self.constants.items.len - 1);
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

            // if the condition is false, jump to the else block
            const condition_data = inner.condition_list[0];
            try self.compileExpression(condition_data.condition.*);
            try self.addInstruction(.{ .jump_if_false = MaxOffset });
            jif_instr = try self.getLastInstruction();
            const jif_offset_start = self.instructions.items.len;

            // compile body & add jump after the body
            try self.compileIfBody(condition_data.body);
            try self.addInstruction(.{ .jump = MaxOffset });
            const jfwd_offset_start = self.instructions.items.len;
            jump_instr = try self.getLastInstruction();

            const jif_offset_end = self.instructions.items.len;
            // if there's an else block, compile it. otherwise, just add a void instruction
            if (inner.alternative) |alternative| {
                try self.compileIfBody(alternative);
            } else {
                try self.addInstruction(.void);
            }
            const jfwd_offset_end = self.instructions.items.len;

            try self.replace(jif_instr, .{ .jump_if_false = @intCast(jif_offset_end - jif_offset_start) });
            try self.replace(jump_instr, .{ .jump = @intCast(jfwd_offset_end - jfwd_offset_start) });
        },
        .while_expr => |inner| {
            var jif_target: CompiledInstruction = undefined;
            // the loop should start before the condition
            const loop_start_instr = try self.getLastInstruction();

            self.scope_context.beginLoop(expression);
            defer self.scope_context.endLoop();

            try self.compileExpression(inner.condition.*);

            try self.addInstruction(.{ .jump_if_false = MaxOffset });
            jif_target = try self.getLastInstruction();

            try self.compileStatement(inner.body.*);

            const post_loop_expr = try self.getLastInstruction();

            // if there's a post statement, compile it after the loop body
            if (inner.post_stmt) |post_stmt| {
                try self.compileStatement(post_stmt.*);
            }

            const last_loop_instr = try self.getLastInstruction();

            const loop_start_offset: u16 = @intCast(last_loop_instr.nextInstructionIndex() - loop_start_instr.index);
            try self.addInstruction(.{ .loop = @intCast(loop_start_offset) });
            const end_loop_instr = try self.getLastInstruction();

            const end_loop_offset: u16 = @intCast(end_loop_instr.index - jif_target.index);

            try self.replace(jif_target, .{ .jump_if_false = end_loop_offset });
            try self.scope_context.patchLoop(self, post_loop_expr, end_loop_instr);

            // todo: only add a void instr if the loop body is empty
            try self.addInstruction(.void);
        },
        .for_expr => |inner| {
            // self.diagnostics.report("expression {s} is not iterable", .{inner.expr});
            // return Error.UnsupportedStatement;
            // compile the expression and iterate over it
            var jif_target: CompiledInstruction = undefined;

            // the loop should start before the condition
            try self.compileExpression(inner.expr.*);

            // todo: multi-captures
            // how should we handle multi-captures with dictionaries? (e.g., for (dict_1, dict_2) {})
            const capture_value_name = inner.captures[0].identifier;
            // const capture_key_name: ?[]const u8 = if (inner.captures.len > 1) inner.captures[1].identifier else null;

            const capture_value_offset = try self.scope_context.addLocal(capture_value_name, false);
            // const capture_key_offset: ?u16 = if (capture_key_name) |name| try self.scope_context.addLocal(
            //     name,
            //     false,
            // ) else null;

            self.scope_context.beginLoop(expression);
            defer self.scope_context.endLoop();

            try self.addInstruction(.iterable_begin);

            const loop_start_instr = try self.getLastInstruction();

            try self.addInstruction(.iterable_has_next);
            try self.addInstruction(.{ .jump_if_false = MaxOffset });
            jif_target = try self.getLastInstruction();

            try self.addInstruction(.iterable_value);
            try self.addInstruction(.{ .set_local = capture_value_offset });

            // if (capture_key_offset) |offset| {
            //     try self.addInstruction(.iterable_key);
            //     try self.addInstruction(.{ .set_local = offset });
            // }

            try self.compileStatement(inner.body.*);

            const post_loop_expr = try self.getLastInstruction();

            try self.addInstruction(.iterable_next);

            try self.addInstruction(.{ .loop = MaxOffset });

            const loop_target = try self.getLastInstruction();

            // remove locals after loop is done
            try self.scope_context.removeLocal(capture_value_name);
            // if (capture_key_name) |name| try self.scope_context.removeLocal(name);
            try self.addInstruction(.iterable_end);
            // todo: only add a void instr if the loop body is empty
            try self.addInstruction(.void);

            try self.scope_context.patchLoop(self, post_loop_expr, loop_target);
            try self.replace(jif_target, .{ .jump_if_false = @intCast(loop_target.index - jif_target.index) });
            try self.replace(loop_target, .{ .loop = @intCast(loop_target.nextInstructionIndex() - loop_start_instr.nextInstructionIndex()) });
        },
        .index => |value| {
            try self.compileExpression(value.lhs.*);
            try self.compileExpression(value.index.*);
            try self.addInstruction(.get_index);
        },
        .member => |value| {
            try self.compileExpression(value.lhs.*);

            // compile identifier to string to prevent VM from trying to resolve it
            // todo: we should clean up the indexing code
            const index = try self.addConstant(.{ .string = value.member });
            try self.addInstruction(.{ .@"const" = index });

            try self.addInstruction(.get_member);
        },
        .number => |value| {
            const index = try self.addConstant(.{ .number = value });
            try self.addInstruction(.{ .@"const" = index });
        },
        .string => |value| {
            const index = try self.addConstant(.{ .string = value });
            try self.addInstruction(.{ .@"const" = index });
        },
        .list => |value| {
            var index: usize = value.expressions.len;
            while (index > 0) {
                index -= 1;
                try self.compileExpression(value.expressions[index]);
            }
            try self.addInstruction(.{ .list = @intCast(value.expressions.len) });
        },
        .dict => |dict| {
            // compile the dictionary in reverse order so we can pop in the correct order
            var index: usize = dict.keys.len;
            while (index > 0) {
                // we decrement before accessing to avoid underflows
                index -= 1;

                // fetch key and declare it
                const key = dict.keys[index];
                const key_index = try self.addConstant(.{ .string = if (key == .identifier) key.identifier else key.string });
                try self.addInstruction(.{ .@"const" = key_index });

                // compile value after key
                const value = dict.values[index];
                try self.compileExpression(value);
            }
            try self.addInstruction(.{ .dict = @intCast(dict.keys.len) });
        },
        .range => |value| {
            try self.compileExpression(value.start.*);
            try self.compileExpression(value.end.*);
            try self.addInstruction(.{ .range = value.inclusive });
        },
        .boolean => |value| try self.addInstruction(if (value) .true else .false),
        .null => try self.addInstruction(.null),
        .identifier => |value| {
            if (self.scope_context.resolveLocalOffset(value)) |offset| {
                try self.addInstruction(.{ .get_local = offset });
            } else {
                const index = try self.addConstant(.{ .identifier = value });
                try self.addInstruction(.{ .get_global = index });
            }
        },
        .call => |inner| {
            const index = try self.addConstant(.{ .identifier = inner.name });
            for (inner.arguments) |arg| {
                try self.compileExpression(arg);
            }
            try self.addInstruction(.{ .call_func = .{
                .constant_index = index,
                .arg_count = @as(u16, @intCast(inner.arguments.len)),
            } });
        },
        .builtin => |inner| {
            const index = try self.addConstant(.{ .identifier = inner.name });
            for (inner.arguments) |arg| {
                try self.compileExpression(arg);
            }
            try self.addInstruction(.{ .call_builtin = .{
                .constant_index = index,
                .arg_count = @as(u16, @intCast(inner.arguments.len)),
            } });
        },
        inline else => {
            self.reportError("Unsupported expression type: {s}", .{expression});
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

/// Helper function that compiles a source and tests it against a provided function
inline fn compileAndTest(source: []const u8, test_fn: *const fn (bytecode: Bytecode) anyerror!void) !void {
    const ally = std.testing.allocator;

    var program = ast.Program.init(ally);
    defer program.deinit();

    const result = try honey.parse(.{ .string = source }, .{
        .allocator = ally,
        .error_writer = std.io.getStdErr().writer().any(),
    });
    defer result.deinit();

    var compiler = Self.init(ally, result.data);
    defer compiler.deinit();

    const bytecode = try compiler.compile();
    try test_fn(bytecode);
}

/// Helper function that compiles a source and tests that it errors via a provided function
inline fn compileAndTestError(source: []const u8, test_fn: *const fn (bytecode_union: anytype) anyerror!void) !void {
    const ally = std.testing.allocator;

    var program = ast.Program.init(ally);
    defer program.deinit();

    const result = try honey.parse(.{ .string = source }, .{
        .allocator = ally,
        .error_writer = std.io.getStdErr().writer().any(),
    });
    defer result.deinit();

    var compiler = Self.init(ally, result.data);
    defer compiler.deinit();

    const err = compiler.compile();
    try test_fn(err);
}

test "test simple addition compilation" {
    try compileAndTest("1 + 2", struct {
        fn run(bytecode: Bytecode) anyerror!void {
            const expected_bytes = opcodes.make(&[_]Instruction{
                .{ .@"const" = 0x00 },
                .{ .@"const" = 0x01 },
                .add,
                .pop,
            });
            try std.testing.expectEqualSlices(u8, expected_bytes, bytecode.instructions);
            try std.testing.expectEqualSlices(Value, &.{ .{ .number = 1 }, .{ .number = 2 } }, bytecode.constants);
        }
    }.run);
}

test "ensure compiler errors on existing variable" {
    const source =
        \\{
        \\  let test = 1;
        \\  let test = 2;
        \\}
    ;
    try compileAndTestError(source, struct {
        fn run(bytecode_union: anytype) anyerror!void {
            try std.testing.expectError(Error.VariableAlreadyExists, bytecode_union);
        }
    }.run);
}

test "ensure compiler errors on reassignment of const variable" {
    const source =
        \\{
        \\  const test = 1;
        \\  test = 2;
        \\}
    ;
    try compileAndTestError(source, struct {
        fn run(bytecode_union: anytype) anyerror!void {
            try std.testing.expectError(Error.VariableAlreadyExists, bytecode_union);
        }
    }.run);
}

test "test simple list compilation" {
    try compileAndTest("[1, 2, 3]", struct {
        fn run(bytecode: Bytecode) anyerror!void {
            // lists are compiled in reverse order so they can be popped and appended correctly
            const expected_bytes = opcodes.make(&[_]Instruction{
                .{ .@"const" = 0x00 },
                .{ .@"const" = 0x01 },
                .{ .@"const" = 0x02 },
                .{ .list = 3 },
                .pop,
            });
            try std.testing.expectEqualSlices(u8, expected_bytes, bytecode.instructions);
            try std.testing.expectEqualSlices(Value, &.{ .{ .number = 3 }, .{ .number = 2 }, .{ .number = 1 } }, bytecode.constants);
        }
    }.run);
}

test "test simple list access compilation" {
    const source =
        \\{
        \\  const test = [1, 2, 3];
        \\  const value = test[1];
        \\}
    ;
    try compileAndTest(source, struct {
        fn run(bytecode: Bytecode) anyerror!void {
            const expected_bytes = opcodes.make(&[_]Instruction{
                // const test = [1, 2, 3];
                .{ .@"const" = 0x00 },
                .{ .@"const" = 0x01 },
                .{ .@"const" = 0x02 },
                .{ .list = 3 },
                // const value = test[1];
                .{ .get_local = 0 },
                .{ .@"const" = 0x02 },
                .get_index,
                .pop,
                .pop,
            });
            try std.testing.expectEqualSlices(u8, expected_bytes, bytecode.instructions);
            try std.testing.expectEqualSlices(Value, &.{ .{ .number = 3 }, .{ .number = 2 }, .{ .number = 1 } }, bytecode.constants);
        }
    }.run);
}
