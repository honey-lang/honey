const std = @import("std");

const ast = @import("../parser/ast.zig");
const Bytecode = @import("Bytecode.zig");
const Compiler = @import("Compiler.zig");
const Value = @import("value.zig").Value;

pub const Local = struct {
    /// The name of the local variable
    name: []const u8,
    /// The depth of the scope the local variable was declared in
    depth: u16,
    /// Whether the local variable is a constant
    is_const: bool,
};

pub const Context = struct {
    const LocalArray = std.BoundedArray(Local, Compiler.MaxLocalVariables);

    /// The local variables being tracked by the compiler
    local_variables: LocalArray = .{},
    /// The depth of the current scope
    current_depth: u16 = 0,
    /// The current loop that the compiler is in
    current_loop: ?ast.Expression = null,
    /// The current break instructions being tracked for the current loop
    break_statements: std.ArrayList(Compiler.CompiledInstruction),
    /// The current continue instructions being tracked for the current loop
    continue_statements: std.ArrayList(Compiler.CompiledInstruction),

    /// Initializes the scope context
    pub fn init(ally: std.mem.Allocator) Context {
        return .{
            .break_statements = std.ArrayList(Compiler.CompiledInstruction).init(ally),
            .continue_statements = std.ArrayList(Compiler.CompiledInstruction).init(ally),
        };
    }

    /// Deinitializes the scope context
    pub fn deinit(self: *Context) void {
        self.break_statements.deinit();
        self.continue_statements.deinit();
    }

    /// Begins a new loop
    pub fn beginLoop(self: *Context, loop: ast.Expression) void {
        self.break_statements.clearRetainingCapacity();
        self.continue_statements.clearRetainingCapacity();
        self.current_loop = loop;
    }

    /// Adds a continue statement to the current loop
    pub fn addBreak(self: *Context, instr: Compiler.CompiledInstruction) !void {
        try self.break_statements.append(instr);
    }

    /// Adds a continue statement to the current loop
    pub fn addContinue(self: *Context, instr: Compiler.CompiledInstruction) !void {
        try self.continue_statements.append(instr);
    }

    /// Patches all break and continue statements with the correct offsets
    pub fn patchLoop(self: *Context, compiler: *Compiler, post_loop_expr: Compiler.CompiledInstruction, loop_end: Compiler.CompiledInstruction) !void {
        for (self.break_statements.items) |instr| {
            try compiler.replace(instr, .{ .jump = @intCast(loop_end.nextInstructionIndex() - instr.index) });
        }

        for (self.continue_statements.items) |instr| {
            try compiler.replace(instr, .{ .jump = @intCast(post_loop_expr.nextInstructionIndex() - instr.nextInstructionIndex()) });
        }
    }

    /// Ends the current loop
    pub fn endLoop(self: *Context) void {
        self.break_statements.clearRetainingCapacity();
        self.continue_statements.clearRetainingCapacity();
        self.current_loop = null;
    }

    /// Returns the local variables in the current scope
    pub inline fn getLocals(self: *Context) []Local {
        return self.local_variables.slice();
    }

    /// Returns the number of local variables in the current scope
    pub inline fn getLocalsCount(self: *Context) usize {
        return self.local_variables.len;
    }

    /// Returns the last local variable in the current scope or null if there are no local variables
    pub inline fn popLocal(self: *Context) ?Local {
        return self.local_variables.popOrNull();
    }

    /// Returns a local variable by offset or errors if the index is out of bounds
    pub inline fn getLocal(self: *Context, offset: u16) Compiler.Error!Local {
        return self.local_variables.get(@intCast(offset));
    }

    /// Returns the last local variable
    /// This will panic or have UB if there are no local variables
    pub inline fn getLastLocal(self: *Context) Local {
        return self.local_variables.get(self.local_variables.len - 1);
    }

    /// Returns true if there is a local variable with the given name
    pub inline fn hasLocal(self: *Context, name: []const u8) bool {
        return self.resolveLocalOffset(name) != null;
    }

    /// Finds a local variable by name and returns its offset
    pub inline fn resolveLocalOffset(self: *Context, name: []const u8) ?u16 {
        for (self.getLocals(), 0..) |local, offset| {
            if (std.mem.eql(u8, local.name, name)) {
                return @intCast(offset);
            }
        }
        return null;
    }

    /// Finds a local variable by name and returns it
    pub inline fn resolveLocal(self: *Context, name: []const u8) ?Local {
        for (self.getLocals()) |local| {
            if (std.mem.eql(u8, local.name, name)) {
                return local;
            }
        }
        return null;
    }

    /// Attempts to add a local variable to the list of local variables and return its index
    pub fn addLocal(self: *Context, name: []const u8, is_const: bool) Compiler.Error!u16 {
        self.local_variables.append(Local{
            .name = name,
            .depth = self.current_depth,
            .is_const = is_const,
        }) catch return Compiler.Error.OutOfMemory;
        return self.local_variables.len - 1;
    }

    /// Attempts to find a local variable by name. If found, it removes it from the list of local variables
    pub fn removeLocal(self: *Context, name: []const u8) Compiler.Error!void {
        for (self.getLocals()) |local| {
            if (std.mem.eql(u8, local.name, name)) {
                _ = self.local_variables.popOrNull() orelse return Compiler.Error.LocalOutOfBounds;
                return;
            }
        }
    }
};

const Self = @This();

/// The instructions being generated
instructions: std.ArrayList(u8),
/// A list of constant expressions used in the program
constants: std.ArrayList(Value),
/// The last instruction that was added
last_compiled_instr: ?Compiler.CompiledInstruction = null,
/// The instruction before the last instruction
penult_compiled_instr: ?Compiler.CompiledInstruction = null,

/// Initializes the scope
pub fn init(ally: std.mem.Allocator) Self {
    return .{
        .instructions = std.ArrayList(u8).init(ally),
        .constants = std.ArrayList(Value).init(ally),
    };
}

/// Deinitializes the instructions & constants in the scope
pub fn deinit(self: *Self) void {
    self.instructions.deinit();
    self.constants.deinit();
}

/// Creates the resulting bytecode from the current scope data
pub fn createBytecode(self: Self) Bytecode {
    return Bytecode{
        .instructions = self.instructions.items,
        .constants = self.constants.items,
    };
}
/// Creates bytecode from the current scope data. The data returned is memory owned by the caller
pub fn createOwnedBytecode(self: *Self) Compiler.Error!Bytecode {
    return Bytecode{
        .instructions = self.instructions.toOwnedSlice() catch return Compiler.Error.OutOfMemory,
        .constants = self.constants.toOwnedSlice() catch return Compiler.Error.OutOfMemory,
    };
}
