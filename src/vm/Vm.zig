const std = @import("std");
const utils = @import("../utils/utils.zig");
const honey = @import("../honey.zig");

const Compiler = @import("../compiler/Compiler.zig");
const opcodes = @import("../compiler/opcodes.zig");
const Opcode = opcodes.Opcode;
const Bytecode = @import("../compiler/Bytecode.zig");
const Value = @import("../compiler/value.zig").Value;

const Self = @This();

/// The type that numbers are represented as in the VM
const NumberType = f64;
/// The size of a number in bytes
const NumberSize = @sizeOf(NumberType) / @sizeOf(u8);
const HexFormat = "0x{x:0<2}";

/// The potential errors that can occur during execution
const VmError = error{
    /// `OutOfMemory` is returned when the VM runs out of memory
    OutOfMemory,
    /// `StackOverflow` is returned when the stack overflows its bounds
    StackOverflow,
    /// `StackUnderflow` is returned when the a pop is attempted on an empty stack
    StackUnderflow,
    /// `InvalidOpcode` is returned when an invalid opcode is encountered
    InvalidOpcode,
    /// `VariableNotFound` is returned when a variable is not found in the VM
    VariableNotFound,
    /// `UnhandledInstruction` is returned when an instruction is encountered that is not handled
    UnhandledInstruction,
    /// `OutOfProgramBounds` is returned when the program counter goes out of bounds
    OutOfProgramBounds,
    /// `InvalidNumberType` is returned when a fetch is attempted with an invalid number type
    InvalidNumberType,
    /// `UnexpectedValueType` is returned when the type of a value is not what was expected
    UnexpectedValueType,
    /// `GenericError` is returned when an error occurs that does not fit into any other category
    GenericError,
};

/// The list used to keep track of objects in the VM
const ObjectList = std.SinglyLinkedList(*Value);
/// The map type used for variables in the VM
const VariableMap = std.StringArrayHashMap(Value);
/// The shape of the built-in functions in the VM
const BuiltinFn = *const fn (*Self, []const Value) anyerror!?Value;

/// The allocator used for memory allocation in the VM
ally: std.mem.Allocator,
/// The bytecode object for the VM
bytecode: Bytecode,
/// The instructions to run in the VM
instructions: []const u8,
/// The constants to use in the VM
constants: []const Value,
/// The objects allocated in the arena for the VM (used for GC)
objects: ObjectList,
/// The global constants in the VM
global_constants: VariableMap,
/// The global variables in the VM
global_variables: VariableMap,
/// Built-in functions for the VM
builtins: std.StringArrayHashMap(BuiltinFn),
/// Diagnostics for the VM
diagnostics: utils.Diagnostics,
/// Whether the VM is running or not
running: bool = true,
/// Where the program is currently executing
program_counter: usize = 0,
/// The stack pointer
stack_pointer: usize = 0,
/// The stack itself
stack: utils.Stack(Value),
/// Holds the last value popped from the stack
last_popped: ?Value = null,
/// The virtual machine options
options: VmOptions,

/// The options for the virtual machine
pub const VmOptions = struct {
    /// If enabled, it will dump the bytecode into stderr before running the program
    dump_bytecode: bool = false,
};

/// Initializes the VM with the needed values
pub fn init(bytecode: Bytecode, ally: std.mem.Allocator, options: VmOptions) Self {
    var self = Self{
        .ally = ally,
        .bytecode = bytecode,
        .instructions = bytecode.instructions,
        .constants = bytecode.constants,
        .objects = .{},
        .global_constants = VariableMap.init(ally),
        .global_variables = VariableMap.init(ally),
        .builtins = std.StringArrayHashMap(BuiltinFn).init(ally),
        .diagnostics = utils.Diagnostics.init(ally),
        .stack = utils.Stack(Value).init(ally),
        .options = options,
    };
    self.addBuiltinLibrary(@import("../builtins.zig"));
    return self;
}

/// Deinitializes the VM
pub fn deinit(self: *Self) void {
    self.stack.deinit();
    self.diagnostics.deinit();
    self.builtins.deinit();
    self.global_constants.deinit();
    self.global_variables.deinit();
}

/// Returns the allocator attached to the arena
fn allocator(self: *Self) std.mem.Allocator {
    return self.ally;
}

/// Collects garbage in the VM
fn collectGarbage(self: *Self) VmError!void {
    while (self.objects.popFirst()) |node| {
        switch (node.data.*) {
            .string => self.allocator().free(node.data.string),
            .identifier => self.allocator().free(node.data.identifier),
            inline else => {
                self.diagnostics.report("Unhandled object type encountered during garbage collection: {s}", .{@tagName(node.data.*)});
                return error.GenericError;
            },
        }
        self.allocator().destroy(node.data);
    }
}

/// Pushes an object onto the object list
fn trackObject(self: *Self, data: *Value) VmError!void {
    const node = self.allocator().create(ObjectList.Node) catch |err| {
        self.diagnostics.report("Failed to allocate memory for object list node: {any}", .{err});
        return error.OutOfMemory;
    };
    node.* = .{ .data = data, .next = self.objects.first };

    self.objects.prepend(node);
}

pub fn addBuiltinLibrary(self: *Self, comptime import: type) void {
    const decls = @typeInfo(import).Struct.decls;
    inline for (decls) |decl| {
        self.builtins.put(decl.name, @field(import, decl.name)) catch unreachable;
    }
}

/// Returns the last value popped from the stack
pub fn getLastPopped(self: *Self) ?Value {
    return self.last_popped;
}

/// Runs the VM
pub fn run(self: *Self) VmError!void {
    if (self.options.dump_bytecode) {
        const writer = std.io.getStdErr().writer();
        writer.writeAll("------------ Bytecode ------------\n") catch unreachable;
        self.bytecode.dump(writer) catch unreachable;
        writer.writeAll("----------------------------------\n") catch unreachable;
    }
    while (self.program_counter < self.instructions.len) {
        const instruction = try self.fetchInstruction();
        try self.execute(instruction);
    }
}

/// Executes the instruction
fn execute(self: *Self, instruction: Opcode) VmError!void {
    if (!self.running) {
        return;
    }

    if (self.objects.len() > 10240) {
        std.debug.print("Collecting garbage...\n", .{});
        try self.collectGarbage();
    }

    switch (instruction) {
        .@"const" => {
            const constant = try self.fetchConstant();
            try self.pushOrError(constant);
        },
        // constant value instructions
        .true => try self.pushOrError(Value.True),
        .false => try self.pushOrError(Value.False),
        .null => try self.pushOrError(Value.Null),
        .void => try self.pushOrError(Value.Void),
        .pop => _ = try self.popOrError(),
        // operation instructions
        .add, .sub, .mul, .div, .mod, .pow => try self.executeArithmetic(instruction),
        .eql, .neql, .@"and", .@"or" => try self.executeLogical(instruction),
        .gt, .gt_eql, .lt, .lt_eql => try self.executeComparison(instruction),
        .neg => {
            const value = try self.popOrError();
            if (value != .number) {
                self.diagnostics.report("Attempted to negate non-number value: {s}", .{value});
                return error.UnexpectedValueType;
            }
            try self.pushOrError(.{ .number = -value.number });
        },
        .not => {
            const value = try self.popOrError();
            if (value != .boolean) {
                self.diagnostics.report("Attempted to negate non-boolean value: {s}", .{value});
                return error.UnexpectedValueType;
            }
            try self.pushOrError(nativeBoolToValue(!value.boolean));
        },
        // jump instructions
        .jump_if_false => {
            const index = try self.fetchNumber(u16);
            const value = try self.popOrError();
            if (!value.boolean) self.program_counter = index;
        },
        .jump => {
            const index = try self.fetchNumber(u16);
            self.program_counter = index;
        },
        .declare_const, .declare_var => {
            const decl_name = try self.fetchConstant();
            const value = try self.popOrError();

            const map = if (instruction == .declare_const) &self.global_constants else &self.global_variables;
            const map_name = if (instruction == .declare_const) "constant" else "variable";
            if (map.contains(decl_name.identifier)) {
                self.diagnostics.report("Global {s} already declared: {s}", .{
                    map_name,
                    decl_name.identifier,
                });
                return error.GenericError;
            }
            map.putNoClobber(decl_name.identifier, value) catch |err| {
                self.diagnostics.report("Failed to declare global {s}: {any}", .{ map_name, err });
                return error.GenericError;
            };
        },
        .set_global => {
            const variable_name = try self.fetchConstant();

            if (!self.global_variables.contains(variable_name.identifier)) {
                // check if it's a constant & error if it is
                if (self.global_constants.contains(variable_name.identifier)) {
                    self.diagnostics.report("Unable to reassign constant: {s}", .{variable_name.identifier});
                    return error.GenericError;
                }
                self.diagnostics.report("Variable not found: {s}", .{variable_name.identifier});
                return error.VariableNotFound;
            }
            const value = try self.popOrError();
            self.global_variables.put(variable_name.identifier, value) catch |err| {
                self.diagnostics.report("Failed to set global variable: {any}", .{err});
                return error.GenericError;
            };
        },
        .set_local => {
            const offset = try self.fetchNumber(u16);
            const value = self.stack.get(0) catch {
                self.diagnostics.report("Stack is empty upon attempting to set local value at offset {d} ", .{offset});
                return error.GenericError;
            };

            self.stack.set(offset, value) catch |err| {
                self.diagnostics.report("Failed to set local variable at offset {d}: {any}", .{ offset, err });
                return error.GenericError;
            };
        },
        .get_global => {
            const global_name = try self.fetchConstant();

            const value = if (self.global_variables.get(global_name.identifier)) |global|
                global
            else {
                // todo: builtin variables
                self.diagnostics.report("Variable not found: {s}", .{global_name.identifier});
                return error.GenericError;
            };
            try self.pushOrError(value);
        },
        .get_local => {
            const offset = try self.fetchNumber(u16);
            const value = self.stack.get(offset) catch {
                self.diagnostics.report("Local variable not found at offset {d}", .{offset});
                return error.GenericError;
            };
            try self.pushOrError(value);
        },
        .call_builtin => {
            const builtin = try self.fetchConstant();
            const arg_count = try self.fetchNumber(u16);
            var args_list = std.ArrayList(Value).init(self.allocator());
            for (0..arg_count) |_| {
                const arg = try self.popOrError();
                args_list.append(arg) catch |err| {
                    self.diagnostics.report("Failed to append argument to builtin arguments: {any}", .{err});
                    return error.GenericError;
                };
            }
            const args = args_list.toOwnedSlice() catch |err| {
                self.diagnostics.report("Failed to convert arguments to owned slice: {any}", .{err});
                return error.GenericError;
            };
            defer self.allocator().free(args);
            std.mem.reverse(Value, args);
            const run_func = self.builtins.get(builtin.identifier) orelse {
                self.diagnostics.report("Builtin not found: @{s}", .{builtin.identifier});
                return error.GenericError;
            };
            const output = run_func(self, args) catch |err| {
                self.diagnostics.report("Failed to run builtin function: {any}", .{err});
                return error.GenericError;
            };
            try self.pushOrError(if (output) |value| value else Value.Void);
        },
        inline else => {
            self.diagnostics.report("Unhandled instruction encountered (" ++ HexFormat ++ ") at PC {d}: {s}", .{
                instruction.byte(),
                self.program_counter,
                @tagName(instruction),
            });
            return error.UnhandledInstruction;
        },
    }
}

/// Reports any errors that have occurred during execution to stderr
pub fn report(self: *Self, error_writer: std.fs.File.Writer) void {
    if (!self.diagnostics.hasErrors()) {
        return;
    }
    error_writer.print("Encountered the following errors during execution:\n", .{}) catch unreachable;
    error_writer.print("--------------------------------------------------\n", .{}) catch unreachable;
    for (self.diagnostics.errors.items, 0..) |msg, index| {
        error_writer.print(" - {s}", .{msg}) catch unreachable;
        if (index < self.diagnostics.errors.items.len) {
            error_writer.print("\n", .{}) catch unreachable;
        }
    }
    error_writer.print("--------------------------------------------------\n", .{}) catch unreachable;
}

/// Fetches the next byte from the program
fn fetchAndIncrement(self: *Self) VmError!u8 {
    if (self.program_counter >= self.instructions.len) {
        self.diagnostics.report("Program counter ({d}) exceeded bounds of program ({d}).", .{ self.program_counter, self.instructions.len });
        return error.OutOfProgramBounds;
    }
    defer self.program_counter += 1;
    return self.instructions[self.program_counter];
}

/// Fetches the next instruction from the program
fn fetchInstruction(self: *Self) VmError!Opcode {
    const instruction = try self.fetchAndIncrement();
    return Opcode.fromByte(instruction) catch {
        self.diagnostics.report("Invalid opcode (" ++ HexFormat ++ ") encountered.", .{instruction});
        return error.InvalidOpcode;
    };
}

/// Fetches the next `count` bytes from the program
fn fetchAmount(self: *Self, count: usize) VmError![]const u8 {
    const end = self.program_counter + count;
    if (end > self.instructions.len) {
        self.diagnostics.report("Program counter ({d}) exceeded bounds of program when fetching slice ({d}).", .{ end, self.instructions.len });
        return error.OutOfProgramBounds;
    }
    defer self.program_counter += count;
    return self.instructions[self.program_counter..end];
}

/// Fetches a number from the program
fn fetchNumber(self: *Self, comptime T: type) VmError!T {
    const type_info = @typeInfo(T);
    const type_size = @sizeOf(T);
    if (type_size / 8 != 0) {
        @compileError("Invalid number type size");
    }
    const bytes = try self.fetchAmount(@sizeOf(T));
    return switch (type_info) {
        .Int => std.mem.readInt(T, bytes[0..@sizeOf(T)], .big),
        .Float => utils.bytes.bytesToFloat(T, bytes, .big),
        inline else => @compileError("Invalid type"),
    };
}

/// Fetches a constant from the program and pushes it onto the stack
fn fetchConstant(self: *Self) VmError!Value {
    const index = try self.fetchNumber(u16);
    if (index >= self.constants.len) {
        self.diagnostics.report("Constant index ({d}) exceeded bounds of constants ({d}).", .{ index, self.constants.len });
        return error.OutOfProgramBounds;
    }
    return self.constants[index];
}

/// Pushes a value onto the stack or reports and returns an error
fn pushOrError(self: *Self, value: Value) VmError!void {
    self.stack.push(value) catch |err| {
        self.diagnostics.report("Failed to push value onto stack: {any}", .{err});
        return error.StackOverflow;
    };
}

/// Pops a value from the stack or reports and returns an error
fn popOrError(self: *Self) VmError!Value {
    self.last_popped = self.stack.pop() catch |err| {
        self.diagnostics.report("Failed to pop value from stack: {any}", .{err});
        return error.StackUnderflow;
    };
    return self.last_popped.?;
}

/// Pops `N` values from the stack or reports and returns an error
fn popCountOrError(self: *Self, comptime N: comptime_int) VmError!utils.RepeatedTuple(Value, N) {
    var tuple: utils.RepeatedTuple(Value, N) = undefined;
    inline for (0..N) |index| {
        @field(tuple, std.fmt.comptimePrint("{d}", .{index})) = try self.popOrError();
    }
    return tuple;
}

/// Executes an arithmetic instruction
fn executeArithmetic(self: *Self, opcode: Opcode) VmError!void {
    const rhs: Value, const lhs: Value = try self.popCountOrError(2);

    // if the lhs is a string, we can assume we are concatenating
    if (lhs == .string) {
        const result = switch (opcode) {
            // "hello" + "world" = "helloworld"
            .add => blk: {
                if (rhs != .string) {
                    self.diagnostics.report("Attempted to concatenate string with non-string value: {s}", .{rhs});
                    return error.UnexpectedValueType;
                }

                const concatted = self.allocator().create(Value) catch |err| {
                    self.diagnostics.report("Failed to allocate memory for string concatenation: {any}", .{err});
                    return error.OutOfMemory;
                };

                concatted.* = lhs.concat(rhs, self.allocator()) catch |err| {
                    self.diagnostics.report("Failed to concatenate strings: {any}", .{err});
                    return error.GenericError;
                };

                // track object for GC
                try self.trackObject(concatted);
                break :blk concatted;
            },
            // "hello" ** 3 = "hellohellohello"
            .pow => blk: {
                if (rhs != .number) {
                    self.diagnostics.report("Attempted to raise string to non-number power: {s}", .{rhs});
                    return error.UnexpectedValueType;
                } else if (rhs.number < 0) {
                    self.diagnostics.report("Attempted to raise string to negative power: {d}", .{rhs.number});
                    return error.GenericError;
                }

                const power: usize = @intFromFloat(rhs.number);
                const concatted = try self.multiplyStr(lhs.string, power);
                errdefer self.allocator().free(concatted);

                const value = self.allocator().create(Value) catch |err| {
                    self.diagnostics.report("Failed to allocate memory for string power operation: {any}", .{err});
                    return error.OutOfMemory;
                };
                errdefer self.allocator().destroy(value);
                value.* = .{ .string = concatted };

                // track object for GC
                try self.trackObject(value);
                break :blk value;
            },
            inline else => {
                self.diagnostics.report("Unexpected opcode for string operation between {s} and {s}: {s}", .{ lhs, rhs, opcode });
                return error.GenericError;
            },
        };
        try self.pushOrError(result.*);
        return;
    }
    if (lhs != .number or rhs != .number) {
        self.diagnostics.report("Attempted to perform arithmetic on non-number values: {s} and {s}", .{ lhs, rhs });
        return error.UnexpectedValueType;
    }
    try self.pushOrError(.{ .number = switch (opcode) {
        .add => lhs.number + rhs.number,
        .sub => lhs.number - rhs.number,
        .mul => lhs.number * rhs.number,
        .div => lhs.number / rhs.number,
        .mod => @mod(lhs.number, rhs.number),
        .pow => std.math.pow(NumberType, lhs.number, rhs.number),
        inline else => unreachable,
    } });
}

fn executeComparison(self: *Self, opcode: Opcode) VmError!void {
    const rhs, const lhs = try self.popCountOrError(2);
    if (lhs != .number or rhs != .number) {
        self.diagnostics.report("Attempted to perform comparison on non-number values: {s} and {s}", .{ lhs, rhs });
        return error.UnexpectedValueType;
    }

    try self.pushOrError(nativeBoolToValue(switch (opcode) {
        .gt => lhs.number > rhs.number,
        .gt_eql => lhs.number >= rhs.number,
        .lt => lhs.number < rhs.number,
        .lt_eql => lhs.number <= rhs.number,
        inline else => unreachable,
    }));
}

/// Executes a logical instruction
fn executeLogical(self: *Self, opcode: Opcode) VmError!void {
    const rhs: Value, const lhs: Value = try self.popCountOrError(2);

    const result = switch (opcode) {
        .@"and" => lhs.@"and"(rhs) catch |err| {
            self.diagnostics.report("Failed to perform logical AND operation: {any}", .{err});
            return error.GenericError;
        },
        .@"or" => lhs.@"or"(rhs) catch |err| {
            self.diagnostics.report("Failed to perform logical OR operation: {any}", .{err});
            return error.GenericError;
        },
        .eql => nativeBoolToValue(lhs.equal(rhs)),
        .neql => nativeBoolToValue(!lhs.equal(rhs)),
        inline else => unreachable,
    };
    try self.pushOrError(result);
}

/// Multiplies a string by a power (e.g., "hello" ** 3 = "hellohellohello")
inline fn multiplyStr(self: *Self, value: []const u8, power: usize) ![]const u8 {
    const buf = self.allocator().alloc(u8, value.len * power) catch |err| {
        self.diagnostics.report("Failed to allocate memory for string power operation: {any}", .{err});
        return error.OutOfMemory;
    };

    // todo: is there a better way to do this?
    for (0..power) |index| {
        const start = index * value.len;
        const end = start + value.len;
        @memcpy(buf[start..end], value);
    }

    return buf;
}

/// Converts a native boolean to the value constant equivalent
inline fn nativeBoolToValue(value: bool) Value {
    return if (value) Value.True else Value.False;
}

test "ensure program results in correct value" {
    const ally = std.testing.allocator;
    const result = try honey.compile("1 + 2", .{ .allocator = ally, .error_writer = std.io.getStdErr().writer() });
    defer result.deinit();
    var vm = Self.init(result.data, ally, .{});
    defer vm.deinit();
    try vm.run();

    try std.testing.expectEqual(Value{ .number = 3 }, vm.getLastPopped().?);
}
