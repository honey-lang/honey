const std = @import("std");
const honey = @import("../honey.zig");

const Compiler = @import("../compiler/Compiler.zig");
const opcodes = @import("../compiler/opcodes.zig");
const Opcode = opcodes.Opcode;
const Bytecode = @import("../compiler/Bytecode.zig");
const Value = @import("../compiler/value.zig").Value;
const Stack = @import("../utils/stack.zig").Stack;
const byte_utils = @import("../utils/bytes.zig");

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
    /// `UnhandledInstruction` is returned when an instruction is encountered that is not handled
    UnhandledInstruction,
    /// `OutOfProgramBounds` is returned when the program counter goes out of bounds
    OutOfProgramBounds,
    /// `InvalidNumberType` is returned when a fetch is attempted with an invalid number type
    InvalidNumberType,
    /// `UnexpectedValueType` is returned when the type of a value is not what was expected
    UnexpectedValueType,
};

const Diagnostics = struct {
    /// The allocator used for printing error messages
    ally: std.mem.Allocator,
    /// The last error message
    errors: std.ArrayList([]const u8),

    pub fn init(ally: std.mem.Allocator) Diagnostics {
        return .{ .ally = ally, .errors = std.ArrayList([]const u8).init(ally) };
    }

    /// Deinitializes the diagnostics
    pub fn deinit(self: *Diagnostics) void {
        for (self.errors.items) |msg| {
            self.ally.free(msg);
        }
        self.errors.deinit();
    }

    /// Returns whether an error has been reported
    pub fn hasErrors(self: *Diagnostics) bool {
        return self.errors.items.len > 0;
    }

    /// Reports an error
    pub fn report(self: *Diagnostics, comptime fmt: []const u8, args: anytype) void {
        const msg = std.fmt.allocPrint(self.ally, fmt ++ "\n", args) catch @panic("Failed to allocate error message");
        self.errors.append(msg) catch @panic("Failed to append error message");
    }
};

/// The instructions to run in the VM
instructions: []const u8,
/// The constants to use in the VM
constants: []const Value,
/// Diagnostics for the VM
diagnostics: Diagnostics,
/// Whether the VM is running or not
running: bool = true,
/// Where the program is currently executing
program_counter: usize = 0,
/// The stack pointer
stack_pointer: usize = 0,
/// The stack itself
stack: Stack(Value),

/// Initializes the VM with the needed values
pub fn init(bytecode: Bytecode, ally: std.mem.Allocator) Self {
    return Self{
        .instructions = bytecode.instructions,
        .constants = bytecode.constants,
        .diagnostics = Diagnostics.init(ally),
        .stack = Stack(Value).init(ally),
    };
}

/// Deinitializes the VM
pub fn deinit(self: *Self) void {
    self.stack.deinit();
    self.diagnostics.deinit();
}

/// Dumps the list of instructions to a hexdump
pub fn dump(self: *Self) void {
    const max_per_line = 16;
    // line length is 3 chars per byte + 1 for the space
    const line = "-" ** (max_per_line * 3 + 1) ++ "\n";
    std.debug.print("\n" ++ line, .{});
    for (self.instructions, 0..) |byte, index| {
        std.debug.print(HexFormat, .{byte});

        if (index % max_per_line == max_per_line - 1 or index == self.instructions.len - 1) {
            std.debug.print("\n", .{});
            continue;
        } else {
            std.debug.print(" ", .{});
        }
    }
    std.debug.print(line, .{});
}

/// Runs the VM
pub fn run(self: *Self) VmError!void {
    while (self.running) {
        const instruction = try self.fetchInstruction();
        try self.execute(instruction);
    }
}

/// Executes the instruction
fn execute(self: *Self, instruction: Opcode) VmError!void {
    if (!self.running) {
        return;
    }

    switch (instruction) {
        .halt => self.running = false,
        .@"const" => {
            const constant = try self.fetchConstant();
            try self.pushOrError(constant);
        },
        .add, .sub, .mul, .div, .mod => try self.executeArithmetic(instruction),
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
pub fn report(self: *Self) void {
    if (!self.diagnostics.hasErrors()) {
        return;
    }
    const stdout = std.io.getStdErr().writer();
    stdout.print("Encountered the following errors during execution:\n", .{}) catch unreachable;
    stdout.print("--------------------------------------------------\n", .{}) catch unreachable;
    for (self.diagnostics.errors.items, 0..) |msg, index| {
        stdout.print(" - {s}", .{msg}) catch unreachable;
        if (index < self.diagnostics.errors.items.len - 1) {
            stdout.print("\n", .{}) catch unreachable;
        }
    }
    stdout.print("--------------------------------------------------\n", .{}) catch unreachable;
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
    if (end >= self.instructions.len) {
        self.diagnostics.report("Program counter ({d}) exceeded bounds of program ({d}).", .{ self.program_counter, self.instructions.len });
        return error.OutOfProgramBounds;
    }
    defer self.program_counter += count;
    return self.instructions[self.program_counter..end];
}

fn fetchNumber(self: *Self, comptime T: type) VmError!T {
    const type_info = @typeInfo(T);
    const type_size = @sizeOf(T);
    if (type_size / 8 != 0) {
        self.diagnostics.report("Invalid type size ({d}) for number type.", .{type_size});
        return error.InvalidNumberType;
    }
    const bytes = try self.fetchAmount(@sizeOf(T));
    return switch (type_info) {
        .Int => std.mem.readInt(T, bytes[0..@sizeOf(T)], .big),
        .Float => byte_utils.bytesToFloat(T, bytes, .big),
        inline else => unreachable,
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
    return self.stack.pop() catch |err| {
        self.diagnostics.report("Failed to pop value from stack: {any}", .{err});
        return error.StackUnderflow;
    };
}

/// Executes an arithmetic instruction
fn executeArithmetic(self: *Self, opcode: Opcode) VmError!void {
    const first = try self.popOrError();
    const second = try self.popOrError();
    if (first != .number or second != .number) {
        // self.diagnostics.report("Attempted to perform arithmetic on non-number values: {s} and {s}", .{ first, second });
        return error.UnexpectedValueType;
    }
    const result = switch (opcode) {
        .add => first.number + second.number,
        .sub => first.number - second.number,
        .mul => first.number * second.number,
        .div => first.number / second.number,
        .mod => @mod(first.number, second.number),
        else => unreachable,
    };
    try self.pushOrError(.{ .number = result });
}

test "ensure program results in correct value" {
    const ally = std.testing.allocator;
    const result = try honey.compile("1 + 2", .{ .allocator = ally });
    defer result.deinit();
    var vm = Self.init(result.data, ally);
    defer vm.deinit();
    try vm.run();

    try std.testing.expectEqual(Value{ .number = 3 }, try vm.popOrError());
}
