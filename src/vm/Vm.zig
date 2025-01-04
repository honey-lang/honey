const std = @import("std");
const utils = @import("../utils/utils.zig");
const honey = @import("../honey.zig");

const token = @import("../lexer/token.zig");

const Compiler = @import("../compiler/Compiler.zig");
const opcodes = @import("../compiler/opcodes.zig");
const Opcode = opcodes.Opcode;
const Bytecode = @import("../compiler/Bytecode.zig");
const Value = @import("../compiler/value.zig").Value;

const Self = @This();
/// The built-in functions for the VM
const BuiltinLibrary = if (@import("builtin").target.isWasm())
    @import("../wasm_builtins.zig")
else
    @import("../builtins.zig");

const HexFormat = "0x{x:0<2}";

/// The potential errors that can occur during execution
const VmError = error{
    /// `OutOfMemory` is returned when the VM runs out of memory
    OutOfMemory,
    /// `StackOverflow` is returned when the stack overflows its bounds
    StackOverflow,
    /// `StackUnderflow` is returned when the a pop is attempted on an empty stack
    StackUnderflow,
    /// `InvalidStackOffset` is returned when the VM attempts to fetch a value from the stack at an offset that doesn't exist
    InvalidStackOffset,
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
    /// `ListAccessOutOfBounds` is returned when a program attempts to access a list value out of bounds
    ListAccessOutOfBounds,
    /// `InvalidListKey` is returned when a program attempts to access a list value via a non-number value
    InvalidListKey,
    /// `GenericError` is returned when an error occurs that does not fit into any other category
    GenericError,
};

/// The list used to keep track of objects in the VM
const ObjectList = std.SinglyLinkedList(*Value);
/// The map type used for variables in the VM
const VariableMap = std.StringArrayHashMap(Value);
/// The shape of the built-in functions in the VM
const BuiltinFn = *const fn (*Self, []const Value) anyerror!?Value;

const CurrentInstructionData = struct { opcode: Opcode, program_counter: usize };
const CallFrame = struct {
    return_address: usize,
    return_slot: ?usize = null,
    stack_pointer: usize = 0,
};

/// The allocator used for memory allocation in the VM
ally: std.mem.Allocator,
/// The bytecode object for the VM
bytecode: Bytecode,
/// The objects allocated in the arena for the VM (used for GC)
objects: ObjectList = .{},
/// The global constants in the VM
global_constants: VariableMap,
/// The global variables in the VM
global_variables: VariableMap,
/// A mapping of function names to their offsets in the program
funcs: std.StringArrayHashMap(usize),
/// Built-in functions for the VM
builtins: std.StringArrayHashMap(BuiltinFn),
/// Diagnostics for the VM
diagnostics: utils.Diagnostics,
/// Whether the VM is running or not
running: bool = true,
/// Where the program is currently executing
program_counter: usize = 0,
/// The current instruction being executed
current_instruction_data: CurrentInstructionData = .{
    .opcode = undefined,
    .program_counter = 0,
},
/// The stack pointer
stack_pointer: usize = 0,
/// The stack itself
stack: utils.Stack(Value),
/// The stack used to track temporary values
temp_store: std.AutoArrayHashMap(u16, Value),
/// Holds a list of stack frames
call_stack: utils.Stack(CallFrame),
/// Holds the last value popped from the stack
last_popped: ?Value = null,
/// The currently active iterators
active_iterator_stack: utils.Stack(Value.Iterator),
/// The index of the currently active iterator
active_iterator_index: ?usize = null,
/// The virtual machine options
options: VmOptions,
/// The writer to use for output and debugging
writer: std.io.AnyWriter,

/// The options for the virtual machine
pub const VmOptions = struct {
    /// If enabled, it will dump the bytecode into stderr before running the program
    dump_bytecode: bool = false,
    /// The writer used for output and debugging
    writer: std.io.AnyWriter,
};

/// Initializes the VM with the needed values
pub fn init(bytecode: Bytecode, ally: std.mem.Allocator, options: VmOptions) Self {
    var self = Self{
        .ally = ally,
        .bytecode = bytecode,
        .global_constants = VariableMap.init(ally),
        .global_variables = VariableMap.init(ally),
        .funcs = bytecode.funcs,
        .builtins = std.StringArrayHashMap(BuiltinFn).init(ally),
        .diagnostics = utils.Diagnostics.init(ally),
        .stack = utils.Stack(Value).init(ally),
        .temp_store = std.AutoArrayHashMap(u16, Value).init(ally),
        .active_iterator_stack = utils.Stack(Value.Iterator).init(ally),
        .call_stack = utils.Stack(CallFrame).init(ally),
        .options = options,
        .writer = options.writer,
    };
    self.addBuiltinLibrary(BuiltinLibrary) catch unreachable;
    return self;
}

/// Deinitializes the VM
pub fn deinit(self: *Self) void {
    self.active_iterator_stack.deinit();
    self.stack.deinit();
    self.temp_store.deinit();
    self.diagnostics.deinit();
    self.builtins.deinit();
    self.global_constants.deinit();
    self.global_variables.deinit();
    self.funcs.deinit();
}

/// Returns the allocator attached to the arena
pub fn allocator(self: *Self) std.mem.Allocator {
    return self.ally;
}

/// Collects garbage in the VM
fn collectGarbage(self: *Self) VmError!void {
    while (self.objects.popFirst()) |node| {
        switch (node.data.*) {
            // .func => self.allocator().free(node.data.func),
            .string => self.allocator().free(node.data.string),
            .identifier => self.allocator().free(node.data.identifier),
            inline else => {
                self.reportError("Unhandled object type encountered during garbage collection: {s}", .{@tagName(node.data.*)});
                return VmError.GenericError;
            },
        }
        self.allocator().destroy(node.data);
    }
}

/// Pushes an object onto the object list
fn trackObject(self: *Self, data: *Value) VmError!void {
    const node = self.allocator().create(ObjectList.Node) catch |err| {
        self.reportError("Failed to allocate memory for object list node: {any}", .{err});
        return VmError.OutOfMemory;
    };
    node.* = .{ .data = data, .next = self.objects.first };

    self.objects.prepend(node);
}

/// Creates a string object in the VM
pub fn createString(self: *Self, value: []const u8) !Value {
    const string = self.allocator().dupe(u8, value) catch |err| {
        self.reportError("Failed to allocate memory for string: {any}", .{err});
        return VmError.OutOfMemory;
    };

    const created = self.allocator().create(Value) catch |err| {
        self.reportError("Failed to allocate memory for string concatenation: {any}", .{err});
        return error.OutOfMemory;
    };

    created.* = .{ .string = string };

    // track object for GC
    try self.trackObject(created);
    return created.*;
}

/// Adds all public functions from the import as a built-in library
/// All built-ins are represented as a top-level call (e.g., `pub fn print` turns into `@print`)
pub fn addBuiltinLibrary(self: *Self, comptime import: type) VmError!void {
    const decls = @typeInfo(import).Struct.decls;
    inline for (decls) |decl| {
        const DeclType = @TypeOf(@field(import, decl.name));
        const decl_type_info = @typeInfo(DeclType);
        const field = @field(import, decl.name);
        if (decl_type_info == .Fn) {
            // validate that the declaration is a function
            // todo: we can use this for mapping native functions to a call in the VM
            // to do so, we'll inspect the params of the function and generate a new function that takes a slice of values,
            // maps them to the correct types, and then calls the native function
            // e.g., pub fn print(data: []const u8) will turn into a function with the parameters (vm: *Vm, args: []const Value)
            self.builtins.put(decl.name, @field(import, decl.name)) catch return VmError.OutOfMemory;
            continue;
        }
        const value: Value = switch (decl_type_info) {
            .Int, .ComptimeInt => .{ .number = @as(f64, @floatFromInt(field)) },
            .Float, .ComptimeFloat => .{ .number = field },
            .Bool => .{ .boolean = field },
            // if we encounter a string slice, add it to our declared list
            .Pointer => |inner| if (inner.size == .One) .{ .string = field } else continue,
            .Null => .null,
            // not a value we can convert into our internal representation
            inline else => continue,
        };
        self.global_constants.put(decl.name, value) catch return VmError.OutOfMemory;
    }
}

pub fn declareGlobal(self: *Self, name: []const u8, value: Value) VmError!void {
    return self.global_constants.put(name, value) catch VmError.OutOfMemory;
}

/// Returns the last value popped from the stack
pub fn getLastPopped(self: *Self) ?Value {
    return self.last_popped;
}

/// Runs the VM
pub fn run(self: *Self) VmError!void {
    if (self.options.dump_bytecode) {
        self.writer.writeAll("------------ Bytecode ------------\n") catch unreachable;
        self.bytecode.dump(self.writer) catch unreachable;
        self.writer.writeAll("----------------------------------\n") catch unreachable;
    }
    while (self.running and self.program_counter < self.bytecode.instructions.len) {
        const pc = self.program_counter;
        const instruction = try self.fetchInstruction();
        self.current_instruction_data = .{ .opcode = instruction, .program_counter = pc };
        try self.execute(instruction);
    }
}

/// Executes the instruction
fn execute(self: *Self, instruction: Opcode) VmError!void {
    if (!self.running) {
        return;
    }

    // if (self.objects.len() > 10240) {
    //     std.debug.print("Collecting garbage...\n", .{});
    //     try self.collectGarbage();
    // }

    switch (instruction) {
        .@"return" => {
            // if return is called and our call stack is empty, stop our VM
            if (self.call_stack.empty()) {
                self.running = false;
                return;
            }
            std.log.info("Stack Post-Call:", .{});
            self.stack.dump();

            const frame = self.call_stack.pop() catch unreachable;
            self.program_counter = frame.return_address;
            self.stack_pointer = frame.stack_pointer;
        },
        .@"const" => {
            const constant = try self.fetchConstant();
            try self.pushOrError(constant);
        },
        .list => {
            const len = try self.fetchNumber(u16);

            var list = Value.ListMap.init(self.allocator());

            for (0..len) |index| {
                var value = try self.popOrError();
                // if it is heap-allocated, we need to initalize it
                switch (value) {
                    .string => |string| value = try self.createString(string),
                    .list => |_| {},
                    else => {},
                }
                try list.put(index, value);
            }
            try self.pushOrError(.{ .list = list });
        },
        .dict => {
            const len = try self.fetchNumber(u16);
            var dict = Value.DictMap.init(self.allocator());

            for (0..len) |_| {
                const value = try self.popOrError();
                const key = try self.popOrError();
                try dict.put(key.string, value);
            }
            try self.pushOrError(.{ .dict = dict });
        },
        .range => {
            // if inclusive, we'll add 1 to the end
            const offset: usize = if (try self.fetchAndIncrement() != 0) 1 else 0;
            const end = try self.popOrError();
            const start = try self.popOrError();

            const range = Value.Range{
                .start = @as(usize, @intFromFloat(start.number)),
                .end = @as(usize, @intFromFloat(end.number)) + offset,
            };
            try self.pushOrError(.{ .range = range });
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
                self.reportError("Attempted to negate non-number value: {s}", .{value});
                return VmError.UnexpectedValueType;
            }
            try self.pushOrError(.{ .number = -value.number });
        },
        .not => {
            const value = try self.popOrError();
            if (value != .boolean) {
                self.reportError("Attempted to negate non-boolean value: {s}", .{value});
                return VmError.UnexpectedValueType;
            }
            try self.pushOrError(nativeBoolToValue(!value.boolean));
        },
        // jump instructions
        .jump_if_false => {
            const index = try self.fetchNumber(u16);
            const value = try self.popOrError();
            if (!value.boolean) self.program_counter += index;
        },
        .jump => {
            const index = try self.fetchNumber(u16);
            self.program_counter += index;
        },
        .loop => {
            const offset = try self.fetchNumber(u16);
            self.program_counter -= offset;
        },
        .declare_const, .declare_var => {
            const decl_name = try self.fetchConstant();

            // default to null if there's no return value
            const value = self.popOrNull();

            const map = if (instruction == .declare_const) &self.global_constants else &self.global_variables;
            const map_name = if (instruction == .declare_const) "constant" else "variable";
            if (map.contains(decl_name.identifier)) {
                self.reportError("Global {s} already declared: {s}", .{
                    map_name,
                    decl_name.identifier,
                });
                return VmError.GenericError;
            }

            map.putNoClobber(decl_name.identifier, value) catch |err| {
                self.reportError("Failed to declare global {s}: {any}", .{ map_name, err });
                return VmError.GenericError;
            };
        },
        .set_global => {
            const variable_name = try self.fetchConstant();

            if (!self.global_variables.contains(variable_name.identifier)) {
                // check if it's a constant & error if it is
                if (self.global_constants.contains(variable_name.identifier)) {
                    self.reportError("Unable to reassign constant: {s}", .{variable_name.identifier});
                    return VmError.GenericError;
                }
                self.reportError("Variable not found: {s}", .{variable_name.identifier});
                return VmError.VariableNotFound;
            }
            const value = self.popOrNull();
            self.global_variables.put(variable_name.identifier, value) catch |err| {
                self.reportError("Failed to set global variable: {any}", .{err});
                return error.GenericError;
            };
        },
        .set_local => {
            const offset = try self.fetchNumber(u16);
            // we don't need to pop & reset the stack at an offset if they match
            if (offset == self.stack.size() - 1) {
                return;
            }
            const value = self.stack.pop() catch {
                self.reportError("Local variable not found at offset {d}", .{offset});
                return VmError.GenericError;
            };

            self.stack.set(offset, value) catch |err| {
                self.reportError("Failed to set local variable at offset {d}: {any}", .{ offset, err });
                return VmError.GenericError;
            };
        },
        .get_global => {
            const global_name = try self.fetchConstant();

            const value = if (self.global_variables.get(global_name.identifier)) |global|
                global
            else if (self.global_constants.get(global_name.identifier)) |constant|
                constant
            else {
                // todo: builtin variables
                self.reportError("Variable not found: {s}", .{global_name.identifier});
                return VmError.GenericError;
            };
            try self.pushOrError(value);
        },
        .get_local => {
            const offset = try self.fetchNumber(u16);
            // ensure we shift by the stack pointer when fetching at the offset
            const value = self.getOrError(offset) catch |err| {
                self.reportError("Local variable not found at offset {d}", .{offset});
                return err;
            };

            try self.pushOrError(value);
        },
        .get_index => {
            const index_value = try self.popOrError();
            const value = try self.popOrError();

            switch (value) {
                .list => {
                    if (index_value != .number) {
                        self.reportError("Expected index to be a number but got {s}", .{index_value});
                        return VmError.InvalidListKey;
                    }
                    if (@floor(index_value.number) != index_value.number) {
                        self.reportError("Expected index to be an integer but got {s}", .{index_value});
                        return VmError.GenericError;
                    }
                    const index: usize = @intFromFloat(index_value.number);
                    try self.pushOrError(value.list.get(index) orelse Value.Null);
                },
                .dict => try self.pushOrError(value.dict.get(index_value.string) orelse Value.Null),
                inline else => {
                    self.reportError("Expected expression to be a list or dictionary but got {s}", .{value});
                    return VmError.GenericError;
                },
            }
        },
        .set_index => {
            const new_value = try self.popOrError();
            // fetch list, fetch index, update at index with new expression
            const index_value = try self.popOrError();

            var value = self.stack.peekPtr() catch unreachable;
            switch (value.*) {
                .list => {
                    if (index_value != .number) {
                        self.reportError("Expected list key to be number but got {s}", .{index_value});
                        return VmError.InvalidListKey;
                    }
                    const index: usize = @intFromFloat(index_value.number);
                    try value.list.put(index, new_value);
                },
                .dict => {
                    if (index_value != .string) {
                        self.reportError("Expected dictionary key to be string but got {s}", .{index_value});
                        return VmError.GenericError;
                    }
                    try value.dict.put(index_value.string, new_value);
                },
                inline else => {
                    self.reportError("Expected list or dictionary but got {s}", .{value});
                    return VmError.GenericError;
                },
            }
        },
        .set_member => {
            const new_value = try self.popOrError();
            // fetch list, fetch index, update at index with new expression
            const index_value = try self.popOrError();

            var value = self.stack.peekPtr() catch unreachable;
            try value.dict.put(index_value.string, new_value);
        },
        .get_member => {
            const index_value = try self.popOrError();
            const value = try self.popOrError();
            try self.pushOrError(value.dict.get(index_value.string) orelse Value.Null);
        },
        .get_temp => {
            const offset = self.fetchNumber(u16) catch |err| {
                self.reportError("Unable to fetch offset for temporary variable: {any}", .{err});
                return VmError.GenericError;
            };
            const value = self.temp_store.get(offset) orelse {
                self.reportError("Unable to fetch temporary variable at offset {d}", .{offset});
                return VmError.GenericError;
            };
            self.stack.push(value) catch |err| {
                self.reportError("Unable to push temporary variable onto stack: {any}", .{err});
                return VmError.GenericError;
            };
        },
        .set_temp => {
            const offset = self.fetchNumber(u16) catch |err| {
                self.reportError("Unable to fetch offset for temporary variable: {any}", .{err});
                return VmError.GenericError;
            };
            const value = self.stack.pop() catch |err| {
                self.reportError("Unable to pop from stack for temporary value: {any}", .{err});
                return VmError.GenericError;
            };
            self.temp_store.put(offset, value) catch |err| {
                self.reportError("Unable to set temporary variable at offset {d}: {any}", .{ offset, err });
                return VmError.GenericError;
            };
        },
        .call_builtin => {
            const builtin = try self.fetchConstant();
            const arg_count = try self.fetchNumber(u16);
            var args_list = std.ArrayList(Value).init(self.allocator());
            for (0..arg_count) |_| {
                const arg = try self.popOrError();
                args_list.append(arg) catch |err| {
                    self.reportError("Failed to append argument to builtin arguments: {any}", .{err});
                    return VmError.GenericError;
                };
            }
            const args = args_list.toOwnedSlice() catch |err| {
                self.reportError("Failed to convert arguments to owned slice: {any}", .{err});
                return VmError.GenericError;
            };
            defer self.allocator().free(args);
            std.mem.reverse(Value, args);
            const run_func = self.builtins.get(builtin.identifier) orelse {
                self.reportError("Builtin not found: @{s}", .{builtin.identifier});
                return VmError.GenericError;
            };
            const output = run_func(self, args) catch return VmError.GenericError;
            try self.pushOrError(if (output) |value| value else Value.Void);
        },
        .call_func => {
            // 1. Find function (or throw error if it doesn't exist)
            const func_name = try self.fetchConstant();
            const func_offset = self.funcs.get(func_name.identifier) orelse {
                self.reportError("Unable to call undefined function '{s}'", .{func_name.identifier});
                return VmError.GenericError;
            };

            // 2. Fetch argument count
            const arg_count = try self.fetchNumber(u16);

            std.log.info("Calling {s}:", .{func_name.identifier});
            self.stack.dump();

            for (0..arg_count) |i| {
                // clone and push onto stack
                const item = self.getOrError(i) catch {
                    self.reportError("Unable to fetch arguments from stack for function call to '{s}'", .{func_name.identifier});
                    return VmError.GenericError;
                };
                try self.pushOrError(item);
            }

            // 3. Create call frame
            const frame = CallFrame{
                .return_address = self.program_counter,
                .stack_pointer = self.stack_pointer,
            };
            self.call_stack.push(frame) catch |err| {
                self.reportError("Unable to push call frame onto stack: {any}", .{err});
                return VmError.OutOfMemory;
            };

            // 4. Call function by jumping to it
            self.program_counter = func_offset;
            // 5. Offset stack pointer
            self.stack_pointer += arg_count;
        },
        .iterable_begin => {
            var iterable = self.stack.peek() catch {
                self.reportError("Expected iterable to begin but stack was empty", .{});
                return VmError.StackUnderflow;
            };
            switch (iterable) {
                .list, .dict, .range => {},
                inline else => {
                    self.reportError("Expected iterable to begin but got {s}", .{iterable});
                    return VmError.GenericError;
                },
            }
            try self.setActiveIterator(.{ .index = 0, .iterable = &iterable });
        },
        .iterable_next => {
            var iterator = try self.getActiveIterator();
            iterator.next();
        },
        .iterable_has_next => {
            var iterator = try self.getActiveIterator();
            try self.pushOrError(nativeBoolToValue(iterator.hasNext()));
            // self.stack.dump();
        },
        .iterable_value => {
            var iterator = try self.getActiveIterator();
            try self.pushOrError(iterator.currentValue() orelse Value.Null);
        },
        .iterable_key => {
            var iterator = try self.getActiveIterator();
            try self.pushOrError(iterator.currentKey() orelse Value.Null);
        },
        .iterable_end => {
            try self.removeActiveIterator();
        },
        // inline else => {
        //     self.diagnostics.report("Unhandled instruction encountered (" ++ HexFormat ++ ") at PC {d}: {s}", .{
        //         instruction.byte(),
        //         self.program_counter,
        //         @tagName(instruction),~
        //     });
        //     return error.UnhandledInstruction;
        // },
    }
}

const ReportedToken = token.TokenData{
    .token = .{ .invalid = '\x00' },
    .position = .{ .start = 0, .end = 0 },
};

pub fn reportError(self: *Self, comptime fmt: []const u8, args: anytype) void {
    // todo: we need to manage tokens in the compiler & vm somehow
    self.diagnostics.report("[pc: {x:0>4} | op: .{s}]: " ++ fmt, utils.mergeTuples(.{
        .{ self.current_instruction_data.program_counter, @tagName(self.current_instruction_data.opcode) },
        args,
    }), ReportedToken);
}

/// Reports any errors that have occurred during execution to stderr
pub fn report(self: *Self) void {
    if (!self.diagnostics.hasErrors()) {
        return;
    }
    const msg_data = self.diagnostics.errors.items(.msg);
    for (msg_data) |msg| {
        self.options.writer.print("error: {s}\n", .{msg}) catch unreachable;
    }
}

/// Fetches the next byte from the program
fn fetchAndIncrement(self: *Self) VmError!u8 {
    if (self.program_counter >= self.bytecode.instructions.len) {
        self.reportError("Program counter ({d}) exceeded bounds of program ({d}).", .{ self.program_counter, self.bytecode.instructions.len });
        return error.OutOfProgramBounds;
    }
    defer self.program_counter += 1;
    return self.bytecode.instructions[self.program_counter];
}

/// Fetches the next instruction from the program
fn fetchInstruction(self: *Self) VmError!Opcode {
    const instruction = try self.fetchAndIncrement();
    return Opcode.fromByte(instruction) catch {
        self.reportError("Invalid opcode (" ++ HexFormat ++ ") encountered.", .{instruction});
        return error.InvalidOpcode;
    };
}

/// Fetches the next `count` bytes from the program
fn fetchAmount(self: *Self, count: usize) VmError![]const u8 {
    const end = self.program_counter + count;
    if (end > self.bytecode.instructions.len) {
        self.reportError("Program counter ({d}) exceeded bounds of program when fetching slice ({d}).", .{ end, self.bytecode.instructions.len });
        return error.OutOfProgramBounds;
    }
    defer self.program_counter += count;
    return self.bytecode.instructions[self.program_counter..end];
}

/// Fetches a number from the program
inline fn fetchNumber(self: *Self, comptime T: type) VmError!T {
    if (@sizeOf(T) / 8 != 0) {
        @compileError("Invalid number type size");
    }
    const bytes = try self.fetchAmount(@sizeOf(T));
    return switch (@typeInfo(T)) {
        .Int => std.mem.readInt(T, bytes[0..@sizeOf(T)], .big),
        .Float => utils.bytes.bytesToFloat(T, bytes, .big),
        inline else => @compileError("Invalid type"),
    };
}

/// Fetches a constant from the program and pushes it onto the stack
fn fetchConstant(self: *Self) VmError!Value {
    const index = try self.fetchNumber(u16);
    if (index >= self.bytecode.constants.len) {
        self.reportError("Constant index ({d}) exceeded bounds of constants ({d}).", .{ index, self.bytecode.constants.len });
        return error.OutOfProgramBounds;
    }
    return self.bytecode.constants[index];
}

/// Gets the active iterator or returns null
fn getActiveIterator(self: *Self) VmError!*Value.Iterator {
    const index = self.active_iterator_index orelse {
        self.reportError("No active iterator found", .{});
        return VmError.GenericError;
    };
    const iterator = self.active_iterator_stack.getPtr(index) catch {
        self.reportError("No active iterator found", .{});
        return VmError.GenericError;
    };
    return iterator;
}

/// Removes the active iterator
fn removeActiveIterator(self: *Self) VmError!void {
    _ = self.active_iterator_stack.pop() catch {
        self.reportError("Failed to remove active iterator", .{});
        return VmError.StackUnderflow;
    };
    self.active_iterator_index = if (self.active_iterator_stack.size() > 0) self.active_iterator_stack.size() - 1 else null;
}

/// Sets the active iterator
fn setActiveIterator(self: *Self, iterator: Value.Iterator) VmError!void {
    self.active_iterator_stack.push(iterator) catch return VmError.OutOfMemory;
    self.active_iterator_index = self.active_iterator_stack.size() - 1;
}

/// Pushes a value onto the stack or reports and returns an error
fn pushOrError(self: *Self, value: Value) VmError!void {
    // std.debug.print("Pushing value onto stack: {s}\n", .{value});
    self.stack.push(value) catch |err| {
        self.reportError("Failed to push value onto stack: {any}", .{err});
        return error.StackOverflow;
    };
    // self.stack.dump();
}

/// Attempts to get a value from the stack or reports and returns an error
fn getOrError(self: *Self, offset: usize) VmError!Value {
    return self.stack.get(self.stack_pointer + offset) catch |err| {
        self.reportError("Failed to retrieve item on stack at offset {d} (original: {d}): {any}", .{
            self.stack_pointer + offset,
            offset,
            err,
        });
        return error.InvalidStackOffset;
    };
}

fn freeValue(self: *Self, value: Value) void {
    switch (value) {
        .list => |list| {
            var iterator = list.iterator();
            while (iterator.next()) |entry| {
                self.freeValue(entry.value_ptr.*);
            }
        },
        .dict => |dict| {
            var iterator = dict.iterator();
            while (iterator.next()) |entry| {
                self.freeValue(entry.value_ptr.*);
            }
        },
        .string => |_| {
            // todo: separate interned strings from reg strings
            // don't free if interned
            // self.allocator().free(string);
        },
        else => {},
    }
}

fn popOrNull(self: *Self) Value {
    // when last popped becomes the penultimate, we will free the memory since we're holding no more references to it
    if (self.last_popped) |last_popped| {
        self.freeValue(last_popped);
    }

    self.last_popped = self.stack.pop() catch return Value.Null;
    return self.last_popped.?;
}

/// Pops a value from the stack or reports and returns an error
fn popOrError(self: *Self) VmError!Value {
    // when last popped becomes the penultimate, we will free the memory since we're holding no more references to it
    if (self.last_popped) |last_popped| {
        self.freeValue(last_popped);
    }

    // std.debug.print("Popping value from stack: {s}\n", .{self.stack.peek() catch return VmError.StackUnderflow});
    self.last_popped = self.stack.pop() catch {
        self.reportError("Failed to pop value from stack", .{});
        return error.StackUnderflow;
    };
    // self.stack.dump();
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
                    self.reportError("Attempted to concatenate string with non-string value: {s}", .{rhs});
                    return error.UnexpectedValueType;
                }

                const concatted = self.allocator().create(Value) catch |err| {
                    self.reportError("Failed to allocate memory for string concatenation: {any}", .{err});
                    return error.OutOfMemory;
                };

                concatted.* = lhs.concat(rhs, self.allocator()) catch |err| {
                    self.reportError("Failed to concatenate strings: {any}", .{err});
                    return error.GenericError;
                };

                // track object for GC
                try self.trackObject(concatted);
                break :blk concatted;
            },
            // "hello" ** 3 = "hellohellohello"
            .pow => blk: {
                if (rhs != .number) {
                    self.reportError("Attempted to raise string to non-number power: {s}", .{rhs});
                    return error.UnexpectedValueType;
                } else if (rhs.number < 0) {
                    self.reportError("Attempted to raise string to negative power: {d}", .{rhs.number});
                    return error.GenericError;
                }

                const power: usize = @intFromFloat(rhs.number);
                const concatted = try self.multiplyStr(lhs.string, power);
                errdefer self.allocator().free(concatted);

                const value = self.allocator().create(Value) catch |err| {
                    self.reportError("Failed to allocate memory for string power operation: {any}", .{err});
                    return error.OutOfMemory;
                };
                errdefer self.allocator().destroy(value);
                value.* = .{ .string = concatted };

                // track object for GC
                try self.trackObject(value);
                break :blk value;
            },
            inline else => {
                self.reportError("Unexpected opcode for string operation between {s} and {s}: {s}", .{ lhs, rhs, opcode });
                return error.GenericError;
            },
        };
        try self.pushOrError(result.*);
        return;
    }
    if (lhs != .number or rhs != .number) {
        self.reportError("Attempted to perform arithmetic on non-number values: {s} and {s}", .{ lhs, rhs });
        return error.UnexpectedValueType;
    }
    try self.pushOrError(.{ .number = switch (opcode) {
        .add => lhs.number + rhs.number,
        .sub => lhs.number - rhs.number,
        .mul => lhs.number * rhs.number,
        .div => lhs.number / rhs.number,
        .mod => @mod(lhs.number, rhs.number),
        .pow => std.math.pow(@TypeOf(lhs.number), lhs.number, rhs.number),
        inline else => unreachable,
    } });
}

fn executeComparison(self: *Self, opcode: Opcode) VmError!void {
    const rhs, const lhs = try self.popCountOrError(2);
    if (lhs != .number or rhs != .number) {
        self.reportError("Attempted to perform comparison on non-number values: {s} and {s}", .{ lhs, rhs });
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
            self.reportError("Failed to perform logical AND operation: {any}", .{err});
            return error.GenericError;
        },
        .@"or" => lhs.@"or"(rhs) catch |err| {
            self.reportError("Failed to perform logical OR operation: {any}", .{err});
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
        self.reportError("Failed to allocate memory for string power operation: {any}", .{err});
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
    const error_writer = std.io.getStdErr().writer().any();
    const result = try honey.compile(.{ .string = "1 + 2" }, .{
        .allocator = ally,
        .error_writer = error_writer,
    });
    defer result.deinit();
    var vm = Self.init(result.data, ally, .{
        .error_writer = error_writer,
    });
    defer vm.deinit();
    try vm.run();

    try std.testing.expectEqual(Value{ .number = 3 }, vm.getLastPopped().?);
}
