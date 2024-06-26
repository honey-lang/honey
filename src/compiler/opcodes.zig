const std = @import("std");

pub const Opcode = enum(u8) {
    /// The `return` opcode is used to return from a function.
    @"return" = 0x00,
    /// The `const` opcode is used to push a constant value onto the stack.
    @"const" = 0x01,
    /// The `pop` opcode is used to pop a value from the stack.
    pop = 0x02,
    /// The `jump` opcode is used to jump to an instruction.
    jump = 0x03,
    /// The `jump_if_false` opcode is used to jump to an instruction if the top of the stack is false.
    /// The top of the stack is popped.
    jump_if_false = 0x04,
    /// The `true` opcode is used to push a true value onto the stack.
    true = 0x10,
    /// The `false` opcode is used to push a false value onto the stack.
    false = 0x11,
    /// The `null` opcode is used to push a null value onto the stack.
    null = 0x12,
    /// The `void` opcode is used to push a void value onto the stack - this is used for functions or blocks that do not return a value.
    void = 0x13,
    /// The `add` opcode is used to add two values.
    add = 0x20,
    /// The `sub` opcode is used to subtract two values.
    sub = 0x21,
    /// The `mul` opcode is used to multiply two values.
    mul = 0x22,
    /// The `div` opcode is used to divide two values.
    div = 0x23,
    /// The `mod` opcode is used to calculate the remainder of two values.
    mod = 0x24,
    /// The `pow` opcode is used to calculate the power of two values.
    pow = 0x25,
    /// The `neg` opcode is used to negate a value (e.g., `-1`)
    neg = 0x26,
    /// The `eql` opcode is used to check if two values are equal.
    eql = 0x30,
    /// The `neql` opcode is used to check if two values are not equal.
    neql = 0x31,
    /// The `less_than` opcode is used to check if the first value is less than the second value.
    lt = 0x32,
    /// The `less_than_or_eql` opcode is used to check if the first value is less than or equal to the second value.
    lt_eql = 0x33,
    /// The `greater_than` opcode is used to check if the first value is greater than the second value.
    gt = 0x34,
    /// The `greater_than_or_eql` opcode is used to check if the first value is greater than or equal to the second value.
    gt_eql = 0x35,
    /// The `and` opcode is used to check if both values are true.
    @"and" = 0x40,
    /// The `or` opcode is used to check if either value is true.
    @"or" = 0x41,
    /// The `not` opcode is used to negate a value (e.g., `!true`).
    not = 0x42,
    /// The `call_builtin` opcode is used to call a builtin function.
    call_builtin = 0x50,
    /// The `declare_global` opcode is used to declare a global variable.
    declare_global = 0x60,
    /// The `set_global` opcode is used to set the value of a global variable.
    set_global = 0x61,
    /// The `declare_local` opcode is used to declare a local variable.
    declare_local = 0x70,
    /// The `set_local` opcode is used to set the value of a local variable.
    set_local = 0x71,
    /// The `get_variable` opcode is used to get the value of a variable. The check order is local -> global -> builtin.
    get_variable = 0x80,

    /// Converts the given opcode to a byte.
    pub fn byte(self: Opcode) u8 {
        return @intFromEnum(self);
    }

    /// Returns the size of the opcode with its operands;
    pub fn size(self: Opcode) usize {
        return self.width() + 1;
    }

    /// Returns the width of the opcode.
    pub fn width(self: Opcode) usize {
        @setEvalBranchQuota(1500);
        return switch (self) {
            inline else => |inner| @sizeOf(std.meta.TagPayload(Instruction, inner)),
        };
    }

    /// Returns the payload of the opcode.
    pub fn payload(self: Opcode) type {
        return switch (self) {
            inline else => |inner| std.meta.TagPayload(Instruction, inner),
        };
    }

    /// Returns the opcode corresponding to the given byte.
    pub fn fromByte(data: u8) !Opcode {
        return try std.meta.intToEnum(Opcode, data);
    }

    /// Encodes the given opcode into a byte array.
    pub fn encode(self: Opcode, writer: anytype) !void {
        return try writer.writeByte(self.byte());
    }

    pub fn format(self: Opcode, _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        return switch (self) {
            inline else => writer.writeAll(@tagName(self)),
        };
    }
};

pub const Instruction = union(Opcode) {
    @"return": void,
    @"const": u16,
    pop: void,
    jump: u16,
    jump_if_false: u16,
    true: void,
    false: void,
    null: void,
    void: void,
    add: void,
    sub: void,
    mul: void,
    div: void,
    mod: void,
    pow: void,
    neg: void,
    eql: void,
    neql: void,
    lt: void,
    lt_eql: void,
    gt: void,
    gt_eql: void,
    @"and": void,
    @"or": void,
    not: void,
    call_builtin: struct { constant_index: u16, arg_count: u16 },
    declare_global: u16,
    set_global: u16,
    declare_local: void,
    set_local: void,
    get_variable: u16,

    pub fn opcode(self: Instruction) Opcode {
        return std.meta.stringToEnum(Opcode, @tagName(self)) orelse unreachable;
    }
};

/// Encodes the given value into a byte array.
pub fn encode(value: anytype, writer: anytype) !void {
    const ValueType = @TypeOf(value);
    const value_type_info = @typeInfo(ValueType);
    switch (value_type_info) {
        .Int => try writer.writeInt(ValueType, value, .big),
        .Float => {
            const value_bytes = @as([@sizeOf(value)]u8, @bitCast(value));
            try writer.writeAll(&value_bytes);
        },
        .Struct => |inner| {
            inline for (inner.fields) |field| {
                try encode(@field(value, field.name), writer);
            }
        },
        .Void => {},
        inline else => @panic("Unsupported type: " ++ @typeName(ValueType)),
    }
}

/// Creates a program from the given instructions.
/// This is an inline function, so the byte array will not go out of scope when the function returns.
pub inline fn make(comptime instructions: []const Instruction) []const u8 {
    // compute the size of the program at comptime
    comptime var size: usize = 0;
    inline for (instructions) |instruction| {
        // size of the opcode + size of the instruction
        const active_tag = comptime std.meta.activeTag(instruction);
        const payload = std.meta.TagPayload(Instruction, active_tag);
        size += @sizeOf(Opcode) + @sizeOf(payload);
    }

    var make_buf = [_]u8{0} ** size;
    var stream = std.io.fixedBufferStream(&make_buf);
    const writer = stream.writer();

    inline for (instructions) |instruction| {
        // size of the opcode + size of the instruction
        const opcode: Opcode = comptime std.meta.activeTag(instruction);
        opcode.encode(writer) catch @panic("Failed to encode opcode");
        switch (instruction) {
            inline else => |value| encode(value, writer) catch @panic("Failed to encode instruction"),
        }
    }
    return &make_buf;
}

test "ensure make outputs the correct program" {
    const program = make(&.{
        .{ .@"const" = 0x00 },
        .{ .@"const" = 0x01 },
        .add,
    });

    // zig fmt: off
    try std.testing.expectEqualSlices(u8, &.{
        Opcode.@"const".byte(),
        0x00, 0x00,
        Opcode.@"const".byte(),
        0x00, 0x01,
        Opcode.add.byte(),
    }, program);
}

test "ensure width outputs the correct operand width" {
    try std.testing.expectEqual(@as(usize, @intCast(0)), Opcode.@"return".width());
    try std.testing.expectEqual(@as(usize, @intCast(2)), Opcode.@"const".width());
}