const std = @import("std");
const codec = @import("../utils/codec.zig");
const Value = @import("value.zig");

pub const Opcode = enum(u8) {
    /// The `return` opcode is used to return from a function.
    @"return" = 0x00,
    /// The `const` opcode is used to push a constant value onto the stack.
    @"const" = 0x01,
    /// The `add` opcode is used to add two values.
    add = 0x10,
    /// The `sub` opcode is used to subtract two values.
    sub = 0x11,
    /// The `mul` opcode is used to multiply two values.
    mul = 0x12,
    /// The `div` opcode is used to divide two values.
    div = 0x13,
    /// The `mod` opcode is used to calculate the remainder of two values.
    mod = 0x14,
    /// The `pow` opcode is used to calculate the power of two values.
    pow = 0x15,
    /// The halt opcode is used to halt execution.
    halt = 0xff,

    /// Converts the given opcode to a byte.
    pub fn byte(self: Opcode) u8 {
        return @intFromEnum(self);
    }

    /// Returns the width of the opcode.
    pub fn width(self: Opcode) usize {
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
    add: void,
    sub: void,
    mul: void,
    div: void,
    mod: void,
    pow: void,
    halt: void,

    pub fn opcode(self: Instruction) Opcode {
        return std.meta.stringToEnum(Opcode, @tagName(self)) orelse unreachable;
    }
};

/// Creates a program from the given instructions.
/// This is an inline function, so the byte array will not go out of scope when the function returns.
pub inline fn make(comptime instructions: []const Instruction) []u8 {
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
            inline else => |value| codec.encode(value, writer) catch @panic("Failed to encode instruction"),
        }
    }
    return &make_buf;
}

test "ensure make outputs the correct program" {
    const program = make(&.{
        .{ .@"const" = 0x00 },
        .{ .@"const" = 0x01 },
        .add,
        .halt,
    });

    // zig fmt: off
    try std.testing.expectEqualSlices(u8, &.{
        Opcode.@"const".byte(),
        0x00, 0x00,
        Opcode.@"const".byte(),
        0x00, 0x01,
        Opcode.add.byte(),
        Opcode.halt.byte(),
    }, program);
}

test "ensure width outputs the correct operand width" {
    try std.testing.expectEqual(@as(usize, @intCast(0)), Opcode.@"return".width());
    try std.testing.expectEqual(@as(usize, @intCast(2)), Opcode.@"const".width());
}