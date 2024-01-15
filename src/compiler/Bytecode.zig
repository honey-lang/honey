const std = @import("std");
const opcodes = @import("opcodes.zig");
const Opcode = opcodes.Opcode;
const Instruction = opcodes.Instruction;
const Value = @import("value.zig").Value;

const Self = @This();

/// The generated instructions
instructions: []const u8,
/// The constant pool for which the instructions can refer to
constants: []const Value,

/// Generates human-readable bytecode instructions as a string
pub fn format(self: Self, _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
    var index: usize = 0;
    while (index < self.instructions.len) {
        const instruction = self.instructions[index];
        try writer.print("{x:0>4}", .{index});
        const opcode = Opcode.fromByte(instruction) catch unreachable;
        index += 1;

        try writer.print(" {s}", .{opcode});
        // only print data if it has any
        if (opcode.width() > 0) {
            const bytes = self.instructions[index .. index + opcode.width()];
            switch (opcode) {
                .@"const" => {
                    const const_idx = std.mem.readInt(u16, bytes[0..2], .big);
                    try writer.print(" {s}", .{self.constants[const_idx]});
                },
                inline else => {},
            }

            index += opcode.width();
        }

        if (index < self.instructions.len) {
            try writer.writeAll("\n");
        }
    }
}

test "ensure formatted instructions are correct" {
    const expected =
        \\0000 const 1
        \\0003 const 2
        \\0006 const 65535
    ;

    var buf: [1024]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    const writer = stream.writer();

    const bytecode = Self{
        .instructions = opcodes.make(&.{
            .{ .@"const" = @as(u16, @intCast(0)) },
            .{ .@"const" = @as(u16, @intCast(1)) },
            .{ .@"const" = @as(u16, @intCast(2)) },
        }),
        .constants = &.{
            .{ .number = 1 },
            .{ .number = 2 },
            .{ .number = 65535 },
        },
    };
    try std.fmt.format(writer, "{s}", .{bytecode});

    try std.testing.expectEqualSlices(u8, expected, stream.getWritten());
}