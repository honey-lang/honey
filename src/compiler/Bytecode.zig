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

/// Dumps the formatted instruction data into the provided writer
pub fn dump(self: Self, writer: anytype) !void {
    try writer.print("{s}\n", .{self});
}

/// Dumps the raw instruction data into the provided writer
pub fn dumpRaw(self: Self, width: u8, writer: anytype) !void {
    for (self.instructions, 1..) |byte, i| {
        try writer.print("{x:0>2} ", .{byte});
        if (i % width == 0) try writer.print("\n", .{});
    }
}

/// Generates human-readable bytecode instructions as a string
pub fn format(self: Self, _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
    var index: usize = 0;
    while (index < self.instructions.len) {
        const instruction = self.instructions[index];
        try writer.print("{x:0>4}", .{index});
        const opcode = Opcode.fromByte(instruction) catch std.debug.panic("unexpected opcode: 0x{x:0>2}", .{instruction});
        index += 1;

        try writer.print(" {s}", .{opcode});
        const operands = self.instructions[index .. index + opcode.width()];
        // formats the opcode
        try self.formatOpcode(opcode, operands, writer);
        index += opcode.width();

        if (index < self.instructions.len) {
            try writer.writeAll("\n");
        }
    }
}

fn formatOpcode(self: Self, opcode: Opcode, operands: []const u8, writer: anytype) !void {
    if (opcode.width() <= 0) return;

    switch (opcode) {
        .@"const" => {
            const const_idx = std.mem.readInt(u16, operands[0..2], .big);
            try writer.print(" {s}", .{self.constants[const_idx]});
        },
        .list, .dict => {
            const size = std.mem.readInt(u16, operands[0..2], .big);
            try writer.print(" {d}", .{size});
        },
        .jump, .jump_if_false, .loop => {
            const instr_idx = std.mem.readInt(u16, operands[0..2], .big);
            try writer.print(" {x:0>4}", .{instr_idx});
        },
        .declare_const, .declare_var, .get_global, .set_global => {
            const const_idx = std.mem.readInt(u16, operands[0..2], .big);
            try writer.print(" {s}", .{self.constants[const_idx]});
        },
        .get_local, .set_local => {
            const stack_slot = std.mem.readInt(u16, operands[0..2], .big);
            try writer.print(" {d}", .{stack_slot});
        },
        .call_builtin => {
            const const_idx = std.mem.readInt(u16, operands[0..2], .big);
            const arg_count = std.mem.readInt(u16, operands[2..4], .big);

            try writer.print(" {s} (arg count: {d})", .{ self.constants[const_idx], arg_count });
        },
        inline else => {},
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
            .{ .@"const" = 0 },
            .{ .@"const" = 1 },
            .{ .@"const" = 2 },
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
