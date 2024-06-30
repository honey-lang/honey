const std = @import("std");
const opcodes = @import("opcodes.zig");

const OutputPath = "src/compiler/ARCHITECTURE.md";

const Header =
    \\ # Architecture
    \\ This document describes the architecture of the virtual machine.
    \\
    \\ ## Instructions
    \\
;

/// Generates the architecture documentation and dumps it to the output path.
pub fn main() !void {
    var file = try std.fs.cwd().createFile(OutputPath, .{});
    defer file.close();

    var table = Table.init(std.heap.page_allocator);
    defer table.deinit();

    try table.addRow(&.{ "Name", "Opcode", "Operands" });
    const instructions = @typeInfo(opcodes.Instruction).Union.fields;
    inline for (instructions) |instruction| {
        const opcode = comptime std.meta.stringToEnum(opcodes.Opcode, instruction.name).?;

        try table.addRow(&.{
            std.fmt.comptimePrint("`{s}`", .{instruction.name}),
            std.fmt.comptimePrint("0x{x:0<2}", .{@intFromEnum(opcode)}),
            comptime printTypeName(instruction.type),
        });
    }

    try file.writeAll(Header);
    try table.write(file.writer());
}

fn printTypeName(comptime T: type) []const u8 {
    return switch (@typeInfo(T)) {
        .Struct => |inner| name: {
            const fields = inner.fields;
            comptime var name: []const u8 = "";
            inline for (fields, 0..) |field, index| {
                name = name ++ printTypeName(field.type);
                if (index != fields.len - 1) {
                    name = name ++ std.fmt.comptimePrint(", ", .{});
                }
            }
            break :name name;
        },
        else => @typeName(T),
    };
}

const Table = struct {
    const String = []const u8;
    const Row = struct {
        data: std.ArrayList(String),

        pub fn init(allocator: std.mem.Allocator, data: []const String) !Row {
            var list = std.ArrayList(String).init(allocator);
            for (data) |cell| {
                try list.append(cell);
            }
            return .{ .data = list };
        }

        pub fn width(self: *Row) usize {
            var total: usize = 0;
            for (self.data.items) |cell| {
                total += cell.len;
            }
            return total;
        }

        pub fn deinit(self: *Row) void {
            self.data.deinit();
        }
    };

    allocator: std.mem.Allocator,
    rows: std.ArrayList(Row),
    column_widths: std.ArrayList(usize),

    pub fn init(allocator: std.mem.Allocator) Table {
        return .{
            .allocator = allocator,
            .rows = std.ArrayList(Row).init(allocator),
            .column_widths = std.ArrayList(usize).init(allocator),
        };
    }

    pub fn deinit(self: *Table) void {
        for (self.rows.items) |*row| {
            row.deinit();
        }
        self.rows.deinit();
    }

    /// Adds a row to the table.
    pub fn addRow(self: *Table, data: []const String) !void {
        const row = try Row.init(self.allocator, data);
        try self.rows.append(row);
    }

    /// Returns the number of columns in the table.
    pub inline fn columnCount(self: *Table) usize {
        return self.rows.items[0].data.items.len;
    }

    /// Returns the width of the given column.
    pub inline fn calculateColumnWidths(self: *Table) !void {
        if (self.rows.items.len == 0) {
            return error.ExpectedRowData;
        }
        self.column_widths.clearAndFree();

        const column_count = self.columnCount();
        for (self.rows.items) |*row| {
            for (row.data.items, 0..) |cell, column_index| {
                if (column_index >= column_count) {
                    return error.UnmatchedColumnCount;
                }
                if (self.column_widths.items.len <= column_index) {
                    try self.column_widths.append(cell.len);
                } else if (self.column_widths.items[column_index] < cell.len) {
                    self.column_widths.items[column_index] = cell.len;
                }
            }
        }
    }

    pub inline fn writeDivider(writer: anytype, divider_width: usize) !void {
        try writer.writeByteNTimes('-', divider_width);
        try writer.writeByte('\n');
    }

    inline fn writeCell(self: *Table, writer: anytype, cell: String, index: usize, width: usize) !void {
        try writer.writeByte('|');
        try writer.writeByte(' ');
        try writer.print("{s}", .{cell});
        // subtract the cell length and add 1 for the space
        try writer.writeByteNTimes(' ', width - cell.len + 1);
        if (index == self.columnCount() - 1) {
            try writer.writeByte('|');
        }
    }

    pub inline fn writeCrossedDivider(self: *Table, writer: anytype) !void {
        for (self.column_widths.items, 0..) |width, column| {
            try writer.writeByte('|');
            try writer.writeByte(' ');
            try writer.writeByteNTimes('-', width);
            try writer.writeByte(' ');
            if (column == self.columnCount() - 1) {
                try writer.writeByte('|');
            }
        }
        try writer.writeByte('\n');
    }

    /// Writes the table to the given writer.
    pub fn write(self: *Table, writer: anytype) !void {
        try self.calculateColumnWidths();
        for (self.rows.items, 0..) |*row, row_index| {
            if (row_index == 1) {
                try self.writeCrossedDivider(writer);
            }
            for (row.data.items, 0..) |cell, column_index| {
                try self.writeCell(
                    writer,
                    cell,
                    column_index,
                    self.column_widths.items[column_index],
                );
            }
            try writer.writeByte('\n');
        }
    }
};
