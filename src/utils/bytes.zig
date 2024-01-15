const std = @import("std");

/// The number of bytes per line in the dump.
const MaxBytesPerLine = 16;
const ByteFormat = "{X:0<2}";

/// Converts a float to a byte array.
pub inline fn floatToBytes(value: anytype) []const u8 {
    const ValueType = @TypeOf(value);
    const value_type_info = @typeInfo(ValueType);
    if (value_type_info != .Float) {
        @compileError("floatToBytes only works on floats");
    } else if (@sizeOf(ValueType) % @sizeOf(u8) != 0) {
        @compileError("floatToBytes only works on floats with a size that is a multiple of the size of u8");
    }

    const byte_count = @sizeOf(ValueType) / @sizeOf(u8);
    return &@as([byte_count]u8, @bitCast(value));
}

/// Converts a byte array to a float.
pub inline fn bytesToFloat(comptime T: type, bytes: []const u8, endian: std.builtin.Endian) T {
    const type_info = @typeInfo(T);
    if (type_info != .Float) {
        @compileError("bytesToFloat only works on floats");
    } else if (@sizeOf(T) % @sizeOf(u8) != 0) {
        @compileError("bytesToFloat only works on floats with a size that is a multiple of the size of u8");
    }

    const IntType = @Type(.{ .Int = .{ .bits = @bitSizeOf(T), .signedness = .unsigned } });
    const int_value = std.mem.readInt(IntType, bytes[0..@sizeOf(T)], endian);
    return @as(T, @bitCast(int_value));
}

pub fn dump(bytes: []const u8) void {
    const line = ("-" ** (MaxBytesPerLine * 3 + 1)) ++ "\n";

    std.debug.print(line, .{});
    for (bytes, 0..) |byte, index| {
        std.debug.print(ByteFormat, .{byte});
        if (index % MaxBytesPerLine == MaxBytesPerLine - 1) {
            std.debug.print("\n", .{});
        } else {
            std.debug.print(" ", .{});
        }
    }
    std.debug.print("\n", .{});
    std.debug.print(line, .{});
}
