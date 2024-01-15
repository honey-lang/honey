const std = @import("std");

/// Encodes the given value into a byte array.
pub fn encode(value: anytype, writer: anytype) !void {
    const ValueType = @TypeOf(value);
    const value_type_info = @typeInfo(ValueType);
    switch (value_type_info) {
        .Int => try writer.writeInt(ValueType, value, .big),
        .Float => try encodeFloat(value, writer),
        .Void => {},
        inline else => @panic("Unsupported type"),
    }
}

/// Encodes the given float into a byte array.
pub fn encodeFloat(value: f64, writer: anytype) !void {
    const value_bytes = @as([8]u8, @bitCast(value));
    try writer.writeAll(&value_bytes);
}

test "test encoding" {
    const value: u16 = 0xfffe;
    var encode_buf: [@sizeOf(@TypeOf(value)) / @sizeOf(u8)]u8 = undefined;
    var stream = std.io.fixedBufferStream(&encode_buf);
    const writer = stream.writer();

    try encode(value, writer);

    try std.testing.expectEqualSlices(u8, &[_]u8{ 0xff, 0xfe }, &encode_buf);
}
