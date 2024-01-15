const std = @import("std");

/// Panics with a formatted message.
pub inline fn panicWithFormat(comptime format: []const u8, args: anytype) noreturn {
    var buf: [1024]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    const writer = stream.writer();
    std.fmt.format(writer, format, args) catch unreachable;
    @panic(stream.getWritten());
}
