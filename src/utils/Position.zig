const std = @import("std");
const Self = @This();

start: usize,
end: usize,

pub fn format(self: Self, _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
    try writer.print("{{ .start = {d}, .end = {d} }}", .{ self.start, self.end });
}
