const std = @import("std");
const ast = @import("../parser/ast.zig");

const Self = @This();

name: []const u8,
parameters: []const ast.Expression,
body: ast.BlockStatement,

pub fn format(self: Self, _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
    try writer.print("fn {s}(", .{self.name});
    for (self.parameters) |parameter| {
        try writer.print("{s}", .{parameter});
    }
    try writer.print(") {s}", .{self.body});
}
