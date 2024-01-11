const std = @import("std");

const Color = struct {};
const Self = @This();

stdout: std.fs.File.Writer,

pub fn init() Self {
    return .{ .stdout = std.io.getStdOut() };
}

pub fn writeColor(_: Self) void {}
