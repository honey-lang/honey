const std = @import("std");

const Self = @This();

/// The allocator used for printing error messages
ally: std.mem.Allocator,
/// The last error message
errors: std.ArrayList([]const u8),

pub fn init(ally: std.mem.Allocator) Self {
    return .{ .ally = ally, .errors = std.ArrayList([]const u8).init(ally) };
}

/// Deinitializes the diagnostics
pub fn deinit(self: *Self) void {
    for (self.errors.items) |msg| {
        self.ally.free(msg);
    }
    self.errors.deinit();
}

/// Returns whether an error has been reported
pub fn hasErrors(self: *Self) bool {
    return self.errors.items.len > 0;
}

/// Returns the number of errors reported
pub fn errorCount(self: *Self) usize {
    return self.errors.items.len;
}

/// Reports an error
pub fn report(self: *Self, comptime fmt: []const u8, args: anytype) void {
    const msg = std.fmt.allocPrint(self.ally, fmt, args) catch @panic("Failed to allocate error message");
    self.errors.append(msg) catch @panic("Failed to append error message");
}

/// Dumps the error messages to the provided writer
pub fn dump(self: *Self, writer: anytype) void {
    writer.print("There were {d} errors reported during compilation\n", .{self.errorCount()}) catch unreachable;
    for (self.errors.items) |msg| {
        writer.print("- {s}\n", .{msg}) catch unreachable;
    }
}
