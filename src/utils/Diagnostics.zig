const std = @import("std");
const token = @import("../lexer/token.zig");

const Self = @This();

pub const ErrorData = struct {
    msg: []const u8,
    token_data: token.TokenData,
};

/// The allocator used for printing error messages
ally: std.mem.Allocator,
/// The last error message
errors: std.MultiArrayList(ErrorData) = .{},

pub fn init(ally: std.mem.Allocator) Self {
    return .{ .ally = ally };
}

/// Deinitializes the diagnostics
pub fn deinit(self: *Self) void {
    for (self.errors.items(.msg)) |msg| {
        self.ally.free(msg);
    }
    self.errors.deinit(self.ally);
}

/// Returns whether an error has been reported
pub fn hasErrors(self: *Self) bool {
    return self.errors.len > 0;
}

/// Returns the number of errors reported
pub fn errorCount(self: *Self) usize {
    return self.errors.len;
}

/// Reports an error
pub fn report(self: *Self, comptime fmt: []const u8, args: anytype, token_data: token.TokenData) void {
    self.errors.append(self.ally, ErrorData{
        // todo: errors over panics
        .msg = std.fmt.allocPrint(self.ally, fmt, args) catch @panic("Failed to allocate error message"),
        .token_data = token_data,
    }) catch @panic("Failed to append error message");
}
