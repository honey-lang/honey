const std = @import("std");
pub const bytes = @import("bytes.zig");
pub const Cursor = @import("cursor.zig").Cursor;
pub const fmt = @import("fmt.zig");
pub const Position = @import("Position.zig");
pub const Repl = @import("Repl.zig");
pub const Stack = @import("stack.zig").Stack;
pub const Store = @import("store.zig").Store;
pub const Terminal = @import("Terminal.zig");

/// Returns a tuple of `N` elements, all of type `T`.
pub fn RepeatedTuple(comptime T: type, comptime N: comptime_int) type {
    return std.meta.Tuple(&[_]type{T} ** N);
}
