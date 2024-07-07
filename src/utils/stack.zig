const std = @import("std");

/// A stack is a data structure that allows you to push and pop values.
/// It is a LIFO (last in, first out) data structure.
pub fn Stack(comptime T: type) type {
    return struct {
        const Error = error{ OutOfMemory, StackEmpty, OutOfBounds };
        const Self = @This();

        /// The structure used to store the data in the stack.
        data: std.ArrayList(T),

        pub fn init(ally: std.mem.Allocator) Self {
            return .{ .data = std.ArrayList(T).init(ally) };
        }

        pub fn deinit(self: *Self) void {
            self.data.deinit();
        }

        /// Returns the value at the specified index.
        pub fn get(self: *Self, index: usize) Error!T {
            if (index < 0 or index >= self.data.items.len) {
                return error.OutOfBounds;
            }
            return self.data.items[index];
        }

        /// Sets the value at the specified index.
        pub fn set(self: *Self, index: usize, value: T) Error!void {
            if (index < 0 or index >= self.data.items.len) {
                return error.OutOfBounds;
            }
            self.data.items[index] = value;
        }

        /// Pushes a value onto the stack.
        pub fn push(self: *Self, value: T) Error!void {
            self.data.append(value) catch return error.OutOfMemory;
        }

        /// Pops a value off the stack or returns an error if the stack is empty.
        pub fn pop(self: *Self) Error!T {
            if (self.data.items.len == 0) {
                return error.StackEmpty;
            }
            return self.data.pop();
        }

        /// Pops and returns the top value of the stack or null if the stack is empty.
        pub fn popOrNull(self: *Self) ?T {
            return self.data.popOrNull();
        }
    };
}
