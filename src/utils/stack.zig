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

        /// Returns the number of elements currently on the stack
        pub fn size(self: *Self) usize {
            return self.data.items.len;
        }

        /// Returns the top index of the stack
        pub fn top(self: *Self) usize {
            return self.data.items.len - 1;
        }

        /// Returns true if the stack contains no elements
        pub fn empty(self: *Self) bool {
            return self.data.items.len == 0;
        }

        /// Converts the stack index to the actual index in the data structure.
        /// The index is reversed because the stack is a LIFO data structure.
        inline fn resolveIndex(self: *Self, index: usize) Error!usize {
            const actual_index = self.data.items.len - index - 1;
            if (actual_index < 0 or actual_index >= self.data.items.len) {
                return error.OutOfBounds;
            }
            return actual_index;
        }

        pub fn dump(self: *Self) void {
            std.debug.print("------------- Stack -------------\n", .{});
            if (self.data.items.len == 0) {
                std.debug.print("Empty\n", .{});
            } else {
                for (0..self.data.items.len) |index| {
                    const resolved = self.resolveIndex(index) catch unreachable;
                    const item = self.data.items[resolved];
                    std.debug.print("#{d} - {s}\n", .{ index, item });
                }
            }
            std.debug.print("---------------------------------\n", .{});
        }

        /// Returns the top value of the stack or an error if the stack is empty.
        pub fn peek(self: *Self) Error!T {
            if (self.data.items.len == 0) {
                return Error.StackEmpty;
            }
            return self.data.items[self.data.items.len - 1];
        }

        /// Returns a pointer to the top value of the stack or an error if the stack is empty.
        pub fn peekPtr(self: *Self) Error!*T {
            if (self.data.items.len == 0) {
                return Error.StackEmpty;
            }
            return &(self.data.items[self.data.items.len - 1]);
        }

        /// Returns the value at the specified index.
        pub fn get(self: *Self, index: usize) Error!T {
            return self.data.items[index];
        }

        /// Returns a pointer to the value at the specified index.
        pub fn getPtr(self: *Self, index: usize) Error!*T {
            return &(self.data.items[index]);
        }

        /// Sets the value at the specified index.
        pub fn set(self: *Self, index: usize, value: T) Error!void {
            if (index >= self.data.items.len) {
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

/// A comptime-based version of the stack that has an upper bound of `capacity`
pub fn BoundedStack(comptime T: type, comptime capacity: comptime_int) type {
    return struct {
        const Error = error{ OutOfMemory, StackEmpty, OutOfBounds };
        const Self = @This();

        /// The structure used to store the data in the stack.
        data: std.BoundedArray(T, capacity) = .{},

        /// Returns the number of elements currently on the stack
        pub fn size(self: *Self) usize {
            return self.data.len;
        }

        /// Returns the top index of the stack
        pub fn top(self: *Self) usize {
            return self.data.len - 1;
        }

        /// Returns true if the stack contains no elements
        pub fn empty(self: *Self) bool {
            return self.data.len == 0;
        }

        /// Converts the stack index to the actual index in the data structure.
        /// The index is reversed because the stack is a LIFO data structure.
        inline fn resolveIndex(self: *Self, index: usize) Error!usize {
            const actual_index = self.data.len - index - 1;
            if (actual_index < 0 or actual_index >= self.data.len) {
                return error.OutOfBounds;
            }
            return actual_index;
        }

        pub fn dump(self: *Self) void {
            std.debug.print("------------- Stack -------------\n", .{});
            if (self.data.len == 0) {
                std.debug.print("Empty\n", .{});
            } else {
                for (0..self.data.len) |index| {
                    const resolved = self.resolveIndex(index) catch unreachable;
                    const item = self.data.get(resolved);
                    std.debug.print("#{d} - {s}\n", .{ index, item });
                }
            }
            std.debug.print("---------------------------------\n", .{});
        }

        /// Returns the top value of the stack or an error if the stack is empty.
        pub fn peek(self: *Self) Error!T {
            if (self.data.len == 0) {
                return Error.StackEmpty;
            }
            return self.data.get(self.data.len - 1);
        }

        /// Returns a pointer to the top value of the stack or an error if the stack is empty.
        pub fn peekPtr(self: *Self) Error!*T {
            if (self.data.len == 0) {
                return Error.StackEmpty;
            }
            return &(self.data.buffer[self.data.len - 1]);
        }

        /// Returns the value at the specified index.
        pub fn get(self: *Self, index: usize) Error!T {
            return self.data.get(index);
        }

        /// Returns a pointer to the value at the specified index.
        pub fn getPtr(self: *Self, index: usize) Error!*T {
            return &(self.data.buffer[index]);
        }

        /// Sets the value at the specified index.
        pub fn set(self: *Self, index: usize, value: T) Error!void {
            if (index >= self.data.len) {
                return error.OutOfBounds;
            }
            self.data.set(index, value);
        }

        /// Pushes a value onto the stack.
        pub fn push(self: *Self, value: T) Error!void {
            self.data.append(value) catch return error.OutOfMemory;
        }

        /// Pops a value off the stack or returns an error if the stack is empty.
        pub fn pop(self: *Self) Error!T {
            if (self.data.len == 0) {
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
