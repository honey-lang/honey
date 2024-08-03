const Position = @import("Position.zig");

pub fn Cursor(comptime T: type) type {
    return struct {
        const Self = @This();
        const PredFn = *const fn (T) bool;
        const ReadResult = struct { []const T, Position };

        input: []const T,
        index: usize,

        pub fn init(input: []const T) Self {
            return Self{ .input = input, .index = 0 };
        }

        /// Returns the current position.
        pub fn getCurrentPos(self: *Self) usize {
            return self.index;
        }

        /// Returns the current value.
        pub fn current(self: *Self) T {
            return self.input[self.index];
        }

        /// Returns true if the cursor can still read values.
        pub fn canRead(self: *Self) bool {
            return self.index < self.input.len;
        }

        /// Advances the cursor by one position.
        pub fn advance(self: *Self) void {
            self.index += 1;
        }

        /// Advances the cursor by the given amount.
        pub fn advanceAmount(self: *Self, amount: usize) void {
            self.index += amount;
        }

        /// Rewinds the cursor by one position.
        pub fn rewind(self: *Self) !void {
            if (self.index == 0) return error.UnableToRewind;
            self.index -= 1;
        }

        pub fn previous(self: *Self) ?T {
            if (self.index == 0) return null;
            return self.input[self.index - 1];
        }

        /// Reads the current value and advances the cursor by one position.
        pub fn readAndAdvance(self: *Self) ?T {
            if (self.index >= self.input.len) return null;
            defer self.index += 1;
            return self.input[self.index];
        }

        /// Reads values until the predicate returns false.
        pub fn readUntil(self: *Self, comptime pred: PredFn) ReadResult {
            const start: usize = self.index;
            while (self.canRead() and !pred(self.current())) : (self.index += 1) {}
            var end = self.index;
            // If the last value was not read, rewind the cursor by one position.
            if (start != end) end -= 1;
            return .{ self.input[start..self.index], .{ .start = start, .end = end } };
        }

        /// Reads values while the predicate returns true.
        pub fn readWhile(self: *Self, comptime pred: PredFn) ReadResult {
            const start: usize = self.index;
            while (self.canRead() and pred(self.current())) : (self.index += 1) {}
            var end = self.index;
            // If the last value was not read, rewind the cursor by one position.
            if (start != end) end -= 1;
            return .{ self.input[start..self.index], .{ .start = start, .end = end } };
        }

        /// Returns the next value without advancing the cursor or null if the cursor is at the end.
        pub fn peek(self: *Self) ?T {
            if (self.index + 1 >= self.input.len) return null;
            return self.input[self.index + 1];
        }

        /// Returns the value at the given index or null if the index is out of bounds.
        pub fn peekAhead(self: *Self, amount: usize) ?T {
            if (self.index + amount >= self.input.len) return null;
            return self.input[self.index + amount];
        }

        /// Returns a slice of the input from the current position to the given amount.
        pub fn peekSlice(self: *Self, amount: usize) []const T {
            return self.input[self.index..(self.index + amount)];
        }

        pub fn peekSliceOrNull(self: *Self, amount: usize) ?[]const T {
            if (self.index + amount > self.input.len) return null;
            return self.input[self.index..(self.index + amount)];
        }

        /// Returns true if the cursor is not at the end.
        pub fn hasNext(self: *Self) bool {
            return self.index + 1 < self.input.len;
        }
    };
}
