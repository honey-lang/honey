const std = @import("std");

pub const Span = struct {
    /// The start of the span
    start: usize,
    /// The end of the span
    end: usize,

    pub fn format(self: Span, _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("{{ .start = {d}, .end = {d} }}", .{ self.start, self.end });
    }

    pub fn offset(self: Span, value: usize) Span {
        return .{ .start = self.start + value, .end = self.end + value };
    }

    pub fn offsetStart(self: Span, value: usize) Span {
        return .{ .start = self.start + value, .end = self.end };
    }

    pub fn offsetEnd(self: Span, value: usize) Span {
        return .{ .start = self.start, .end = self.end + value };
    }

    pub fn offsetBoth(self: Span, start_offset: usize, end_offset: usize) Span {
        return .{ .start = self.start + start_offset, .end = self.end + end_offset };
    }
};

pub fn Cursor(comptime T: type) type {
    return struct {
        const Self = @This();
        const PredFn = *const fn (T) bool;
        const ReadResult = struct { []const T, Span };

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

        /// Returns a slice of the input from the cursor position to the given amount
        /// or null if the index + amount exceeds the bounds
        pub fn peekSliceOrNull(self: *Self, amount: usize) ?[]const T {
            if (self.index + amount > self.input.len) return null;
            return self.input[self.index..(self.index + amount)];
        }

        /// Slices the input given a start and end index
        pub fn slice(self: *Self, start: usize, end: usize) []const T {
            return self.input[start..end];
        }

        /// Returns true if the cursor is not at the end.
        pub fn hasNext(self: *Self) bool {
            return self.index + 1 < self.input.len;
        }
    };
}
