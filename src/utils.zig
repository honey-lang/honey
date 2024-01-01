const std = @import("std");

pub const Position = struct {
    start: usize,
    end: usize,

    pub fn format(self: Position, _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("{{ .start = {d}, .end = {d} }}", .{ self.start, self.end });
    }
};

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

        //// Reads values while the predicate returns true.
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

        /// Returns true if the cursor is not at the end.
        pub fn hasNext(self: *Self) bool {
            return self.index + 1 < self.input.len;
        }
    };
}

pub fn Store(comptime T: type) type {
    return struct {
        const Self = @This();
        // We use a BufSet to store the keys so that the pointers to the keys are stable.
        keys: std.BufSet,
        values: std.StringHashMap(T),

        /// Initializes the store with the given allocator.
        pub fn init(ally: std.mem.Allocator) Self {
            return .{
                .keys = std.BufSet.init(ally),
                .values = std.StringHashMap(T).init(ally),
            };
        }

        /// Deinitializes the store.
        pub fn deinit(self: *Self) void {
            self.keys.deinit();
            self.values.deinit();
        }

        /// Deletes the value with the given key.
        pub fn delete(self: *Self, name: []const u8) void {
            if (!self.values.contains(name)) {
                return;
            }
            self.keys.remove(name);
            self.values.remove(name);
        }

        /// Stores the given value under the given key.
        pub fn store(self: *Self, key: []const u8, value: T) !void {
            const result = self.values.getOrPut(key) catch unreachable;
            if (!result.found_existing) {
                // we have to store the slices ourselves because the hashmap doesn't copy them
                try self.keys.insert(key);
                result.key_ptr.* = self.keys.hash_map.getKey(key).?;
            }
            result.value_ptr.* = value;
        }

        /// Returns true if the store contains a value with the given key.
        pub fn contains(self: *Self, key: []const u8) bool {
            return self.values.contains(key);
        }

        /// Attempts to load the value with the given key.
        pub fn load(self: *Self, name: []const u8) ?T {
            return self.values.get(name);
        }
    };
}
