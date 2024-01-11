const std = @import("std");

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
