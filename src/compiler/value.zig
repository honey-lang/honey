const std = @import("std");
const Bytecode = @import("Bytecode.zig");

pub const Value = union(enum) {
    pub const Error = error{
        ExpectedNumber,
        ExpectedString,
        ExpectedBoolean,
        UnexpectedType,
        OutOfMemory,
    };
    pub const True = Value{ .boolean = true };
    pub const False = Value{ .boolean = false };
    pub const Null = Value{ .null = {} };
    pub const Void = Value{ .void = {} };

    pub const ListMap = std.AutoArrayHashMap(usize, Value);
    pub const DictMap = std.StringArrayHashMap(Value);
    pub const Range = struct { start: usize, end: usize, period: usize = 1 };
    /// A small struct to keep track of the current iterator
    pub const Iterator = struct {
        index: usize,
        iterable: *Value,

        pub fn len(self: *Iterator) usize {
            return switch (self.iterable.*) {
                .range => |range| range.end - range.start,
                .list => |list| list.count(),
                .dict => |dict| dict.count(),
                inline else => std.debug.panic("Unhandled iterable type: {s}", .{@tagName(self.iterable.*)}),
            };
        }

        /// Returns true if the iterator has a next value
        pub fn hasNext(self: *Iterator) bool {
            return self.index < self.len();
        }

        /// Returns the value at the given index
        fn get(self: *Iterator, index: usize) ?Value {
            return switch (self.iterable.*) {
                .range => |range| .{ .number = @floatFromInt(range.start + index) },
                .list => |list| list.get(index),
                .dict => |dict| dict.get(dict.keys()[index]),
                inline else => unreachable,
            };
        }

        /// Returns the next value in the iterator or null if there are no more values
        pub fn next(self: *Iterator) void {
            self.index += 1;
        }

        /// Returns the current value in the iterator or null if there are no more values
        pub fn currentValue(self: *Iterator) ?Value {
            return self.get(self.index);
        }

        /// Returns the current key in the iterator or null if there are no more values
        pub fn currentKey(self: *Iterator) ?Value {
            return switch (self.iterable.*) {
                .list, .range => .{ .number = @floatFromInt(self.index) },
                .dict => |dict| .{ .string = dict.keys()[self.index] },
                inline else => unreachable,
            };
        }
    };

    pub const Function = struct { name: []const u8, parameter_count: u8, bytecode: Bytecode };

    /// `constant` represents an index to a constant in the constant pool.
    /// This constant is a value that is known at compile time.
    constant: u16,
    /// `number` represents a numerical value, both integers and floats.
    number: f64,
    /// `boolean` represents a boolean value (true or false).
    boolean: bool,
    /// `null` represents the null value and will be used for optional values.
    null: void,
    /// `void` represents the void value and will be used for functions that do not return a value.
    void: void,
    /// `string` represents a string object.
    string: []const u8,
    /// `identifier` represents an identifier.
    identifier: []const u8,
    /// `list` represents a list of values
    list: ListMap,
    /// `dict` represents a dictionary of values
    dict: DictMap,
    /// `range` represents a list of values that can be iterated
    range: Range,
    /// `func` represents a reference to a function
    func: Function,

    /// Calculates the maximum width of the payload in bytes.
    pub inline fn maxWidth() usize {
        comptime var max_width: usize = 0;
        inline for (std.enums.values(Value)) |value| {
            const current_width = value.width();
            if (current_width > max_width) max_width = current_width;
        }
        return max_width;
    }

    /// Returns the width of the payload in bytes.
    pub fn width(self: Value) usize {
        return switch (self) {
            inline else => |inner| @as(usize, @sizeOf(@TypeOf(inner))),
        };
    }

    pub fn isVoid(self: Value) bool {
        return self == .void;
    }

    pub fn isIterable(self: Value) bool {
        return self == .dict or self == .list or self == .range;
    }

    /// Checks if two values are equal.
    pub fn equal(self: Value, other: Value) bool {
        return switch (self) {
            .number => |value| if (other == .number) value == other.number else false,
            .boolean => |value| if (other == .boolean) value == other.boolean else false,
            .null => other == .null,
            // todo: string hash comparison
            .string => |value| if (other == .string) std.mem.eql(u8, value, other.string) else false,
            .list => |value| {
                if (other != .list or value.count() != other.list.count()) {
                    return false;
                }

                var value_iterator = value.iterator();
                while (value_iterator.next()) |value_entry| {
                    // get other entry from current entry's key
                    const other_entry = other.list.get(value_entry.key_ptr.*);
                    if (other_entry == null) {
                        return false;
                    }
                    if (value_entry.value_ptr.* == .string and other_entry.? == .string) {}
                    if (!value_entry.value_ptr.equal(other_entry.?)) return false;
                }

                return true;
            },
            .dict => |value| {
                if (other != .dict or value.count() != other.dict.count()) {
                    return false;
                }
                // TODO: implement dict comparison
                return true;
            },
            .range => |range| {
                if (other != .range) return false;
                return range.start == other.range.start and range.end == other.range.end and range.period == other.range.period;
            },
            inline else => false,
        };
    }

    pub fn plus(self: Value, other: Value) Error!Value {
        return switch (self) {
            .number => |value| switch (other) {
                .number => |other_value| .{ .number = value + other_value },
                inline else => return error.ExpectedNumber,
            },
            inline else => return error.UnexpectedType,
        };
    }

    pub fn minus(self: Value, other: Value) Error!Value {
        if (self == .number and other == .number) {
            return .{ .number = self.number - other.number };
        }
        return error.ExpectedNumber;
    }

    pub fn multiply(self: Value, other: Value) Error!Value {
        if (self == .number and other == .number) {
            return .{ .number = self.number * other.number };
        }
        return error.ExpectedNumber;
    }

    pub fn divide(self: Value, other: Value) Error!Value {
        if (self == .number and other == .number) {
            return .{ .number = self.number / other.number };
        }
        return error.ExpectedNumber;
    }

    pub fn modulo(self: Value, other: Value) Error!Value {
        if (self == .number and other == .number) {
            return .{ .number = @mod(self.number, other.number) };
        }
        return error.ExpectedNumber;
    }

    pub fn power(self: Value, other: Value) Error!Value {
        if (self == .number and other == .number) {
            return .{ .number = std.math.pow(@TypeOf(self.number), self.number, other.number) };
        }
        return error.ExpectedNumber;
    }

    pub fn @"or"(self: Value, other: Value) Error!Value {
        if (self == .boolean and other == .boolean) {
            return .{ .boolean = self.boolean or other.boolean };
        }
        return error.ExpectedBoolean;
    }

    pub fn @"and"(self: Value, other: Value) Error!Value {
        if (self == .boolean and other == .boolean) {
            return .{ .boolean = self.boolean and other.boolean };
        }
        return error.ExpectedBoolean;
    }

    pub fn not(self: Value) Error!Value {
        if (self == .boolean) {
            return .{ .boolean = !self.boolean };
        }
        return error.ExpectedBoolean;
    }

    pub fn greaterThan(self: Value, other: Value) Error!Value {
        if (self == .number and other == .number) {
            return .{ .boolean = self.number > other.number };
        }
        return error.ExpectedNumber;
    }

    pub fn greaterThanEqual(self: Value, other: Value) Error!Value {
        if (self == .number and other == .number) {
            return .{ .boolean = self.number >= other.number };
        }
        return error.ExpectedNumber;
    }

    pub fn lessThan(self: Value, other: Value) Error!Value {
        if (self == .number and other == .number) {
            return .{ .boolean = self.number < other.number };
        }
        return error.ExpectedNumber;
    }

    pub fn lessThanEqual(self: Value, other: Value) Error!Value {
        if (self == .number and other == .number) {
            return .{ .boolean = self.number <= other.number };
        }
        return error.ExpectedNumber;
    }

    pub fn concat(self: Value, other: Value, allocator: std.mem.Allocator) Error!Value {
        if (self != .string or other != .string) return error.ExpectedString;

        const value = std.mem.concat(allocator, u8, &.{ self.string, other.string }) catch return error.OutOfMemory;
        errdefer allocator.free(value);

        return .{ .string = value };
    }

    /// Formats a value.
    pub fn format(self: Value, _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .number => |value| try writer.print("{d}", .{value}),
            .boolean => |value| try writer.writeAll(if (value) "true" else "false"),
            .null => try writer.writeAll("null"),
            .void => try writer.writeAll("void"),
            .string => |value| try writer.print("\"{s}\"", .{value}),
            .identifier => |value| try writer.print("{s}", .{value}),
            .list => |value| {
                var iterator = value.iterator();

                var index: usize = 0;
                try writer.writeAll("[");
                while (iterator.next()) |entry| : (index += 1) {
                    try writer.print("{s}", .{entry.value_ptr});
                    if (index < value.count() - 1) {
                        try writer.writeAll(", ");
                    }
                }
                try writer.writeAll("]");
            },
            .dict => |value| {
                var iterator = value.iterator();
                try writer.writeAll("{");

                var index: usize = 0;
                while (iterator.next()) |entry| : (index += 1) {
                    try writer.print(" {s}: {s}", .{ entry.key_ptr.*, entry.value_ptr });
                    if (index < value.count() - 1) {
                        try writer.writeAll(",");
                    }
                }
                try writer.writeAll(" }");
            },
            .range => |value| {
                // TODO: it's probably best for us to store metadata about the range being inclusive
                // but in general, 0..10 == 0...9

                // TODO: Implement custom periods
                try writer.print("{d}..{d}", .{ value.start, value.end });
            },
            .func => |value| try writer.print("fn {s}(...) {{ }}", .{value.name}),
            inline else => try writer.print("{s}", .{@tagName(self)}),
        }
    }

    /// Returns the name of the type associated with the value
    pub fn typeName(self: Value) []const u8 {
        return switch (self) {
            // todo: return class names
            inline else => @tagName(self),
        };
    }
};
