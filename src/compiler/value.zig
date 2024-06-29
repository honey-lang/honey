const std = @import("std");

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

    pub fn isVoid(self: Value) bool {
        return self == .void;
    }

    /// Checks if two values are equal.
    pub fn equal(self: Value, other: Value) bool {
        return switch (self) {
            .number => |value| if (other == .number) value == other.number else false,
            .boolean => |value| if (other == .boolean) value == other.boolean else false,
            .null => other == .null,
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
            inline else => {},
        }
    }
};
