const std = @import("std");

pub const Value = union(enum) {
    pub const True = Value{ .boolean = true };
    pub const False = Value{ .boolean = false };
    pub const Null = Value{ .null = {} };
    pub const Void = Value{ .void = {} };

    number: f64,
    string: []const u8,
    boolean: bool,
    null: void,
    void: void,

    pub fn isVoid(self: Value) bool {
        return self == .void;
    }

    /// Checks if two values are equal.
    pub fn equal(self: Value, other: Value) bool {
        return switch (self) {
            .number => |value| if (other == .number) value == other.number else false,
            .string => |value| if (other == .string) std.mem.eql(u8, value, other.string) else false,
            .boolean => |value| if (other == .boolean) value == other.boolean else false,
            .null => other == .null,
            inline else => false,
        };
    }

    pub fn plus(self: Value, other: Value, ally: std.mem.Allocator) !Value {
        return switch (self) {
            .number => |value| if (other == .number) return .{ .number = value + other.number } else return error.ExpectedNumber,
            .string => |value| .{ .string = std.fmt.allocPrint(ally, "{s}{s}", .{ value, other }) catch return error.OutOfMemory },
            inline else => return error.UnexpectedType,
        };
    }

    pub fn minus(self: Value, other: Value) !Value {
        if (self == .number and other == .number) {
            return .{ .number = self.number - other.number };
        }
        return error.ExpectedNumber;
    }

    pub fn multiply(self: Value, other: Value) !Value {
        if (self == .number and other == .number) {
            return .{ .number = self.number * other.number };
        }
        return error.ExpectedNumber;
    }

    pub fn divide(self: Value, other: Value) !Value {
        if (self == .number and other == .number) {
            return .{ .number = self.number / other.number };
        }
        return error.ExpectedNumber;
    }

    pub fn modulo(self: Value, other: Value) !Value {
        if (self == .number and other == .number) {
            return .{ .number = @mod(self.number, other.number) };
        }
        return error.ExpectedNumber;
    }

    pub fn power(self: Value, other: Value) !Value {
        if (self == .number and other == .number) {
            return .{ .number = std.math.pow(@TypeOf(self.number), self.number, other.number) };
        }
        return error.ExpectedNumber;
    }

    pub fn @"or"(self: Value, other: Value) !Value {
        if (self == .boolean and other == .boolean) {
            return .{ .boolean = self.boolean or other.boolean };
        }
        return error.ExpectedBoolean;
    }

    pub fn @"and"(self: Value, other: Value) !Value {
        if (self == .boolean and other == .boolean) {
            return .{ .boolean = self.boolean and other.boolean };
        }
        return error.ExpectedBoolean;
    }

    pub fn greaterThan(self: Value, other: Value) !Value {
        if (self == .number and other == .number) {
            return .{ .boolean = self.number > other.number };
        }
        return error.ExpectedNumber;
    }

    pub fn greaterThanEqual(self: Value, other: Value) !Value {
        if (self == .number and other == .number) {
            return .{ .boolean = self.number >= other.number };
        }
        return error.ExpectedNumber;
    }

    pub fn lessThan(self: Value, other: Value) !Value {
        if (self == .number and other == .number) {
            return .{ .boolean = self.number < other.number };
        }
        return error.ExpectedNumber;
    }

    pub fn lessThanEqual(self: Value, other: Value) !Value {
        if (self == .number and other == .number) {
            return .{ .boolean = self.number <= other.number };
        }
        return error.ExpectedNumber;
    }

    /// Formats a value.
    pub fn format(self: Value, _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .number => |value| try writer.print("{d}", .{value}),
            .string => |value| try writer.writeAll(value),
            .boolean => |value| try writer.writeAll(if (value) "true" else "false"),
            .null => try writer.writeAll("null"),
            inline else => {},
        }
    }
};
