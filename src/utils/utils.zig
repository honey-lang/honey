const std = @import("std");
pub const bytes = @import("bytes.zig");

pub const cursor = @import("cursor.zig");
pub const Cursor = cursor.Cursor;
pub const Span = cursor.Span;

pub const Diagnostics = @import("Diagnostics.zig");
pub const Repl = @import("Repl.zig");
pub const Stack = @import("stack.zig").Stack;
pub const BoundedStack = @import("stack.zig").BoundedStack;
pub const Store = @import("store.zig").Store;
pub const Terminal = @import("Terminal.zig");

/// Attempts to parse a number from a slice
pub fn parseNumberlike(value: []const u8) !f64 {
    const result: ?isize = std.fmt.parseInt(isize, value, 0) catch null;
    if (result) |number| {
        return @as(f64, @floatFromInt(number));
    }
    return try std.fmt.parseFloat(f64, value);
}

/// Returns a tuple of `N` elements, all of type `T`.
pub fn RepeatedTuple(comptime T: type, comptime N: comptime_int) type {
    return std.meta.Tuple(&[_]type{T} ** N);
}

/// Merges all fields of a struct into a single struct.
pub fn mergeTuples(input: anytype) MergeTuples(@TypeOf(input)) {
    const T = @TypeOf(input);
    var x: MergeTuples(T) = undefined;

    comptime var index = 0;
    inline for (std.meta.fields(T)) |item| {
        const a = @field(input, item.name);
        inline for (comptime std.meta.fieldNames(item.type)) |name| {
            @field(x, std.fmt.comptimePrint("{d}", .{index})) = @field(a, name);
            index += 1;
        }
    }
    return x;
}

fn MergeTuples(comptime T: type) type {
    var fields: []const std.builtin.Type.StructField = &.{};
    inline for (std.meta.fields(T)) |item| {
        inline for (std.meta.fields(item.type)) |f| {
            fields = fields ++ &[_]std.builtin.Type.StructField{structField(
                std.fmt.comptimePrint("{d}", .{fields.len}),
                f.type,
            )};
        }
    }
    return Struct(fields);
}

fn structField(comptime name: [:0]const u8, comptime T: type) std.builtin.Type.StructField {
    return .{ .name = name, .type = T, .default_value = null, .is_comptime = false, .alignment = @alignOf(T) };
}

fn Struct(comptime fields: []const std.builtin.Type.StructField) type {
    return @Type(.{ .Struct = .{ .layout = .auto, .fields = fields, .decls = &.{}, .is_tuple = false } });
}
