const std = @import("std");
const wasm = @import("wasm.zig");
const Vm = @import("vm/Vm.zig");
const Value = @import("compiler/value.zig").Value;

/// use the existing builtins.zig module
pub usingnamespace @import("builtins.zig");

pub fn alert(_: *Vm, args: []const Value) !?Value {
    var alert_writer = wasm.AlertWriter{};
    var buf = std.io.bufferedWriter(alert_writer.any());
    const writer = buf.writer();
    for (args) |arg| {
        switch (arg) {
            .string => writer.print("{s}", .{arg.string}) catch return error.PrintFailed,
            inline else => writer.print("{s}", .{arg}) catch return error.PrintFailed,
        }
    }
    buf.flush() catch return error.PrintFailed;
    return null;
}
