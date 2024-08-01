const std = @import("std");
const builtin = @import("builtin");
const honey = @import("../honey.zig");
const ast = @import("../parser/ast.zig");
// const Evaluator = @import("evaluator/Evaluator.zig");
const Value = @import("compiler/value.zig").Value;
const Vm = @import("vm/Vm.zig");

const wasm = @import("wasm.zig");

pub fn rand(_: *Vm, args: []const Value) !?Value {
    var prng = std.rand.DefaultPrng.init(blk: {
        const seed: u64 = undefined;
        // std.posix.getrandom(std.mem.asBytes(&seed)) catch return error.GetRandomFailed;
        break :blk seed;
    });
    const random = prng.random();
    const result = random.float(f64);
    return switch (args.len) {
        0 => .{ .number = result },
        1 => blk: {
            const arg = args[0];
            if (arg != .number) {
                return error.InvalidArgument;
            }
            const max = arg.number;
            break :blk .{ .number = @floor(result * max) };
        },
        2 => blk: {
            const arg0 = args[0];
            const arg1 = args[1];
            if (arg0 != .number or arg1 != .number) {
                return error.InvalidArgument;
            }
            const min = arg0.number;
            const max = arg1.number;
            break :blk .{ .number = @floor(result * (max - min) + min) };
        },
        else => error.InvalidNumberOfArguments,
    };
}
pub fn print(_: *Vm, args: []const Value) !?Value {
    var under_writer = if (builtin.target.isWasm()) blk: {
        break :blk wasm.LogWriter{};
    } else blk: {
        break :blk std.io.getStdErr().writer();
    };
    var buf = std.io.bufferedWriter(under_writer.any());
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

pub fn println(_: *Vm, args: []const Value) !?Value {
    var under_writer = if (builtin.target.isWasm()) blk: {
        break :blk wasm.LogWriter{};
    } else blk: {
        break :blk std.io.getStdErr().writer();
    };
    var buf = std.io.bufferedWriter(under_writer.any());
    const writer = buf.writer();
    for (args) |arg| {
        switch (arg) {
            .string => writer.print("{s}", .{arg.string}) catch return error.PrintFailed,
            inline else => writer.print("{s}", .{arg}) catch return error.PrintFailed,
        }
    }
    writer.writeByte('\n') catch return error.PrintFailed;
    buf.flush() catch return error.PrintFailed;
    return null;
}

/// Returns the name of the operating system.
pub fn os(vm: *Vm, args: []const Value) !?Value {
    if (args.len != 0) {
        return error.InvalidNumberOfArguments;
    }

    return try vm.createString(@tagName(builtin.os.tag));
}

/// The maximum size of a prompt message.
const MaxBufferSize = 1024;
/// Prints a given message and then prompts the user for input using stdin.
pub fn prompt(vm: *Vm, args: []const Value) !?Value {
    // todo: wasm-based reader
    if (builtin.target.isWasm()) {
        return error.UnsupportedBuiltin;
    }
    const stderr = std.io.getStdErr().writer();
    if (args.len != 1 or args[0] != .string) {
        return null;
    }

    const msg = args[0].string;
    stderr.print("{s}", .{msg}) catch return error.PrintFailed;

    // create a prompt buffer & a related stream
    var prompt_buf: [MaxBufferSize]u8 = undefined;
    var stream = std.io.fixedBufferStream(&prompt_buf);

    // stream the prompt message to stdin
    const stdin = std.io.getStdIn().reader();
    stdin.streamUntilDelimiter(stream.writer(), '\n', MaxBufferSize) catch |err| switch (err) {
        error.StreamTooLong => return error.PromptTooLong,
        else => return error.PromptFailed,
    };

    // trim the prompt buffer of CRLF
    const trimmed = std.mem.trim(u8, stream.getWritten(), "\r\n");

    // create a new string within the evaluator's arena and return it
    return try vm.createString(trimmed);
}

pub fn parse_number(_: *Vm, args: []const Value) !?Value {
    if (args.len != 1 or args[0] != .string) {
        return error.InvalidNumberOfArguments;
    }

    const str = args[0].string;
    const number = std.fmt.parseFloat(f64, str) catch return error.ParseFailed;
    return .{ .number = number };
}

pub fn to_string(vm: *Vm, args: []const Value) !?Value {
    if (args.len != 1) {
        return error.InvalidNumberOfArguments;
    }

    // create small buffer to write to
    var buf: [MaxBufferSize]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    const writer = stream.writer();
    writer.print("{s}", .{args[0]}) catch return error.PrintFailed;
    // alloc a new string in the arena and return it
    return try vm.createString(stream.getWritten());
}

pub fn memory(vm: *Vm, args: []const Value) !?Value {
    if (args.len != 0) {
        return error.InvalidNumberOfArguments;
    }
    _ = vm;

    // todo: implement memory
    return .{ .number = 0 };
}
