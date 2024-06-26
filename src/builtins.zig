const std = @import("std");
const honey = @import("../honey.zig");
const ast = @import("../parser/ast.zig");
// const Evaluator = @import("evaluator/Evaluator.zig");
const Value = @import("compiler/value.zig").Value;
const Vm = @import("./vm/Vm.zig");

pub fn rand(_: *Vm, args: []const Value) !?Value {
    var prng = std.rand.DefaultPrng.init(blk: {
        var seed: u64 = undefined;
        std.posix.getrandom(std.mem.asBytes(&seed)) catch return error.GetRandomFailed;
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
    const stderr = std.io.getStdErr().writer();
    for (args) |arg| {
        switch (arg) {
            .string => stderr.print("{s}", .{arg.string}) catch return error.PrintFailed,
            inline else => stderr.print("{s}", .{arg}) catch return error.PrintFailed,
        }
    }
    return null;
}

pub fn println(vm: *Vm, args: []const Value) !?Value {
    _ = try print(vm, args);
    const stderr = std.io.getStdErr().writer();
    stderr.writeAll("\n") catch return error.PrintFailed;
    return null;
}

/// The maximum size of a prompt message.
const MaxPromptSize = 1024;

/// Prints a given message and then prompts the user for input using stdin.
pub fn prompt(_: *Vm, args: []const Value) !?Value {
    const stderr = std.io.getStdErr().writer();
    if (args.len != 1 or args[0] != .string) {
        return null;
    }

    const msg = args[0].string;
    stderr.print("{s}", .{msg}) catch return error.PrintFailed;

    // create a prompt buffer & a related stream
    var prompt_buf: [MaxPromptSize]u8 = undefined;
    var stream = std.io.fixedBufferStream(&prompt_buf);

    // stream the prompt message to stdin
    const stdin = std.io.getStdIn().reader();
    stdin.streamUntilDelimiter(stream.writer(), '\n', MaxPromptSize) catch |err| switch (err) {
        error.StreamTooLong => return error.PromptTooLong,
        else => return error.PromptFailed,
    };

    // trim the prompt buffer of CRLF
    const trimmed = std.mem.trim(u8, stream.getWritten(), "\r\n");

    // create a new string within the evaluator's arena and return it
    return .{ .string = trimmed };
}

pub fn memory(vm: *Vm, args: []const Value) !?Value {
    if (args.len != 0) {
        return error.InvalidNumberOfArguments;
    }
    _ = vm;

    // todo: implement memory
    return .{ .number = 0 };
}
