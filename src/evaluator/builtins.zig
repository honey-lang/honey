const std = @import("std");
const honey = @import("../honey.zig");
const ast = @import("../parser/ast.zig");
const Evaluator = @import("Evaluator.zig");

pub fn rand(_: *Evaluator, args: []const Evaluator.Value) !?Evaluator.Value {
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

pub fn print(_: *Evaluator, args: []const Evaluator.Value) !?Evaluator.Value {
    const stdout = std.io.getStdOut().writer();
    for (args) |arg| {
        stdout.print("{s}", .{arg}) catch return error.PrintFailed;
    }
    return null;
}

pub fn println(evaluator: *Evaluator, args: []const Evaluator.Value) !?Evaluator.Value {
    _ = try print(evaluator, args);
    const stdout = std.io.getStdOut().writer();
    stdout.writeAll("\n") catch return error.PrintFailed;
    return null;
}

pub fn prompt(evaluator: *Evaluator, args: []const Evaluator.Value) !?Evaluator.Value {
    const stdout = std.io.getStdOut().writer();
    if (args.len != 1 or args[0] != .string) {
        return null;
    }
    stdout.print("{s}", .{args[0].string}) catch return error.PrintFailed;
    const stdin = std.io.getStdIn().reader();
    const data = stdin.readUntilDelimiterOrEofAlloc(evaluator.allocator(), '\n', 1024) catch return error.OutOfMemory;
    return .{ .string = data.? };
}

pub fn memory(evaluator: *Evaluator, args: []const Evaluator.Value) !?Evaluator.Value {
    if (args.len != 0) {
        return error.InvalidNumberOfArguments;
    }
    return .{ .number = @floatFromInt(evaluator.arena.queryCapacity()) };
}
