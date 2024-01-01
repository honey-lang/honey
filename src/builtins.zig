const std = @import("std");
const ast = @import("ast.zig");
const Evaluator = @import("Evaluator.zig");

pub fn rand(_: *Evaluator, args: []const Evaluator.Value) ?Evaluator.Value {
    var prng = std.rand.DefaultPrng.init(blk: {
        var seed: u64 = undefined;
        std.os.getrandom(std.mem.asBytes(&seed)) catch unreachable;
        break :blk seed;
    });
    const random = prng.random();
    const result = random.float(f64);
    return switch (args.len) {
        0 => .{ .number = result },
        1 => blk: {
            const arg = args[0];
            if (arg != .number) {
                return null;
            }
            const max = arg.number;
            break :blk .{ .number = @floor(result * max) };
        },
        2 => blk: {
            const arg0 = args[0];
            const arg1 = args[1];
            if (arg0 != .number or arg1 != .number) {
                return null;
            }
            const min = arg0.number;
            const max = arg1.number;
            break :blk .{ .number = @floor(result * (max - min) + min) };
        },
        else => null,
    };
}

pub fn print(_: *Evaluator, args: []const Evaluator.Value) ?Evaluator.Value {
    const stdout = std.io.getStdOut().writer();
    for (args) |arg| {
        stdout.print("{s}", .{arg}) catch unreachable;
    }
    return null;
}

pub fn println(evaluator: *Evaluator, args: []const Evaluator.Value) ?Evaluator.Value {
    _ = print(evaluator, args);
    const stdout = std.io.getStdOut().writer();
    stdout.writeAll("\n") catch unreachable;
    return null;
}
