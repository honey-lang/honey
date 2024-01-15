const std = @import("std");
const clap = @import("clap");
const honey = @import("honey.zig");
const Repl = @import("utils/Repl.zig");
const ast = @import("parser/ast.zig");
const Parser = @import("parser/Parser.zig");
const Evaluator = @import("evaluator/Evaluator.zig");

const Header =
    \\
    \\       _  _                   
    \\      | || |___ _ _  ___ _  _ 
    \\      | __ / _ \ ' \/ -_) || |
    \\      |_||_\___/_||_\___|\_, |
    \\                         |__/ 
    \\  A small, toy programming language.
    \\          Version: v{s}
    \\
    \\
;

const Options =
    \\  -h, --help           Display this help menu and exit.
    \\  -r, --repl           Start the REPL.
    \\  -e, --engine <str>   Select the engine to use: 'bytecode' or 'eval' (default: 'eval')
    \\  -i, --input  <str>   Evaluate code from the command line.
    \\  <str>                Evaluate code from a given file.
;

const Engine = enum {
    bytecode,
    eval,
};

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const stdout = std.io.getStdOut().writer();

    const params = comptime clap.parseParamsComptime(Options);
    var diag = clap.Diagnostic{};
    var res = clap.parse(clap.Help, &params, clap.parsers.default, .{
        .diagnostic = &diag,
        .allocator = allocator,
    }) catch |err| {
        // Report useful error and exit
        diag.report(stdout, err) catch {};
        return;
    };
    defer res.deinit();

    const engine: Engine = if (res.args.engine != null and std.mem.eql(u8, res.args.engine.?, "bytecode")) .bytecode else .eval;

    var environment = Evaluator.Environment.init(allocator);
    defer environment.deinit();
    var evaluator = Evaluator.init(allocator, &environment);
    // handle REPL separately
    if (res.args.repl != 0) {
        if (engine == .bytecode) {
            try stdout.print("Bytecode engine is not supported in REPL mode.\n", .{});
            return;
        }
        try runRepl(&evaluator, allocator);
        return;
    }
    const input = blk: {
        if (res.args.input) |input| {
            break :blk input;
        } else if (res.positionals.len > 0) {
            var file = try std.fs.cwd().openFile(res.positionals[0], .{});
            defer file.close();
            break :blk try file.reader().readAllAlloc(allocator, std.math.maxInt(usize));
        } else {
            // show help & exit
            try stdout.print(Header ++ Options, .{honey.version});
            return;
        }
    };

    if (engine == .bytecode) {
        _ = try honey.runInVm(input, .{ .allocator = allocator });
        return;
    }

    _ = run(&evaluator, input, allocator) catch |err| {
        try stdout.print("An unexpected error occurred: {s}\n", .{@errorName(err)});
    };
}
fn runRepl(evaluator: *Evaluator, allocator: std.mem.Allocator) !void {
    var repl = try Repl.init(allocator, 1024);
    defer repl.deinit();
    try repl.addCommand("exit", "Exit the REPL", exit);
    try repl.getStdOut().print("Honey v{s} - REPL\n", .{honey.version});
    try repl.getStdOut().writeAll("Type ':exit' to exit the REPL.\n");
    while (true) {
        const input = try repl.prompt("repl > ") orelse continue;
        const result = run(evaluator, input, allocator) catch |err| {
            try repl.getStdOut().print("Error: {any}\n", .{err});
            continue;
        };
        if (result) |value| {
            try repl.getStdOut().print("Output: {s}\n", .{value});
        }
    }
}

inline fn run(evaluator: *Evaluator, input: []const u8, allocator: std.mem.Allocator) !?Evaluator.Value {
    const result = try honey.parse(input, .{ .allocator = allocator });
    defer result.deinit();
    return evaluator.run(result.data);
}

pub fn exit() !void {
    std.os.exit(0);
}
