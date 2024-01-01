const std = @import("std");
const clap = @import("clap");
const honey = @import("honey.zig");
const Repl = @import("Repl.zig");
const Parser = @import("Parser.zig");
const Evaluator = @import("Evaluator.zig");

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
    \\  -h, --help          Display this help menu and exit.
    \\  -r, --repl          Start the REPL.
    \\  -i, --input <str>   Evaluate code from the command line.
    \\  <str>               Evaluate code from a given file.
;

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

    var environment = Evaluator.Environment.init(allocator);
    defer environment.deinit();
    var evaluator = Evaluator.init(allocator, &environment);
    if (res.args.repl != 0) {
        try runRepl(&evaluator, allocator);
    } else if (res.args.input) |input| {
        _ = run(&evaluator, allocator, input) catch |err| {
            try stdout.print("Error: {any}\n", .{err});
        };
    } else if (res.positionals.len > 0) {
        var file = try std.fs.cwd().openFile(res.positionals[0], .{});
        defer file.close();
        const input = try file.reader().readAllAlloc(allocator, std.math.maxInt(usize));

        _ = run(&evaluator, allocator, input) catch |err| {
            try stdout.print("Error: {any}\n", .{err});
        };
    } else {
        // show help
        try stdout.print(Header ++ Options, .{honey.version});
    }
}

fn runRepl(evaluator: *Evaluator, allocator: std.mem.Allocator) !void {
    var repl = try Repl.init(allocator, 1024);
    defer repl.deinit();
    try repl.addCommand("exit", "Exit the REPL", exit);
    try repl.getStdOut().print("Honey v{s} - REPL\n", .{honey.version});
    try repl.getStdOut().writeAll("Type ':exit' to exit the REPL.\n");
    while (true) {
        const input = try repl.prompt("repl > ") orelse continue;
        const output = run(evaluator, allocator, input) catch |err| {
            try repl.getStdOut().print("Error: {any}\n", .{err});
            continue;
        };
        if (output) |value| {
            try repl.getStdOut().print("Output: {s}\n", .{value});
        }
    }
}

fn run(evaluator: *Evaluator, allocator: std.mem.Allocator, input: []const u8) !?Evaluator.Value {
    const tokens = try honey.tokenize(input, allocator);
    var parser = Parser.init(tokens, allocator);
    const program = try parser.parse();
    defer parser.deinit();
    defer allocator.free(tokens);
    return evaluator.run(program);
}

pub fn exit() !void {
    std.os.exit(0);
}
