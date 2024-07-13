const std = @import("std");
const clap = @import("clap");
pub const honey = @import("honey.zig");
const Repl = @import("utils/Repl.zig");
const ast = @import("parser/ast.zig");
const Parser = @import("parser/Parser.zig");
pub const utils = @import("./utils/utils.zig");

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
    \\  -h, --help                Display this help menu and exit.
    \\  -o, --output <output>       The output file for the compiled bytecode. Defaults to the input file name with the .honc extension.
    \\  -r, --repl                Start the REPL.
    \\  -d, --dump-bytecode       Dumps the bytecode before running (only used for the bytecode engine)
    \\  -p, --print-popped        Prints the last popped value after the program runs
    \\  -i, --input  <input>      Evaluate code from the command line.
    \\  <file>                    Evaluate code from a given file.
;

/// The parsers used to parse the arguments
const ClapParsers = .{
    .input = clap.parsers.string,
    .file = clap.parsers.string,
    .output = clap.parsers.string,
};

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const stdout = std.io.getStdOut().writer();

    const params = comptime clap.parseParamsComptime(Options);
    // the parsers for the arguments
    var diag = clap.Diagnostic{};
    var res = clap.parse(clap.Help, &params, &ClapParsers, .{ .diagnostic = &diag, .allocator = allocator }) catch |err| {
        // Report useful error and exit
        diag.report(stdout, err) catch {};
        return;
    };
    defer res.deinit();

    // print help and exit
    // if (res.args.help != 0) {
    //     try stdout.print(Header ++ Options, .{honey.version});
    //     return;
    // }

    // handle REPL separately
    if (res.args.repl != 0) {
        try runRepl(allocator);
        return;
    }
    const input: honey.Source = blk: {
        if (res.args.input) |input| {
            break :blk .{ .string = input };
        } else if (res.positionals.len > 0) {
            break :blk .{ .file = try std.fs.cwd().openFile(res.positionals[0], .{}) };
        } else {
            // show help & exit
            try stdout.print(Header ++ Options, .{honey.version});
            return;
        }
    };
    // close the file if we opened it
    defer if (input == .file) input.file.close();

    const result = try honey.run(input, .{
        .allocator = allocator,
        .error_writer = std.io.getStdErr().writer(),
        .dump_bytecode = res.args.@"dump-bytecode" == 1,
    });
    defer result.deinit();

    var vm = result.data;

    if (res.args.@"print-popped" == 0) return;

    if (vm.getLastPopped()) |value| {
        std.debug.print("Result: {s}\n", .{value});
    }
}
fn runRepl(allocator: std.mem.Allocator) !void {
    var repl = try Repl.init(allocator, 1024);
    defer repl.deinit();
    try repl.addCommand("exit", "Exit the REPL", exit);
    try repl.getStdOut().print("Honey v{s} - REPL\n", .{honey.version});
    try repl.getStdOut().writeAll("Type ':exit' to exit the REPL.\n");
    while (true) {
        const input = try repl.prompt("repl > ") orelse continue;
        // runInVm will print the error for us
        var result = honey.run(.{ .string = input }, .{
            .allocator = allocator,
            .error_writer = std.io.getStdOut().writer(),
        }) catch continue;
        defer result.deinit();
        if (result.data.getLastPopped()) |value| {
            try repl.getStdOut().print("Output: {s}\n", .{value});
        }
    }
}

pub fn exit() !void {
    std.posix.exit(0);
}

// Recursively loads all declarations for ZLS checking
comptime {
    zlsAnalyze(@This());
}

/// A small utility function to expose all declarations at runtime
fn zlsAnalyze(comptime T: type) void {
    inline for (declarations(T)) |decl| {
        if (@TypeOf(@field(T, decl.name)) == type) {
            zlsAnalyze(@field(T, decl.name));
        }
        _ = &@field(T, decl.name);
    }
}

/// Returns all declarations of a type
fn declarations(comptime T: type) []const std.builtin.Type.Declaration {
    return switch (@typeInfo(T)) {
        .Struct => |info| info.decls,
        .Enum => |info| info.decls,
        .Union => |info| info.decls,
        .Opaque => |info| info.decls,
        else => &.{},
    };
}
