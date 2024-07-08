const std = @import("std");
const clap = @import("clap");
pub const honey = @import("honey.zig");
const Repl = @import("utils/Repl.zig");
const ast = @import("parser/ast.zig");
const Parser = @import("parser/Parser.zig");
const Evaluator = @import("evaluator/Evaluator.zig");
const utils = @import("./utils/utils.zig");

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
    \\  -c, --compile             Compiles the input file to bytecode without running it.
    \\  -o, --output <output>       The output file for the compiled bytecode. Defaults to the input file name with the .honc extension.
    \\  -r, --repl                Start the REPL.
    \\  -e, --engine <engine>     Select the engine to use: 'bytecode' or 'eval' (default: 'eval')
    \\  -d, --dump-bytecode       Dumps the bytecode before running (only used for the bytecode engine)
    \\  -p, --print-popped        Prints the last popped value after the program runs
    \\  -i, --input  <input>      Evaluate code from the command line.
    \\  <file>                    Evaluate code from a given file.
;

const Engine = enum { bytecode, eval };

/// The parsers used to parse the arguments
const ClapParsers = .{
    .engine = clap.parsers.enumeration(Engine),
    .input = clap.parsers.string,
    .file = clap.parsers.string,
    .output = clap.parsers.string,
};

/// The default extension for compiled files
const DefaultCompiledExtension = "honc";

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

    const engine = res.args.engine orelse .bytecode;

    // print help and exit
    // if (res.args.help != 0) {
    //     try stdout.print(Header ++ Options, .{honey.version});
    //     return;
    // }

    var environment = Evaluator.Environment.init(allocator);
    defer environment.deinit();
    var evaluator = Evaluator.init(allocator, &environment);
    // handle REPL separately
    if (res.args.repl != 0) {
        try runRepl(&evaluator, allocator, engine);
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

    if (res.args.compile != 0) {
        const output_path: []const u8, const allocated: bool = if (res.args.output) |output| .{ output, false } else blk: {
            const input_path: []const u8 = res.positionals[0];
            const file_name = std.fs.path.basename(input_path);
            const output = try std.mem.concat(allocator, u8, &.{ file_name, DefaultCompiledExtension });
            break :blk .{ output, true };
        };
        // free the path if we allocated it
        defer if (allocated) allocator.free(output_path);

        const result = try honey.compile(input, .{ .allocator = allocator, .error_writer = std.io.getStdOut().writer() });
        defer result.deinit();
        // const output_file = try std.fs.cwd().createFile(output_path, .{});
        // output_file.writer().writeAll(result.data.toSlice());
        @panic("compilation is not implemented yet");
    }

    if (engine == .bytecode) {
        const result = try honey.runInVm(input, .{
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
        return;
    }

    _ = run(&evaluator, input, allocator) catch |err| {
        try stdout.print("An unexpected error occurred: {s}\n", .{@errorName(err)});
    };
}
fn runRepl(evaluator: *Evaluator, allocator: std.mem.Allocator, engine: Engine) !void {
    var repl = try Repl.init(allocator, 1024);
    defer repl.deinit();
    try repl.addCommand("exit", "Exit the REPL", exit);
    try repl.getStdOut().print("Honey v{s} - REPL\n", .{honey.version});
    try repl.getStdOut().writeAll("Type ':exit' to exit the REPL.\n");
    while (true) {
        const input = try repl.prompt("repl > ") orelse continue;
        switch (engine) {
            .bytecode => {
                // runInVm will print the error for us
                var result = honey.runInVm(input, .{
                    .allocator = allocator,
                    .error_writer = std.io.getStdOut().writer(),
                }) catch continue;
                defer result.deinit();
                if (result.data.getLastPopped()) |value| {
                    try repl.getStdOut().print("Output: {s}\n", .{value});
                }
            },
            .eval => {
                const result = run(evaluator, input, allocator) catch |err| {
                    try repl.getStdOut().print("Error: {any}\n", .{err});
                    continue;
                };
                if (result) |value| {
                    try repl.getStdOut().print("Output: {s}\n", .{value});
                }
            },
        }
    }
}

inline fn run(evaluator: *Evaluator, input: []const u8, allocator: std.mem.Allocator) !?Evaluator.Value {
    const result = try honey.parse(input, .{ .allocator = allocator, .error_writer = std.io.getStdOut().writer() });
    defer result.deinit();
    return evaluator.run(result.data);
}

pub fn exit() !void {
    std.posix.exit(0);
}

// Recursively loads all declarations for ZLS checking
comptime {
    zlsAnalyze(honey);
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
