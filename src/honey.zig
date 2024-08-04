const std = @import("std");
pub const Lexer = @import("lexer/Lexer.zig");
pub const TokenData = @import("lexer/token.zig").TokenData;
pub const ast = @import("parser/ast.zig");
pub const Parser = @import("parser/Parser.zig");
pub const Compiler = @import("compiler/Compiler.zig");
pub const Bytecode = @import("compiler/Bytecode.zig");
pub const Vm = @import("vm/Vm.zig");

pub const version = "0.1.0";

pub fn tokenize(input: []const u8, allocator: std.mem.Allocator) ![]const TokenData {
    var lexer = Lexer.init(input, allocator);
    errdefer lexer.deinit();
    _ = try lexer.readAll();
    return try lexer.tokens.toOwnedSlice();
}

/// The source of the input to be parsed.
pub const Source = union(enum) {
    file: std.fs.File,
    string: []const u8,
};

pub const ParseOptions = struct {
    allocator: std.mem.Allocator,
    error_writer: std.fs.File.Writer,
};

pub fn parse(source: Source, options: ParseOptions) !Result(ast.Program) {
    var arena = std.heap.ArenaAllocator.init(options.allocator);
    errdefer arena.deinit();

    const input: []const u8 = switch (source) {
        .string => |string| string,
        .file => |file| try file.readToEndAlloc(arena.allocator(), std.math.maxInt(usize)),
    };
    const tokens = try tokenize(input, arena.allocator());

    var parser = Parser.init(tokens, .{ .ally = arena.allocator(), .error_writer = std.io.getStdErr().writer().any() });
    defer parser.deinit();

    const data = parser.parse() catch |err| {
        parser.report(options.error_writer.any());
        return err;
    };

    return Result(ast.Program){
        .data = data,
        .arena = arena,
    };
}

pub const CompileOptions = struct {
    allocator: std.mem.Allocator,
    error_writer: std.fs.File.Writer,
};

pub fn compile(source: Source, options: CompileOptions) !Result(Bytecode) {
    const result = try parse(source, .{
        .allocator = options.allocator,
        .error_writer = options.error_writer,
    });
    var arena = result.arena;
    var compiler = Compiler.init(arena.allocator(), result.data);

    const program = compiler.compile() catch |err| {
        compiler.diagnostics.dump(options.error_writer);
        return err;
    };

    return Result(Bytecode){
        .data = program,
        .arena = result.arena,
    };
}

pub const RunOptions = struct {
    allocator: std.mem.Allocator,
    error_writer: std.fs.File.Writer,
    dump_bytecode: bool = false,
};

pub fn run(source: Source, options: RunOptions) !Result(Vm) {
    const result = try compile(source, .{
        .allocator = options.allocator,
        .error_writer = options.error_writer,
    });
    defer result.deinit();

    var arena = std.heap.ArenaAllocator.init(options.allocator);
    var vm = Vm.init(result.data, arena.allocator(), .{
        .dump_bytecode = options.dump_bytecode,
        .writer = std.io.getStdErr().writer().any(),
    });
    vm.run() catch |err| {
        vm.reportErrors(options.error_writer.any());
        return err;
    };
    return Result(Vm){ .data = vm, .arena = arena };
}

/// A result type that holds a value and an arena allocator.
/// Can be used to easily free the data allocated.
pub fn Result(comptime T: type) type {
    return struct {
        data: T,
        arena: std.heap.ArenaAllocator,

        pub fn deinit(self: @This()) void {
            self.arena.deinit();
        }
    };
}
