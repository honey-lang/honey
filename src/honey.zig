const std = @import("std");
const Lexer = @import("lexer/Lexer.zig");
const TokenData = @import("lexer/token.zig").TokenData;
const ast = @import("parser/ast.zig");
const Parser = @import("parser/Parser.zig");
const Compiler = @import("compiler/Compiler.zig");
const Bytecode = @import("compiler/Bytecode.zig");
const Evaluator = @import("evaluator/Evaluator.zig");
const Vm = @import("vm/Vm.zig");

pub const version = "0.0.1";

pub fn tokenize(input: []const u8, allocator: std.mem.Allocator) ![]const TokenData {
    var lexer = Lexer.init(input, allocator);
    errdefer lexer.deinit();
    _ = try lexer.readAll();
    return try lexer.tokens.toOwnedSlice();
}

pub const ParseOptions = struct {
    allocator: std.mem.Allocator,
    error_writer: std.fs.File.Writer,
};

pub fn parse(input: []const u8, options: ParseOptions) !Result(ast.Program) {
    var arena = std.heap.ArenaAllocator.init(options.allocator);
    errdefer arena.deinit();
    const tokens = try tokenize(input, arena.allocator());

    var parser = Parser.init(tokens, .{ .ally = arena.allocator() });
    defer parser.deinit();

    const data = parser.parse() catch |err| {
        parser.report(options.error_writer);
        return err;
    };

    return Result(ast.Program){
        .data = data,
        .arena = arena,
    };
}

pub const RunOptions = struct {
    allocator: std.mem.Allocator,
    environment: *Evaluator.Environment,
};

pub fn run(input: []const u8, options: RunOptions) !Result(?Evaluator.Value) {
    const result = try parse(input, .{ .allocator = options.allocator });
    const allocator = result.arena.allocator();
    var evaluator = Evaluator.init(allocator, options.environment);
    defer evaluator.deinit();
    return Result(?Evaluator.Value){
        .data = evaluator.run(result.data),
        .arena = result.arena,
    };
}

pub const CompileOptions = struct {
    allocator: std.mem.Allocator,
    error_writer: std.fs.File.Writer,
};

pub fn compile(input: []const u8, options: CompileOptions) !Result(Bytecode) {
    const result = try parse(input, .{
        .allocator = options.allocator,
        .error_writer = options.error_writer,
    });
    var arena = result.arena;
    var compiler = Compiler.init(arena.allocator(), result.data);
    return Result(Bytecode){
        .data = try compiler.compile(),
        .arena = result.arena,
    };
}

pub const VmRunOptions = struct {
    allocator: std.mem.Allocator,
    error_writer: std.fs.File.Writer,
};

pub fn runInVm(input: []const u8, options: VmRunOptions) !Result(Vm) {
    const result = try compile(input, .{
        .allocator = options.allocator,
        .error_writer = options.error_writer,
    });
    defer result.deinit();

    var arena = std.heap.ArenaAllocator.init(options.allocator);
    var vm = Vm.init(result.data, arena.allocator());
    vm.run() catch |err| {
        vm.report(options.error_writer);
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
