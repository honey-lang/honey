const std = @import("std");
pub const Lexer = @import("lexer/Lexer.zig");
pub const TokenData = @import("lexer/token.zig").TokenData;
pub const ast = @import("parser/ast.zig");
pub const Parser = @import("parser/Parser.zig");
pub const Compiler = @import("compiler/Compiler.zig");
pub const Bytecode = @import("compiler/Bytecode.zig");
pub const Vm = @import("vm/Vm.zig");

pub const version = "0.1.1";

pub fn tokenize(input: []const u8, allocator: std.mem.Allocator, source_name: ?[]const u8) Lexer.Data.Error!Lexer.Data {
    var lexer = Lexer.init(input, allocator, source_name);
    errdefer lexer.deinit();
    return lexer.readAll();
}

/// The source of the input to be parsed.
pub const Source = union(enum) {
    file: struct {
        name: []const u8,
        handle: std.fs.File,
    },
    string: []const u8,
};

pub const ParseOptions = struct {
    allocator: std.mem.Allocator,
    error_writer: std.io.AnyWriter,
};

pub fn parse(source: Source, options: ParseOptions) !Result(ast.Program) {
    var arena = std.heap.ArenaAllocator.init(options.allocator);
    errdefer arena.deinit();

    const input: []const u8 = switch (source) {
        .string => |string| string,
        .file => |file| try file.handle.readToEndAlloc(arena.allocator(), std.math.maxInt(usize)),
    };

    const lex_data = try tokenize(input, arena.allocator(), if (source == .file) source.file.name else null);

    var parser = Parser.init(lex_data, .{
        .ally = arena.allocator(),
        .error_writer = options.error_writer,
    });
    defer parser.deinit();

    const parsed_data = parser.parse() catch |err| {
        parser.report();
        return err;
    };

    return Result(ast.Program){
        .data = parsed_data,
        .arena = arena,
    };
}

pub const CompileOptions = struct {
    allocator: std.mem.Allocator,
    error_writer: std.io.AnyWriter,
};

pub fn compile(source: Source, options: CompileOptions) !Result(Bytecode) {
    const result = try parse(source, .{
        .allocator = options.allocator,
        .error_writer = options.error_writer,
    });
    var arena = result.arena;
    var compiler = Compiler.init(arena.allocator(), result.data);

    const program = compiler.compile() catch {
        // compiler.diagnostics.dump(options.error_writer);
        @panic("need to dump error here");
    };

    return Result(Bytecode){
        .data = program,
        .arena = result.arena,
    };
}

pub const RunOptions = struct {
    allocator: std.mem.Allocator,
    error_writer: std.io.AnyWriter,
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
        .error_writer = options.error_writer,
    });
    vm.run() catch |err| {
        vm.reportErrors();
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
