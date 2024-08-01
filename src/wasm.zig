const std = @import("std");
pub const Lexer = @import("lexer/Lexer.zig");
pub const TokenData = @import("lexer/token.zig").TokenData;
pub const ast = @import("parser/ast.zig");
pub const Parser = @import("parser/Parser.zig");
pub const Compiler = @import("compiler/Compiler.zig");
pub const Bytecode = @import("compiler/Bytecode.zig");
pub const Vm = @import("vm/Vm.zig");

const allocator = std.heap.wasm_allocator;

/// A small buffer to hold a formatted string of the last popped value.
export var last_popped: [1024]u8 = undefined;

pub const LogWriter = struct {
    /// A log function exported by JS to log messages from the VM.
    extern fn honey_log(msg: [*]const u8, len: usize) void;

    /// Writes a message to the log.
    pub fn write(self: *const LogWriter, msg: []const u8) anyerror!usize {
        _ = self; // autofix
        honey_log(msg.ptr, msg.len);
        return msg.len;
    }

    /// Returns a writer that can be used for logging.
    pub fn any(self: *const LogWriter) std.io.AnyWriter {
        return std.io.AnyWriter{
            .context = self,
            .writeFn = struct {
                pub fn write(context: *const anyopaque, bytes: []const u8) !usize {
                    var log_writer: *const LogWriter = @ptrCast(@alignCast(context));
                    return try log_writer.write(bytes);
                }
            }.write,
        };
    }
};

/// Allocates a slice of u8 for use in JS.
export fn allocU8(length: u32) [*]const u8 {
    const slice = allocator.alloc(u8, length) catch @panic("failed to allocate memory");
    return slice.ptr;
}

export fn run(source: [*]u8, source_len: usize) usize {
    var writer = LogWriter{};
    const result = compile(source[0..source_len], .{
        .error_writer = writer.any(),
    }) catch |err| {
        std.debug.panic("Error compiling source: {any}\n", .{err});
    };
    defer result.deinit();

    var arena = std.heap.ArenaAllocator.init(allocator);
    var vm = Vm.init(result.data, arena.allocator(), .{
        .dump_bytecode = false,
        .writer = writer.any(),
    });
    vm.run() catch |err| {
        std.debug.panic("Error running VM: {any}\n", .{err});
    };
    defer vm.deinit();

    const output = std.fmt.bufPrint(&last_popped, "{s}", .{vm.getLastPopped() orelse .void}) catch |err| {
        std.debug.panic("Error formatting last popped value: {any}\n", .{err});
    };

    return output.len;
}

fn tokenize(input: []const u8) ![]const TokenData {
    var lexer = Lexer.init(input, allocator);
    errdefer lexer.deinit();
    _ = try lexer.readAll();
    return try lexer.tokens.toOwnedSlice();
}

const ParseOptions = struct {
    error_writer: std.io.AnyWriter,
};

fn parse(source: []const u8, options: ParseOptions) !Result(ast.Program) {
    var arena = std.heap.ArenaAllocator.init(allocator);
    errdefer arena.deinit();

    const tokens = try tokenize(source);

    var parser = Parser.init(tokens, .{ .ally = arena.allocator(), .error_writer = options.error_writer });
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

const CompileOptions = struct {
    error_writer: std.io.AnyWriter,
};

fn compile(source: []const u8, options: CompileOptions) !Result(Bytecode) {
    const result = try parse(source, .{
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

const RunOptions = struct {
    allocator: std.mem.Allocator,
    error_writer: std.fs.File.Writer,
    dump_bytecode: bool = false,
};

/// A result type that holds a value and an arena allocator.
/// Can be used to easily free the data allocated.
fn Result(comptime T: type) type {
    return struct {
        data: T,
        arena: std.heap.ArenaAllocator,

        pub fn deinit(self: @This()) void {
            self.arena.deinit();
        }
    };
}
