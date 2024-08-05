const std = @import("std");
const honey = @import("honey.zig");
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

/// A generic writer used to wrap external console.* functions.
pub fn ConsoleWriter(comptime log_func: *const fn (msg: [*]const u8, len: usize) callconv(.C) void) type {
    return struct {
        const Self = @This();

        /// Writes a message to the log.
        pub fn write(self: *const Self, msg: []const u8) anyerror!usize {
            _ = self;
            log_func(msg.ptr, msg.len);
            return msg.len;
        }

        /// Returns a writer that can be used for logging.
        pub fn any(self: *const Self) std.io.AnyWriter {
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
}

/// A log function exported by JS to log messages from the VM.
extern fn js_log(msg: [*]const u8, len: usize) void;
/// An error function exported by JS to log errors from the VM.
extern fn js_error(msg: [*]const u8, len: usize) void;
/// A function exported by JS to prompt the user for input.
extern fn js_prompt(msg: [*]const u8, len: usize) [*:0]u8;
/// A function exported by JS to generate random bytes.
extern fn js_random_bytes(len: usize) [*]u8;

pub const LogWriter = ConsoleWriter(js_log);
pub const ErrorWriter = ConsoleWriter(js_error);

/// Logs a message using the JS `console.log` function.
pub fn honeyLog(comptime fmt: []const u8, args: anytype) !void {
    var log_writer = LogWriter{};
    try log_writer.any().print(fmt, args);
}

/// Logs an error message using the JS `console.error` function.
pub fn honeyError(comptime fmt: []const u8, args: anytype) void {
    var error_writer = ErrorWriter{};
    try error_writer.any().print(fmt, args);
}

/// Prints the given message and then prompts the user for input using JS `prompt`.
pub fn honeyPrompt(comptime buf_size: usize, comptime fmt: []const u8, args: anytype) ![*:0]const u8 {
    var buf: [buf_size]u8 = undefined;
    const fmted = try std.fmt.bufPrint(&buf, fmt, args);
    return js_prompt(fmted.ptr, fmted.len);
}

const SeedType = u64;
const SeedSize = @sizeOf(SeedType);

/// Generates a random seed using the JS `crypto.getRandomValues` function.
pub fn generateSeed() SeedType {
    const bytes = js_random_bytes(SeedSize);
    defer allocator.free(bytes[0..SeedSize]);

    return std.mem.readInt(SeedType, bytes[0..SeedSize], .big);
}

/// Allocates a slice of u8 for use in JS.
pub export fn allocU8(length: u32) [*]const u8 {
    const slice = allocator.alloc(u8, length) catch @panic("failed to allocate memory");
    return slice.ptr;
}

/// Deallocates a slice of u8 allocated by `allocU8`.
pub export fn deallocU8(slice: [*]const u8) void {
    defer allocator.free(slice[0..SeedSize]);
}

/// Exposes the version to the WASM module.
export fn version() [*:0]const u8 {
    return honey.version;
}

export fn run(source: [*]u8, source_len: usize) usize {
    var log_writer = LogWriter{};
    var error_writer = ErrorWriter{};

    const result = compile(source[0..source_len], .{
        .error_writer = error_writer.any(),
    }) catch {
        // `compile` already logs the error so we can just return 0 here.
        return 0;
    };
    defer result.deinit();

    var arena = std.heap.ArenaAllocator.init(allocator);
    var vm = Vm.init(result.data, arena.allocator(), .{
        .dump_bytecode = false,
        .writer = log_writer.any(),
    });
    vm.run() catch {
        vm.report();
        return 0;
    };
    defer vm.deinit();

    const output = std.fmt.bufPrint(&last_popped, "{s}", .{vm.getLastPopped() orelse .void}) catch |err| {
        error_writer.any().print("Error formatting last popped value: {any}\n", .{err}) catch unreachable;
        return 0;
    };

    return output.len;
}

fn tokenize(input: []const u8) Lexer.Data.Error!Lexer.Data {
    var lexer = Lexer.init(input, allocator, null);
    return try lexer.readAll();
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
        parser.report();
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
        // todo: dump errors
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
