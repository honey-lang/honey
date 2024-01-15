const std = @import("std");

const Self = @This();

const Command = struct {
    const RunFn = *const fn () anyerror!void;
    name: []const u8,
    description: []const u8,
    run: RunFn,
};

allocator: std.mem.Allocator,
buffer: []u8,
prefix: u8 = ':',
commands: std.StringHashMap(Command),
stdin: std.fs.File.Reader,
stdout: std.fs.File.Writer,

pub fn init(allocator: std.mem.Allocator, max_size: usize) !Self {
    const buffer = try allocator.alloc(u8, max_size);
    return .{
        .allocator = allocator,
        .buffer = buffer,
        .commands = std.StringHashMap(Command).init(allocator),
        .stdin = std.io.getStdIn().reader(),
        .stdout = std.io.getStdOut().writer(),
    };
}

pub fn initWithPrefix(allocator: std.mem.Allocator, max_size: usize, prefix: u8) !Self {
    const self = try Self.init(allocator, max_size);
    self.prefix = prefix;
    return self;
}

pub fn addCommand(self: *Self, name: []const u8, description: []const u8, comptime run_func: Command.RunFn) !void {
    const command = Command{
        .name = name,
        .description = description,
        .run = run_func,
    };
    try self.commands.put(name, command);
}

pub fn deinit(self: *Self) void {
    self.allocator.free(self.buffer);
}

pub fn prompt(self: Self, message: []const u8) !?[]const u8 {
    _ = try self.stdout.write(message);
    var stream = std.io.fixedBufferStream(self.buffer);
    const writer = stream.writer();
    self.stdin.streamUntilDelimiter(writer, '\r', null) catch |err| {
        // EOF shouldn't be elevated to an error
        if (err == error.EndOfStream) {
            return null;
        }
        return err;
    };
    // ensure we strip whitespace
    const trimmed = std.mem.trim(u8, stream.getWritten(), "\r");

    if (trimmed.len < 1) {
        return null;
    } else if (trimmed.len > 1 and trimmed[0] == self.prefix) {
        const command = self.commands.get(trimmed[1..]) orelse return null;
        try command.run();
        return null;
    }
    return trimmed;
}

pub fn getStdOut(self: Self) std.fs.File.Writer {
    return self.stdout;
}
