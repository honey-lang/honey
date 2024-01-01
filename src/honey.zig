const std = @import("std");
const Lexer = @import("Lexer.zig");
const ast = @import("ast.zig");
const Parser = @import("Parser.zig");
const Evaluator = @import("Evaluator.zig");

pub const version = "0.0.1";

pub fn tokenize(input: []const u8, allocator: std.mem.Allocator) ![]const Lexer.TokenData {
    var lexer = Lexer.init(input, allocator);
    _ = try lexer.readAll();
    return try lexer.tokens.toOwnedSlice();
}

pub fn run(input: []const u8, allocator: std.mem.Allocator, environment: *Evaluator.Environment) !?Evaluator.Value {
    const tokens = try tokenize(input, allocator);
    defer allocator.free(tokens);
    var parser = Parser.init(tokens, allocator);
    defer parser.deinit();
    var program = try parser.parse();
    defer program.deinit();
    var evaluator = Evaluator.init(allocator, environment);
    defer evaluator.deinit();
    return evaluator.run(program);
}
