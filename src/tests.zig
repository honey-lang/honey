const std = @import("std");
pub const Parser = @import("parser/Parser.zig");

test {
    std.testing.refAllDecls(@This());
}
