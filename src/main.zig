const std = @import("std");

pub fn main() !void {}

comptime {
    std.testing.refAllDecls(@import("lexing.zig"));
}
