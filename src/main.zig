const std = @import("std");

pub fn main() !void {}

comptime {
    const refAllDecls = std.testing.refAllDecls;

    refAllDecls(@import("lexing.zig"));
    refAllDecls(@import("parsing.zig"));
}
