const std = @import("std");
const Value = @import("symtable.zig").Value;

pub const BuiltinError = error{
    AssertionFailed,
} || std.mem.Allocator.Error || std.fs.File.WriteError;

pub const Builtin = fn (args: []const Value) BuiltinError!void;

const builtins_table = std.StaticStringMap(*const Builtin).initComptime(&.{
    .{
        "assert", assert,
        "say",    say,
    },
});

pub fn lookup(str: []const u8) ?*const Builtin {
    return builtins_table.get(str);
}

fn say(args: []const Value) !void {
    const stdout = std.io.getStdOut().writer();
    for (args) |arg| {
        try stdout.print("{s} ", .{arg});
    }
    try stdout.print("\n", .{});
}

fn assert(args: []const Value) !void {
    const stderr = std.io.getStdErr().writer();
    var all_passed = true;
    for (args, 0..) |arg, idx| {
        switch (arg) {
            .boolean => |b| {
                if (!b) {
                    try stderr.print("Assertion failed for value {}\n", .{idx});
                    all_passed = false;
                }
            },
            else => unreachable,
        }
    }

    return switch (all_passed) {
        true => {},
        false => BuiltinError.AssertionFailed,
    };
}
