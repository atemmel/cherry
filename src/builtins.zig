const std = @import("std");
const Value = @import("symtable.zig").Value;

pub const BuiltinError = error{
    AssertionFailed,
} || std.mem.Allocator.Error || std.fs.File.WriteError;

const Result = union(enum) {
    value: Value,
    none: void,
};

pub const Builtin = fn (args: []const Value) BuiltinError!Result;

const builtins_table = std.StaticStringMap(*const Builtin).initComptime(&.{
    // general
    .{ "assert", assert },
    .{ "say", say },
    // operations
    .{ "+", sum },
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

fn sum(args: []const Value) !void {
    var sum_value: i64 = 0;
    for (args) |arg| {
        switch (arg) {
            .integer => |i| {
                sum_value += i;
            },
        }
    }
    return Value{};
}
