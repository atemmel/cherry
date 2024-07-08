const std = @import("std");
const values = @import("value.zig");
const Value = values.Value;
const Result = values.Result;

const something = values.something;
const nothing = values.nothing;
const integer = values.integer;

pub const BuiltinError = error{
    AssertionFailed,
} || std.mem.Allocator.Error || std.fs.File.WriteError;

pub const Builtin = fn (args: []const Value) BuiltinError!Result;

const builtins_table = std.StaticStringMap(*const Builtin).initComptime(&.{
    // general
    .{ "assert", assert },
    .{ "say", say },
    // operations
    .{ "sum", add },
    .{ "+", add },
});

pub fn lookup(str: []const u8) ?*const Builtin {
    return builtins_table.get(str);
}

fn say(args: []const Value) !Result {
    const stdout = std.io.getStdOut().writer();
    for (args) |arg| {
        try stdout.print("{s} ", .{arg});
    }
    try stdout.print("\n", .{});
    return nothing;
}

fn assert(args: []const Value) !Result {
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
        true => nothing,
        false => BuiltinError.AssertionFailed,
    };
}

pub fn add(args: []const Value) !Result {
    var sum_value: i64 = 0;
    for (args) |arg| {
        switch (arg) {
            .integer => |i| {
                sum_value += i;
            },
            else => unreachable, //TODO: hmmm...
        }
    }
    return integer(sum_value);
}
