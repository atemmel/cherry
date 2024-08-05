const std = @import("std");
const values = @import("value.zig");
const gc = @import("gc.zig");
const Value = values.Value;
const Result = values.Result;

const something = values.something;
const nothing = values.nothing;

pub const BuiltinError = error{
    AssertionFailed,
} || std.mem.Allocator.Error || std.fs.File.WriteError;

pub const Builtin = fn (args: []const *Value) BuiltinError!Result;

const builtins_table = std.StaticStringMap(*const Builtin).initComptime(&.{
    // general
    .{ "assert", assert },
    .{ "say", say },
    // operations
    .{ "sum", add },
    .{ "+", add },
    .{ "eq", equals },
    .{ "==", equals },
    .{ "len", len },
});

pub fn lookup(str: []const u8) ?*const Builtin {
    return builtins_table.get(str);
}

fn say(args: []const *Value) !Result {
    const stdout = std.io.getStdOut().writer();
    for (args) |arg| {
        try stdout.print("{s} ", .{arg});
    }
    try stdout.print("\n", .{});
    return nothing;
}

fn assert(args: []const *Value) !Result {
    const stderr = std.io.getStdErr().writer();
    var all_passed = true;
    for (args, 0..) |arg, idx| {
        switch (arg.as) {
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

fn add(args: []const *Value) !Result {
    var sum_value: i64 = 0;
    for (args) |arg| {
        switch (arg.as) {
            .integer => |i| {
                sum_value += i;
            },
            else => unreachable, //TODO: hmmm...
        }
    }
    return something(try gc.integer(sum_value));
}

fn equals(args: []const *Value) !Result {
    if (args.len == 0) {
        return something(try gc.boolean(true));
    }

    const first = args[0];
    for (args[1..]) |arg| {
        //TODO: handle type error
        const order = first.compare(arg) catch unreachable;
        switch (order) {
            .equal => {},
            else => return something(try gc.boolean(false)),
        }
    }
    return something(try gc.boolean(true));
}

fn len(args: []const *Value) !Result {
    var sum: i64 = 0;
    for (args) |arg| {
        switch (arg.as) {
            .string => |s| sum += @intCast(s.len),
            .integer, .float, .boolean => unreachable, //TODO: this
        }
    }
    return something(try gc.integer(sum));
}