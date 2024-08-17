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
    .{ "add", add },
    .{ "sum", add },
    .{ "sub", sub },
    .{ "mul", mul },
    .{ "div", div },
    .{ "eq", equals },
    .{ "lt", less },
    .{ "gt", greater },
    .{ "ne", notEqual },
    .{ "len", len },
    .{ "append", append },
    .{ "get", get },
    .{ "put", put },
    .{ "trim", trim },
    // operations (symbols)
    .{ "+", add },
    .{ "-", sub },
    .{ "*", mul },
    .{ "/", div },
    .{ "==", equals },
    .{ "<", less },
    .{ ">", greater },
    .{ "!=", notEqual },
});

pub fn lookup(str: []const u8) ?*const Builtin {
    return builtins_table.get(str);
}

fn say(args: []const *Value) !Result {
    const stdout = std.io.getStdOut().writer();

    var trailing_newline = false;
    var i: usize = 0;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        try stdout.print("{s}", .{arg});
        if (i + 1 < args.len) {
            try stdout.print(" ", .{});
        } else {
            trailing_newline = switch (arg.as) {
                .string => |s| s.len > 0 and s[s.len - 1] == '\n',
                else => false,
            };
        }
    }

    if (!trailing_newline) {
        try stdout.print("\n", .{});
    }
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
    if (args.len < 1) {
        unreachable;
    }
    return something(switch (args[0].as) {
        .integer => try addIntegers(args),
        .string => try concatenateStrings(args),
        else => unreachable,
    });
}

fn addIntegers(args: []const *Value) !*Value {
    var sum_value: i64 = 0;
    for (args) |arg| {
        switch (arg.as) {
            .integer => |i| {
                sum_value += i;
            },
            else => unreachable,
        }
    }
    return try gc.integer(sum_value);
}

fn concatenateStrings(args: []const *Value) !*Value {
    var result = std.ArrayList(u8).init(gc.backing_allocator);
    for (args) |arg| {
        switch (arg.as) {
            .string => |s| {
                try result.appendSlice(s);
            },
            else => unreachable,
        }
    }
    return try gc.allocedString(try result.toOwnedSlice());
}

fn sub(args: []const *Value) !Result {
    if (args.len < 2) unreachable;
    var diff_value: i64 = switch (args[0].as) {
        .integer => |i| i,
        else => unreachable,
    };
    for (args[1..]) |arg| {
        switch (arg.as) {
            .integer => |i| {
                diff_value -= i;
            },
            else => unreachable, //TODO: hmmm...
        }
    }
    return something(try gc.integer(diff_value));
}

fn mul(args: []const *Value) !Result {
    if (args.len < 2) unreachable;
    var product_value: i64 = switch (args[0].as) {
        .integer => |i| i,
        else => unreachable,
    };
    for (args[1..]) |arg| {
        switch (arg.as) {
            .integer => |i| {
                product_value *= i;
            },
            else => unreachable, //TODO: hmmm...
        }
    }
    return something(try gc.integer(product_value));
}

fn div(args: []const *Value) !Result {
    if (args.len < 2) unreachable;
    var quotient_value: i64 = switch (args[0].as) {
        .integer => |i| i,
        else => unreachable,
    };
    for (args[1..]) |arg| {
        switch (arg.as) {
            .integer => |i| {
                //TODO: handle division edge cases
                quotient_value = @divFloor(quotient_value, i);
            },
            else => unreachable, //TODO: hmmm...
        }
    }
    return something(try gc.integer(quotient_value));
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

fn less(args: []const *Value) !Result {
    if (args.len < 2) {
        unreachable;
    }

    const first = args[0];
    for (args[1..]) |arg| {
        //TODO: handle type error
        const order = first.compare(arg) catch unreachable;
        switch (order) {
            .less => {},
            else => return something(try gc.boolean(false)),
        }
    }
    return something(try gc.boolean(true));
}

fn greater(args: []const *Value) !Result {
    if (args.len < 2) {
        unreachable;
    }

    const first = args[0];
    for (args[1..]) |arg| {
        //TODO: handle type error
        const order = first.compare(arg) catch unreachable;
        switch (order) {
            .greater => {},
            else => return something(try gc.boolean(false)),
        }
    }
    return something(try gc.boolean(true));
}

fn notEqual(args: []const *Value) !Result {
    if (args.len < 2) {
        return something(try gc.boolean(true));
    }

    const first = args[0];
    for (args[1..]) |arg| {
        //TODO: handle type error
        const order = first.compare(arg) catch unreachable;
        switch (order) {
            .equal => return something(try gc.boolean(false)),
            else => {},
        }
    }
    return something(try gc.boolean(true));
}

fn len(args: []const *Value) !Result {
    var length: i64 = 0;
    for (args) |arg| {
        switch (arg.as) {
            .string => |s| length += @intCast(s.len),
            .integer, .float, .boolean => unreachable, //TODO: this
            .list => |l| length += @intCast(l.items.len),
            .record => |r| length += @intCast(r.count()),
        }
    }
    return something(try gc.integer(length));
}

fn append(args: []const *Value) !Result {
    if (args.len == 0) {
        unreachable;
    }

    switch (args[0].as) {
        .list => |*l| {
            for (args[1..]) |arg| {
                try l.append(arg);
            }
        },
        .integer, .boolean, .float, .string, .record => unreachable,
    }

    return something(args[0]);
}

fn get(args: []const *Value) !Result {
    if (args.len != 2) {
        unreachable;
    }

    const index = switch (args[1].as) {
        .integer => |i| i,
        .float, .boolean, .list, .string, .record => unreachable,
    };

    return switch (args[0].as) {
        //TODO: range check
        .list => |l| something(l.items[@intCast(index)]),
        //TODO: This should be possible
        .string => unreachable,
        .integer, .boolean, .float, .record => unreachable,
    };
}

fn put(args: []const *Value) !Result {
    if (args.len != 3) {
        unreachable;
    }

    const index = switch (args[1].as) {
        .integer => |i| i,
        .float, .boolean, .list, .string, .record => unreachable,
    };

    const value = args[2];

    switch (args[0].as) {
        //TODO: handle negative values properly
        .list => |l| {
            //TODO: range check
            l.items[@intCast(index)] = value;
        },
        //TODO: This should be possible
        .string => unreachable,
        .integer, .boolean, .float, .record => unreachable,
    }

    return something(args[0]);
}

//TODO this should be inside a module...
fn trim(args: []const *Value) !Result {
    if (args.len != 1) {
        unreachable;
    }

    const arg = args[0];
    const str = switch (arg.as) {
        .string => |s| s,
        else => unreachable,
    };

    if (str.len == 0) {
        return something(try gc.string(""));
    }

    var first_real: usize = 0;
    var last_real = str.len - 1;

    while (first_real < str.len) : (first_real += 1) {
        if (std.ascii.isWhitespace(str[first_real])) {
            break;
        }
    }

    while (last_real > first_real) : (last_real -= 1) {
        if (std.ascii.isWhitespace(str[last_real])) {
            break;
        }
    }

    return something(try gc.string(str[first_real..last_real]));
}
