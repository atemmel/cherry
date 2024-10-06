const std = @import("std");
const values = @import("value.zig");
const gc = @import("gc.zig");
const interpreter = @import("interpreter.zig");
const pipeline = @import("pipeline.zig");

const Value = values.Value;
const Type = values.Type;
const Result = values.Result;
const State = pipeline.State;
const InterpreterError = interpreter.InterpreterError;

const something = values.something;
const nothing = values.nothing;

pub const BuiltinError = error{
    AssertionFailed,
} || std.mem.Allocator.Error || std.fs.File.WriteError || InterpreterError;

pub const Builtin = fn (ctx: *State, args: []const *Value) BuiltinError!Result;

const builtins_table = std.StaticStringMap(*const Builtin).initComptime(
    &.{
        // general
        .{ "assert", assert },
        .{ "say", say },
        // collections
        .{ "append", append },
        .{ "get", get },
        .{ "len", len },
        .{ "put", put },
        .{ "trim", trim },
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
        // operations (symbols)
        .{ "+", add },
        .{ "-", sub },
        .{ "*", mul },
        .{ "/", div },
        .{ "==", equals },
        .{ "<", less },
        .{ ">", greater },
        .{ "!=", notEqual },
    },
);

pub fn lookup(str: []const u8) ?*const Builtin {
    return builtins_table.get(str);
}

fn say(_: *State, args: []const *Value) !Result {
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

fn assert(_: *State, args: []const *Value) !Result {
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

fn add(state: *State, args: []const *Value) !Result {
    if (args.len < 1) {
        unreachable;
    }
    return something(switch (args[0].as) {
        .integer => try addIntegers(state, args),
        .string => try concatenateStrings(state, args),
        else => unreachable,
    });
}

fn addIntegers(_: *State, args: []const *Value) !*Value {
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

fn concatenateStrings(_: *State, args: []const *Value) !*Value {
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

fn sub(_: *State, args: []const *Value) !Result {
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

fn mul(_: *State, args: []const *Value) !Result {
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

fn div(_: *State, args: []const *Value) !Result {
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

fn equals(_: *State, args: []const *Value) !Result {
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

fn less(_: *State, args: []const *Value) !Result {
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

fn greater(_: *State, args: []const *Value) !Result {
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

fn notEqual(_: *State, args: []const *Value) !Result {
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

fn len(_: *State, args: []const *Value) !Result {
    var length: i64 = 0;
    for (args) |arg| {
        switch (arg.as) {
            .string => |s| {
                length += std.unicode.utf8CountCodepoints(s);
            },
            .integer, .float, .boolean => unreachable, //TODO: this
            .list => |l| length += @intCast(l.items.len),
            .record => |r| length += @intCast(r.count()),
        }
    }
    return something(try gc.integer(length));
}

fn append(_: *State, args: []const *Value) !Result {
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

fn get(state: *State, args: []const *Value) !Result {
    try validateArgsCount(state, &.{ 2, 3 }, args.len);

    const index = switch (args[1].as) {
        .integer => |i| i,
        .float, .boolean, .list, .string, .record => unreachable,
    };

    return switch (args[0].as) {
        //TODO: range check
        .list => |l| something(l.items[@intCast(index)]),
        //TODO: This should be possible
        .string => unreachable, //something(try gc.string(s[
        .integer, .boolean, .float, .record => unreachable,
    };
}

fn slice(_: *State, list_or_string: *Value, from: u64, to: u64) !Result {
    _ = list_or_string; // autofix
    _ = from; // autofix
    _ = to; // autofix
}

fn put(state: *State, args: []const *Value) !Result {
    try validateArgsCount(state, &.{3}, args.len);

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
fn trim(state: *State, args: []const *Value) !Result {
    try validateArgsCount(state, &.{1}, args.len);

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
        if (!std.ascii.isWhitespace(str[first_real])) {
            break;
        }
    }

    while (last_real > first_real) : (last_real -= 1) {
        if (!std.ascii.isWhitespace(str[last_real])) {
            break;
        }
    }

    return something(try gc.string(str[first_real..last_real]));
}

fn validateArgsCount(
    state: *State,
    accepted_counts: []const usize,
    actual_count: usize,
) !void {
    for (accepted_counts) |count| {
        if (count == actual_count) {
            return;
        }
    }
    state.error_report = .{
        .msg = try std.fmt.allocPrint(state.arena, "Function expects {any} args, recieved {}", .{ accepted_counts, actual_count }),
        .offending_token = undefined,
        .trailing = false,
    };
    return InterpreterError.ArgsCountMismatch;
}
