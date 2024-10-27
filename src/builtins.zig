const std = @import("std");
const values = @import("value.zig");
const gc = @import("gc.zig");
const interpreter = @import("interpreter.zig");
const semantics = @import("semantics.zig");
const pipeline = @import("pipeline.zig");
const symtable = @import("symtable.zig");

const TypeInfo = semantics.TypeInfo;
const Signature = semantics.Signature;

const Value = values.Value;
const Result = values.Result;
const State = pipeline.State;
const InterpreterError = interpreter.InterpreterError;

const something = values.something;
const nothing = values.nothing;

pub const Utf8Error = error{
    Utf8ExpectedContinuation,
    Utf8OverlongEncoding,
    Utf8EncodesSurrogateHalf,
    Utf8CodepointTooLarge,
    Utf8InvalidStartByte,
    TruncatedInput,
};

pub const BuiltinError = error{
    AssertionFailed,
} || std.mem.Allocator.Error || std.fs.File.WriteError || InterpreterError || Utf8Error;

pub const BuiltinFn = fn (ctx: *State, args: []const *Value) BuiltinError!Result;

pub const BuiltinInfo = struct {
    func: *const BuiltinFn,
    signature: Signature,
};

fn unchecked(func: *const BuiltinFn) BuiltinInfo {
    return .{
        .func = func,
        .signature = .{
            .last_parameter_is_variadic = true,
            .parameters = &.{
                .{ .something = {} },
            },
        },
    };
}

const builtins_table = std.StaticStringMap(BuiltinInfo).initComptime(
    &.{
        // general
        .{ "assert", assert_info },
        .{ "say", say_info },
        .{ "alias", alias_info },
        .{ "cd", cd_info },
        // collections
        .{ "append", unchecked(append) },
        .{ "get", unchecked(get) },
        .{ "len", unchecked(len) },
        .{ "put", unchecked(put) },
        // operations
        .{ "add", unchecked(add) },
        .{ "sum", unchecked(add) },
        .{ "sub", unchecked(sub) },
        .{ "mul", unchecked(mul) },
        .{ "div", unchecked(div) },
        .{ "eq", unchecked(equals) },
        .{ "lt", unchecked(less) },
        .{ "gt", unchecked(greater) },
        .{ "ne", unchecked(notEqual) },
        // operations (symbols)
        .{ "+", unchecked(add) },
        .{ "-", unchecked(sub) },
        .{ "*", unchecked(mul) },
        .{ "/", unchecked(div) },
        .{ "==", unchecked(equals) },
        .{ "<", unchecked(less) },
        .{ ">", unchecked(greater) },
        .{ "!=", unchecked(notEqual) },
    },
);

pub fn lookup(str: []const u8) ?BuiltinInfo {
    return builtins_table.get(str);
}

const say_info: BuiltinInfo = .{
    .func = say,
    .signature = .{
        // takes any number of any argument
        .last_parameter_is_variadic = true,
        .parameters = &.{
            .{ .something = {} },
        },
    },
};

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

const assert_info: BuiltinInfo = .{
    .func = assert,
    .signature = .{
        // takes any number of booleans
        .last_parameter_is_variadic = true,
        .parameters = &.{
            .{ .boolean = {} },
        },
    },
};

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

const alias_info: BuiltinInfo = .{
    .func = alias,
    .signature = .{
        // takes two strings
        .parameters = &.{
            .{ .string = {} },
            .{ .string = {} },
        },
    },
};

fn alias(state: *State, args: []const *Value) !Result {
    try validateArgsCount(state, &.{2}, args.len);

    const alias_from = switch (args[0].as) {
        .string => |s| s,
        else => unreachable,
    };
    const alias_to = switch (args[1].as) {
        .string => |s| s,
        else => unreachable,
    };

    try symtable.aliases.append(.{
        .from = alias_from,
        .to = alias_to,
    });

    return nothing;
}

const cd_info: BuiltinInfo = .{
    .func = cd,
    .signature = .{
        .parameters = &.{ // takes one string
            .{ .string = {} },
        },
    },
};

fn cd(state: *State, args: []const *Value) !Result {
    try validateArgsCount(state, &.{1}, args.len);
    const path = switch (args[0].as) {
        .string => |str| str,
        else => {
            return typeMismatchError(state, "string", "int", 0);
        },
    };
    var dir = std.fs.cwd().openDir(path, .{}) catch |err| {
        const err_str = switch (err) {
            error.FileNotFound => "No such directory",
            error.AccessDenied => "Access denied",
            error.NotDir => "Not a directory",
            error.DeviceBusy => "Device is busy",
            else => @errorName(err),
        };
        std.debug.print("{s}: {s}\n", .{ path, err_str });
        return nothing;
    };
    defer dir.close();
    dir.setAsCwd() catch {};
    return nothing;
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
                length += @intCast(std.unicode.utf8CountCodepoints(s) catch |err| {
                    return switch (err) {
                        error.Utf8OverlongEncoding => Utf8Error.Utf8OverlongEncoding,
                        error.Utf8EncodesSurrogateHalf => Utf8Error.Utf8EncodesSurrogateHalf,
                        error.Utf8ExpectedContinuation => Utf8Error.Utf8ExpectedContinuation,
                        error.Utf8CodepointTooLarge => Utf8Error.Utf8CodepointTooLarge,
                        error.Utf8InvalidStartByte => Utf8Error.Utf8InvalidStartByte,
                        error.TruncatedInput => Utf8Error.TruncatedInput,
                    };
                });
            },
            .integer, .float, .boolean => unreachable, //TODO: this
            .list => |l| length += @intCast(l.items.len),
            .record => |r| length += @intCast(r.count()),
        }
    }
    return something(try gc.integer(length));
}

const append_info_list_of: TypeInfo = .{
    .generic = "T",
};

const append_info: BuiltinInfo = .{
    .func = append,
    .signature = .{
        // takes one list of generic content, plus zero or more arguments of generic type
        .generics = &.{"T"},
        .parameters = &.{
            .{
                .list = .{
                    .of = &append_info_list_of,
                },
            },
        },
    },
};

const wip_append_info: BuiltinInfo = .{
    .func = append,
    .signature = .{
        .last_parameter_is_variadic = true,
        .parameters = &.{
            .{ .something = {} },
        },
    },
};

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
        .string => |s| {
            const view = std.unicode.Utf8View.initUnchecked(s);
            var it = view.iterator();
            var i: i64 = 0;
            while (it.nextCodepointSlice()) |str| {
                if (i == index) {
                    return something(try gc.string(str));
                }
                i += 1;
            }
            unreachable;
        },
        .integer, .boolean, .float, .record => unreachable,
    };
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
        .offending_token = undefined, // Ok, caller will set the value
        .trailing = false,
    };
    return InterpreterError.ArgsCountMismatch;
}

const TypeMismatchError = InterpreterError || std.mem.Allocator.Error;

fn typeMismatchError(state: *State, wants: []const u8, got: []const u8, offending_value_idx: usize) TypeMismatchError {
    state.error_report = try semantics.typeMismatchReportIdx(state.arena, wants, got, offending_value_idx);
    return error.TypeMismatch;
}
