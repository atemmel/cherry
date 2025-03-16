const std = @import("std");
const ast = @import("ast.zig");
const values = @import("value.zig");
const gc = @import("gc.zig");
const interpreter = @import("interpreter.zig");
const semantics = @import("semantics.zig");
const pipeline = @import("pipeline.zig");

const Signature = ast.Signature;
const TypeInfo = semantics.TypeInfo;

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

pub const BuiltinFn = fn (ctx: *State, args: []const *Value, call: ast.Call) BuiltinError!Result;

pub const BuiltinInfo = struct {
    func: *const BuiltinFn,
    signature: Signature,
};

pub const BuiltinsTable = std.StaticStringMap(BuiltinInfo);

fn unchecked(func: *const BuiltinFn) BuiltinInfo {
    return .{
        .func = func,
        .signature = .{
            .last_parameter_is_variadic = true,
            .parameters = &.{
                .{
                    .name = "args",
                    .param_type = .{
                        .type_info = .something,
                    },
                },
            },
            .produces = .something,
        },
    };
}

const builtins_table = BuiltinsTable.initComptime(
    &.{
        // general
        .{ "assert", assert_info },
        .{ "say", say_info },
        .{ "alias", alias_info },
        .{ "cd", cd_info },
        .{ "export", export_info },
        // collections
        .{ "append", append_info },
        .{ "get", get_info },
        .{ "len", unchecked(len) },
        .{ "put", unchecked(put) },
        .{ "del", unchecked(del) },
        .{ "slice", unchecked(slice) },
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
        .{ "and", unchecked(andFn) },
        .{ "or", unchecked(orFn) },
        // operations (symbols)
        .{ "+", unchecked(add) },
        .{ "-", unchecked(sub) },
        .{ "*", unchecked(mul) },
        .{ "/", unchecked(div) },
        .{ "<", unchecked(less) },
        .{ ">", unchecked(greater) },
        // types
        .{ "int", unchecked(int) },
        // dev
        .{ "vardump", unchecked(vardump) },
        .{ "gcdump", unchecked(gcdump) },
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
            .{
                .name = "args",
                .param_type = .{
                    .type_info = .something,
                },
            },
        },
    },
};

fn say(_: *State, args: []const *Value, _: ast.Call) !Result {
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
            .{
                .name = "args",
                .param_type = .{
                    .type_info = .boolean,
                },
            },
        },
    },
};

fn assert(state: *State, args: []const *Value, call: ast.Call) !Result {
    const stderr = std.io.getStdErr().writer();
    for (args, 0..) |arg, idx| {
        switch (arg.as) {
            .boolean => |b| {
                if (!b) {
                    try stderr.print("Assertion failed for value {}\n", .{idx});
                    const expr = call.arguments[idx];
                    const token = ast.tokenFromExpr(expr);

                    state.error_report = .{
                        .msg = try std.fmt.allocPrint(state.scratch_arena.allocator(), "Assertion failed", .{}),
                        .offending_token = token,
                        .trailing = false,
                    };
                    return BuiltinError.AssertionFailed;
                }
            },
            else => {
                const expr = call.arguments[idx];
                const token = ast.tokenFromExpr(expr);
                state.error_report = .{
                    .msg = try std.fmt.allocPrint(state.scratch_arena.allocator(), "Non boolean value given to 'assert': {}", .{arg}),
                    .offending_token = token,
                    .trailing = false,
                };
                return BuiltinError.AssertionFailed;
            },
        }
    }

    return nothing;
}

const alias_info: BuiltinInfo = .{
    .func = alias,
    .signature = .{
        // takes two strings
        .parameters = &.{
            .{
                .name = "name",
                .param_type = .{
                    .type_info = .string,
                },
            },
            .{
                .name = "command",
                .param_type = .{
                    .type_info = .string,
                },
            },
        },
    },
};

fn alias(state: *State, args: []const *Value, _: ast.Call) !Result {
    try validateArgsCount(state, &.{2}, args.len);

    const alias_from = switch (args[0].as) {
        .string => |s| s,
        else => unreachable,
    };
    const alias_to = switch (args[1].as) {
        .string => |s| s,
        else => unreachable,
    };

    try gc.aliases.append(.{
        .from = alias_from,
        .to = alias_to,
    });

    return nothing;
}

const cd_info: BuiltinInfo = .{
    .func = cd,
    .signature = .{
        .parameters = &.{ // takes one string
            .{
                .name = "path",
                .param_type = .{
                    .type_info = .string,
                },
            },
        },
    },
};

fn cd(state: *State, args: []const *Value, _: ast.Call) !Result {
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

fn add(state: *State, args: []const *Value, call: ast.Call) !Result {
    if (args.len < 1) {
        unreachable;
    }
    return something(switch (args[0].as) {
        .integer => try addIntegers(state, args, call),
        .string => try concatenateStrings(state, args, call),
        else => unreachable,
    });
}

fn addIntegers(state: *State, args: []const *Value, call: ast.Call) !*Value {
    var sum_value: i64 = 0;
    for (args) |arg| {
        switch (arg.as) {
            .integer => |i| {
                sum_value += i;
            },
            else => unreachable,
        }
    }
    const opt = gc.ValueOptions{
        .origin = call.token,
        .origin_module = state.current_module_in_process,
    };

    const i = try gc.integer(sum_value, opt);
    return (i);
}

fn concatenateStrings(state: *State, args: []const *Value, call: ast.Call) !*Value {
    var result = std.ArrayList(u8).init(gc.allocator());
    for (args) |arg| {
        switch (arg.as) {
            .string => |s| {
                try result.appendSlice(s);
            },
            else => unreachable,
        }
    }
    const opt = gc.ValueOptions{
        .origin = call.token,
        .origin_module = state.current_module_in_process,
    };
    return try gc.allocedString(try result.toOwnedSlice(), opt);
}

fn sub(state: *State, args: []const *Value, call: ast.Call) !Result {
    var diff_value: i64 = switch (args[0].as) {
        .integer => |i| i,
        .string => return typeMismatchError(state, "int", "string", 0),
        .list => return typeMismatchError(state, "int", "list", 0),
        .float => return typeMismatchError(state, "int", "float", 0),
        .boolean => return typeMismatchError(state, "int", "bool", 0),
        .record => return typeMismatchError(state, "int", "record", 0),
    };
    for (args[1..]) |arg| {
        switch (arg.as) {
            .integer => |i| {
                diff_value -= i;
            },
            .string => return typeMismatchError(state, "int", "string", 1),
            .list => return typeMismatchError(state, "int", "list", 1),
            .float => return typeMismatchError(state, "int", "float", 1),
            .boolean => return typeMismatchError(state, "int", "bool", 1),
            .record => return typeMismatchError(state, "int", "record", 1),
        }
    }

    const opt = gc.ValueOptions{
        .origin = call.token,
        .origin_module = state.current_module_in_process,
    };

    const i = try gc.integer(diff_value, opt);
    return something(i);
}

fn mul(state: *State, args: []const *Value, call: ast.Call) !Result {
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
    const opt = gc.ValueOptions{
        .origin = call.token,
        .origin_module = state.current_module_in_process,
    };
    const i = try gc.integer(product_value, opt);
    return something(i);
}

fn div(state: *State, args: []const *Value, call: ast.Call) !Result {
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
    const opt = gc.ValueOptions{
        .origin = call.token,
        .origin_module = state.current_module_in_process,
    };
    const i = try gc.integer(quotient_value, opt);
    return something(i);
}

fn equals(state: *State, args: []const *Value, call: ast.Call) !Result {
    const opt = gc.ValueOptions{
        .origin = call.token,
        .origin_module = state.current_module_in_process,
    };
    if (args.len == 0) {
        return something(try gc.boolean(true, opt));
    }

    const first = args[0];
    for (args[1..], 1..) |arg, idx| {
        //TODO: handle type error
        const order = first.compare(arg) catch unreachable;
        switch (order) {
            .equal => {},
            .failure => |fail| return typeMismatchError(state, fail.wants, fail.got, idx),
            else => return something(try gc.boolean(false, opt)),
        }
    }
    return something(try gc.boolean(true, opt));
}

fn less(state: *State, args: []const *Value, call: ast.Call) !Result {
    const opt = gc.ValueOptions{
        .origin = call.token,
        .origin_module = state.current_module_in_process,
    };
    if (args.len < 2) {
        unreachable;
    }

    const first = args[0];
    for (args[1..], 1..) |arg, idx| {
        //TODO: handle type error
        const order = first.compare(arg) catch unreachable;
        switch (order) {
            .less => {},
            .failure => |fail| return typeMismatchError(state, fail.wants, fail.got, idx),
            else => return something(try gc.boolean(false, opt)),
        }
    }
    return something(try gc.boolean(true, opt));
}

fn greater(state: *State, args: []const *Value, call: ast.Call) !Result {
    const opt = gc.ValueOptions{
        .origin = call.token,
        .origin_module = state.current_module_in_process,
    };
    if (args.len < 2) {
        unreachable;
    }

    const first = args[0];
    for (args[1..], 1..) |arg, idx| {
        //TODO: handle type error
        const order = first.compare(arg) catch unreachable;
        switch (order) {
            .greater => {},
            .failure => |fail| return typeMismatchError(state, fail.wants, fail.got, idx),
            else => return something(try gc.boolean(false, opt)),
        }
    }
    return something(try gc.boolean(true, opt));
}

fn notEqual(state: *State, args: []const *Value, call: ast.Call) !Result {
    const opt = gc.ValueOptions{
        .origin = call.token,
        .origin_module = state.current_module_in_process,
    };
    if (args.len < 2) {
        return something(try gc.boolean(true, opt));
    }

    const first = args[0];
    for (args[1..], 1..) |arg, idx| {
        //TODO: handle type error
        const order = first.compare(arg) catch unreachable;
        switch (order) {
            .equal => return something(try gc.boolean(false, opt)),
            .failure => |fail| return typeMismatchError(state, fail.wants, fail.got, idx),
            else => {},
        }
    }
    return something(try gc.boolean(true, opt));
}

fn andFn(state: *State, args: []const *Value, call: ast.Call) !Result {
    const opt = gc.ValueOptions{
        .origin = call.token,
        .origin_module = state.current_module_in_process,
    };
    for (args, 0..) |arg, idx| {
        //TODO: handle type error
        const boolean = switch (arg.as) {
            .boolean => |b| b,
            .list => return typeMismatchError(state, "bool", "list", idx),
            .float => return typeMismatchError(state, "bool", "float", idx),
            .string => return typeMismatchError(state, "bool", "string", idx),
            .record => return typeMismatchError(state, "bool", "record", idx),
            .integer => return typeMismatchError(state, "bool", "int", idx),
        };
        if (!boolean) {
            return something(try gc.boolean(false, opt));
        }
    }
    return something(try gc.boolean(true, opt));
}

fn orFn(state: *State, args: []const *Value, call: ast.Call) !Result {
    const opt = gc.ValueOptions{
        .origin = call.token,
        .origin_module = state.current_module_in_process,
    };

    for (args, 0..) |arg, idx| {
        //TODO: handle type error
        const boolean = switch (arg.as) {
            .boolean => |b| b,
            .list => return typeMismatchError(state, "bool", "list", idx),
            .float => return typeMismatchError(state, "bool", "float", idx),
            .string => return typeMismatchError(state, "bool", "string", idx),
            .record => return typeMismatchError(state, "bool", "record", idx),
            .integer => return typeMismatchError(state, "bool", "int", idx),
        };
        if (boolean) {
            return something(try gc.boolean(true, opt));
        }
    }
    return something(try gc.boolean(false, opt));
}

fn len(state: *State, args: []const *Value, call: ast.Call) !Result {
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
    const opt = gc.ValueOptions{
        .origin = call.token,
        .origin_module = state.current_module_in_process,
    };
    const i = try gc.integer(length, opt);
    return something(i);
}

const append_info_list_of: TypeInfo = .{
    .generic = "T",
};

const append_info: BuiltinInfo = .{
    .func = append,
    .signature = .{
        // takes one list of generic content, plus zero or more arguments of generic type
        .generics = &.{"T"},
        .last_parameter_is_variadic = true,
        .parameters = &.{
            .{
                .name = "list",
                .param_type = .{
                    .type_info = .{
                        .list = .{
                            .of = &append_info_list_of,
                        },
                    },
                },
            },
            .{
                .name = "value",
                .param_type = .{
                    .type_info = append_info_list_of,
                },
            },
        },
    },
};

fn append(_: *State, args: []const *Value, _: ast.Call) !Result {
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

const get_info_list_of: TypeInfo = .{
    .generic = "T",
};

const get_info: BuiltinInfo = .{
    .func = get,
    .signature = .{
        // takes one list of generic content, plus one indexing argument
        .generics = &.{"T"},
        .parameters = &.{
            .{
                .name = "list",
                .param_type = .{
                    .type_info = .{
                        .list = .{
                            .of = &get_info_list_of,
                        },
                    },
                },
            },
            .{
                .name = "index",
                .param_type = .{
                    .type_info = .integer,
                },
            },
        },
    },
};

fn get(state: *State, args: []const *Value, call: ast.Call) !Result {
    try validateArgsCount(state, &.{2}, args.len);

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
                    const opt = gc.ValueOptions{
                        .origin = call.token,
                        .origin_module = state.current_module_in_process,
                    };
                    return something(try gc.string(str, opt));
                }
                i += 1;
            }
            unreachable;
        },
        .integer, .boolean, .float, .record => unreachable,
    };
}

fn put(state: *State, args: []const *Value, _: ast.Call) !Result {
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

fn del(state: *State, args: []const *Value, call: ast.Call) !Result {
    _ = call;
    try validateArgsCount(state, &.{2}, args.len);

    switch (args[0].as) {
        //TODO: handle negative values properly
        .list => |*l| delList(l, args[1]),
        //TODO: This should be possible
        .string => unreachable,
        .integer, .boolean, .float, .record => unreachable,
    }
    return nothing;
}

fn delList(list: *std.ArrayList(*Value), idx: *Value) void {
    const index = switch (idx.as) {
        .integer => |i| i,
        .float, .boolean, .list, .string, .record => unreachable,
    };
    //TODO: range check
    //TODO: handle negative values properly
    _ = list.orderedRemove(@intCast(index));
}

fn slice(state: *State, args: []const *Value, call: ast.Call) !Result {
    try validateArgsCount(state, &.{ 1, 2, 3 }, args.len);
    return switch (args[0].as) {
        .list => |list| sliceList(state, args, list, call),
        .string => |string| sliceString(state, args, string, call),
        .boolean => return typeMismatchError(state, "list or string", "bool", 0),
        .float => return typeMismatchError(state, "list or string", "float", 0),
        .integer => return typeMismatchError(state, "list or string", "int", 0),
        .record => return typeMismatchError(state, "list or string", "record", 0),
    };
}

fn sliceList(state: *State, args: []const *Value, list: values.List, call: ast.Call) !Result {
    var from_idx: usize = 0;
    var to_idx: usize = @intCast(list.items.len);

    if (args.len > 1) {
        //TODO: more error checks here
        from_idx = @intCast(@max(0, try getInt(state, args, 1)));
    }

    if (args.len > 2) {
        //TODO: more error checks here
        to_idx = @intCast(@max(0, try getInt(state, args, 2)));
    }

    if (from_idx > to_idx) {
        unreachable; //TODO: error message
    }

    var new_list = try std.ArrayList(*Value).initCapacity(gc.allocator(), to_idx - from_idx);
    for (list.items[from_idx..to_idx]) |val| {
        try new_list.append(try gc.cloneOrReference(val));
    }

    const opt = gc.ValueOptions{
        .origin = call.token,
        .origin_module = state.current_module_in_process,
    };
    return something(try gc.list(new_list, opt));
}

fn sliceString(state: *State, args: []const *Value, string: []const u8, call: ast.Call) !Result {
    var from_idx: usize = 0;
    var to_idx: usize = @intCast(string.len);

    if (args.len > 1) {
        //TODO: more error checks here
        from_idx = @intCast(@max(0, try getInt(state, args, 1)));
    }

    if (args.len > 2) {
        //TODO: more error checks here
        to_idx = @intCast(@max(0, try getInt(state, args, 2)));
    }

    if (from_idx > to_idx) {
        unreachable; //TODO: error message
    }

    const opt = gc.ValueOptions{
        .origin = call.token,
        .origin_module = state.current_module_in_process,
    };
    const new_string = try gc.string(string[from_idx..to_idx], opt);
    return something(new_string);
}

fn getInt(state: *State, args: []const *Value, idx: usize) !i64 {
    std.debug.assert(idx < args.len);
    return switch (args[idx].as) {
        .integer => |integer| integer,
        .boolean => typeMismatchError(state, "int", "bool", idx),
        .float => typeMismatchError(state, "int", "float", idx),
        .list => typeMismatchError(state, "int", "list", idx),
        .record => typeMismatchError(state, "int", "record", idx),
        .string => typeMismatchError(state, "int", "string", idx),
    };
}

const export_info: BuiltinInfo = .{
    .func = envExport,
    .signature = .{
        .parameters = &.{ // takes two strings
            .{
                .name = "name",
                .param_type = .{
                    .type_info = .string,
                },
            },
            .{
                .name = "value",
                .param_type = .{
                    .type_info = .string,
                },
            },
        },
    },
};

fn envExport(state: *State, args: []const *Value, _: ast.Call) !Result {
    try validateArgsCount(state, &.{2}, args.len);
    const ally = gc.allocator();

    const name = switch (args[0].as) {
        .string => |string| try ally.dupe(u8, string),
        .record => return typeMismatchError(state, "string", "record", 0),
        .list => return typeMismatchError(state, "string", "list", 0),
        .float => return typeMismatchError(state, "string", "float", 0),
        .boolean => return typeMismatchError(state, "string", "boolean", 0),
        .integer => return typeMismatchError(state, "string", "integer", 0),
    };

    errdefer ally.free(name);

    const value = switch (args[1].as) {
        .string, .boolean, .integer => try args[1].asStr(ally),
        .record => return typeMismatchError(state, "string, boolean or integer", "record", 0),
        .list => return typeMismatchError(state, "string, boolean or integer", "list", 0),
        .float => return typeMismatchError(state, "string, boolean or integer", "float", 0),
    };

    errdefer ally.free(value);

    try state.env_map.putMove(name, value);

    return nothing;
}

fn int(state: *State, args: []const *Value, call: ast.Call) !Result {
    try validateArgsCount(state, &.{1}, args.len);

    const arg = args[0];
    const ok_string = "string, int, bool or float";

    const int_result = arg.asInt() catch |e| {
        return switch (e) {
            error.Overflow => unreachable,
            error.InvalidCharacter => unreachable,
            error.InvalidIntConversion => switch (arg.as) {
                .string, .float, .integer, .boolean => unreachable, // ok, this can never happen
                .record => return typeMismatchError(state, ok_string, "record", 0),
                .list => return typeMismatchError(state, ok_string, "list", 0),
            },
        };
    };

    const opt = gc.ValueOptions{
        .origin = call.token,
        .origin_module = state.current_module_in_process,
    };

    //return something(try gc.integer(int_result, opt));
    const integ = (try gc.integer(int_result, opt));
    return something(integ);
}

fn vardump(_: *State, args: []const *Value, _: ast.Call) !Result {
    var show_root_values = false;
    if (args.len > 0) {
        switch (args[0].as) {
            .boolean => |b| show_root_values = b,
            else => {},
        }
    }
    gc.varDump(show_root_values);
    return nothing;
}

fn gcdump(_: *State, _: []const *Value, _: ast.Call) !Result {
    gc.dump();
    return nothing;
}

fn exit(state: *State, args: []const *Value, call: ast.Call) !Result {
    _ = call; // autofix
    validateArgsCount(state, &.{1}, args.len);

    const code = switch (args[0].as) {
        .integer => |integer| integer,
        else => {
            return typeMismatchError(state, "int", "other", 0);
        },
    };
    std.process.exit(code);
    return nothing;
}

pub fn validateArgsCount(
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
        .msg = try std.fmt.allocPrint(state.scratch_arena.allocator(), "Function expects {any} args, recieved {}", .{ accepted_counts, actual_count }),
        .offending_token = undefined, // Ok, caller will set the value
        .trailing = false,
    };
    return InterpreterError.ArgsCountMismatch;
}

const TypeMismatchError = InterpreterError || std.mem.Allocator.Error;

pub fn typeMismatchError(state: *State, wants: []const u8, got: []const u8, offending_value_idx: usize) TypeMismatchError {
    state.error_report = try semantics.typeMismatchReportIdx(state.scratch_arena.allocator(), wants, got, offending_value_idx);
    return error.TypeMismatch;
}
