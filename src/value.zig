const std = @import("std");
const builtin = @import("builtin");
const gc = @import("gc.zig");
const interpreter = @import("interpreter.zig");
const tokens = @import("tokens.zig");
const pipeline = @import("pipeline.zig");
const ast = @import("ast.zig");

const PipelineState = pipeline.State;
const InterpreterError = interpreter.InterpreterError;

const indexOfPos = std.mem.indexOfPos;

pub const List = std.array_list.Managed(*Value);
pub const Record = std.StringArrayHashMap(*Value);
pub const Closure = struct {
    pub const Upvalues = std.array_list.Managed(*Value);

    ast: ast.Closure,
    upvalues: Upvalues,

    pub fn deinit(c: *Closure) void {
        c.upvalues.deinit();
    }
};

// types used by the interpreter
pub const Type = enum {
    string,
    integer,
    float,
    boolean,
    list,
    record,
    closure,
};

pub const Value = struct {
    as: union(Type) {
        // strings are immutable
        string: []const u8, //TODO: this should perhaps be u32 so as to be unicode-compatible
        // integer type
        integer: i64,
        float: f64,
        boolean: bool,
        list: List,
        record: Record,
        closure: Closure,
    },
    marked: bool = false,
    //TODO: remove in release builds
    //origin: if (builtin.mode == .Debug) *const tokens.Token else void,
    //origin_module: if (builtin.mode == .Debug) []const u8 else void,
    origin: *const tokens.Token,
    origin_module: []const u8,

    pub fn mark(self: *Value) void {
        if (self.marked) {
            return;
        }
        self.marked = true;
        switch (self.as) {
            .string, .integer, .float, .boolean => {},
            // Visit children/etc
            .list => |l| {
                for (l.items) |i| {
                    i.mark();
                }
            },
            .record => |r| {
                var it = r.iterator();
                while (it.next()) |el| {
                    el.value_ptr.*.mark();
                }
            },
            .closure => |c| {
                for (c.upvalues.items) |i| {
                    i.mark();
                }
            },
        }
    }

    pub fn unmark(self: *Value) void {
        self.marked = false;
    }

    pub fn deinit(self: *Value, ally: std.mem.Allocator) void {
        switch (self.as) {
            .integer, .float, .boolean => {},
            .string => |s| {
                ally.free(s);
            },
            .list => |l| {
                l.deinit();
            },
            .record => |*r| {
                r.deinit();
            },
            .closure => |*c| c.deinit(),
        }
    }

    pub fn format(
        self: *const Value,
        writer: *std.Io.Writer,
    ) !void {
        switch (self.as) {
            .string => |s| try writer.print("{s}", .{s}),
            .integer => |i| try writer.print("{}", .{i}),
            .float => |f| try writer.print("{}", .{f}),
            .boolean => |b| try writer.print("{s}", .{if (b) "true" else "false"}),
            .list => |l| {
                try writer.print("[ ", .{});
                for (l.items) |item| {
                    try item.format(writer);
                    try writer.writeByte(' ');
                }
                try writer.print("]", .{});
            },
            .record => |r| {
                try writer.print("[", .{});
                var it = r.iterator();
                var entered = false;
                while (it.next()) |pair| {
                    try writer.print(" {s} = {any} ", .{ pair.key_ptr.*, pair.value_ptr.* });
                    entered = true;
                }
                if (!entered) {
                    try writer.print("=", .{});
                }
                try writer.print("]", .{});
            },
            .closure => {
                try writer.print("closure@{x}", .{@intFromPtr(self)});
            },
        }
    }

    pub fn asStr(self: *const Value, ally: std.mem.Allocator) ![]u8 {
        return switch (self.as) {
            .string => |s| try ally.dupe(u8, s),
            .integer => |i| try std.fmt.allocPrint(ally, "{}", .{i}),
            .float => |f| try std.fmt.allocPrint(ally, "{}", .{f}),
            .boolean => |b| try ally.dupe(u8, if (b) "true" else "false"),
            .list => |l| blk: {
                const write = std.fmt.format;
                if (l.items.len == 0) {
                    return try ally.dupe(u8, "[]");
                }
                var string = try std.array_list.Managed(u8).initCapacity(ally, 16);
                const writer = string.writer();
                try write(writer, "[ ", .{});

                for (l.items) |item| {
                    const value_str = try item.asStr(ally);
                    defer ally.free(value_str);
                    try write(writer, "{s} ", .{value_str});
                }
                try write(writer, "]", .{});
                break :blk try string.toOwnedSlice();
            },
            .record => |r| blk: {
                const write = std.fmt.format;
                if (r.count() == 0) {
                    return try ally.dupe(u8, "[=]");
                }
                var string = try std.array_list.Managed(u8).initCapacity(ally, 16);
                const writer = string.writer();
                try write(writer, "[ ", .{});
                var it = r.iterator();
                while (it.next()) |*entry| {
                    const key_str = entry.key_ptr.*;
                    const value_str = try entry.value_ptr.*.asStr(ally);
                    std.debug.print("this is it: {s}\n", .{value_str});
                    defer ally.free(value_str);
                    try write(writer, "{s} = {s} ", .{ key_str, value_str });
                }
                try write(writer, "]", .{});
                break :blk try string.toOwnedSlice();
            },
            .closure => try std.fmt.allocPrint(ally, "fn@{x}", .{@intFromPtr(self)}),
        };
    }

    pub fn asInt(self: Value) !i64 {
        return switch (self.as) {
            .string => |s| try std.fmt.parseInt(i64, s, 10),
            .integer => |i| i,
            .float => |f| @as(i64, @intFromFloat(f)),
            .boolean => |b| if (b) 1 else 0,
            .list, .record, .closure => error.InvalidIntConversion,
        };
    }

    const Order = union(enum) {
        less,
        greater,
        equal,
        failure: struct {
            wants: []const u8,
            got: []const u8,
        },
    };

    pub fn compare(self: *const Value, other: *const Value) !Order {
        switch (other.as) {
            .float => {
                std.debug.print("other ({*}) was float: {any}", .{ other, other });
            },
            else => {},
        }
        switch (self.as) {
            //TODO: needs more comparisons
            .string => |lhs| switch (other.as) {
                .string => |rhs| {
                    const order = std.mem.order(u8, lhs, rhs);
                    return switch (order) {
                        .lt => .less,
                        .gt => .greater,
                        .eq => .equal,
                    };
                },
                else => return typeMismatch(.string, other.kindName()),
            },
            .integer => |lhs| switch (other.as) {
                .integer => |rhs| {
                    if (lhs < rhs) {
                        return .less;
                    } else if (lhs > rhs) {
                        return .greater;
                    }
                    return .equal;
                },
                else => return typeMismatch(.string, other.kindName()),
            },
            .float => |lhs| {
                std.debug.print("this ({*}) was float: {} {any}", .{ self, lhs, other });

                unreachable;
            },
            .boolean => |lhs| switch (other.as) {
                .boolean => |rhs| {
                    if (lhs and rhs) {
                        return .equal;
                    } else if (!lhs and rhs) {
                        return .less;
                    } else if (lhs and !rhs) {
                        return .greater;
                    } else if (!lhs and !rhs) {
                        return .equal;
                    }
                },
                else => return typeMismatch(.string, other.kindName()),
            },
            .list => |lhs| switch (other.as) {
                .list => |rhs| {
                    const l = lhs.items;
                    const r = rhs.items;
                    for (0..@max(l.len, r.len)) |i| {
                        if (l.len + 1 < i) { // lhs ends early
                            return .less;
                        } else if (r.len + 1 < i) { // rhs ends early
                            return .less;
                        } else if (try l[i].compare(l[i]) == .less) { // lhs is lesser
                            return .less;
                        } else if (try l[i].compare(r[i]) == .greater) { // rhs is lesser
                            return .greater;
                        }
                    }
                    return .equal;
                },
                else => return typeMismatch(.string, other.kindName()),
            },
            .record => |lhs| switch (other.as) {
                .record => |rhs| {
                    const lhs_count = lhs.count();
                    const rhs_count = rhs.count();
                    if (lhs_count < rhs_count) {
                        return .less;
                    }
                    if (lhs_count > rhs_count) {
                        return .greater;
                    }

                    var lhs_it = lhs.iterator();
                    var rhs_it = rhs.iterator();

                    while (lhs_it.next()) |a| {
                        const b = rhs_it.next().?;
                        const key_order = std.mem.order(u8, a.key_ptr.*, b.key_ptr.*);
                        switch (key_order) {
                            .lt => return .less,
                            .gt => return .greater,
                            .eq => {},
                        }

                        const value_order = try a.value_ptr.*.compare(b.value_ptr.*);
                        switch (value_order) {
                            .less => return .less,
                            .greater => return .greater,
                            .failure => return value_order,
                            .equal => {},
                        }
                    }

                    return .equal;
                },
                else => return typeMismatch(.string, other.kindName()),
            },
            .closure => unreachable,
        }
        unreachable;
    }

    pub fn access(self: *const Value) !*Record {
        return switch (self.as) {
            .record => |*r| r,
            else => InterpreterError.MembersNotAllowed,
        };
    }

    pub fn kindName(self: *const Value) []const u8 {
        return switch (self.as) {
            .integer => "int",
            .string => "string",
            .record => "record",
            .float => "float",
            .boolean => "bool",
            .list => "list",
            .closure => "closure",
        };
    }
    pub fn kind(self: Value) Type {
        return std.meta.activeTag(self);
    }
};

fn typeMismatch(lhs_type: Type, rhs_type: []const u8) !Value.Order {
    return Value.Order{
        .failure = .{
            .wants = @tagName(lhs_type),
            .got = rhs_type,
        },
    };
}

pub const Result = union(enum) {
    value: *Value,
    nothing,
    values: []*Value,
};

pub const nothing = Result{ .nothing = {} };

pub fn something(value: *Value) Result {
    return Result{ .value = value };
}

pub fn multiple(vals: []*Value) Result {
    return Result{ .values = vals };
}

const testState = pipeline.testState;

test "value kindString" {
    const v = Value{
        .as = .{ .integer = 0 },
        .origin = undefined,
        .origin_module = undefined,
    };

    try std.testing.expectEqualStrings("int", v.kindName());
}
