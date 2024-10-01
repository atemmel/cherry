const std = @import("std");
const gc = @import("gc.zig");
const symtable = @import("symtable.zig");
const indexOfPos = std.mem.indexOfPos;

pub const Errors = error{ MismatchedTypeError, MismatchedBraces, BadLookup, MembersNotAllowed };

pub const List = std.ArrayList(*Value);
pub const Record = std.StringArrayHashMap(*Value);

pub const Type = enum {
    string,
    integer,
    float,
    boolean,
    list,
    record,
};

pub const Value = struct {
    as: union(Type) {
        // strings are immutable
        string: []const u8,
        // integer type
        integer: i64,
        float: f64,
        boolean: bool,
        list: List,
        record: Record,
    },
    marked: bool = false,

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
        }
    }

    pub fn format(
        self: Value,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        switch (self.as) {
            .string => |s| try writer.print("{s}", .{s}),
            .integer => |i| try writer.print("{}", .{i}),
            .float => |f| try writer.print("{}", .{f}),
            .boolean => |b| try writer.print("{s}", .{if (b) "true" else "false"}),
            .list => |l| {
                try writer.print("[ ", .{});
                for (l.items) |item| {
                    try writer.print("{any} ", .{item});
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
        }
    }

    pub fn asStr(self: Value, ally: std.mem.Allocator) ![]const u8 {
        return switch (self.as) {
            .string => |s| try ally.dupe(u8, s),
            .integer => |i| try std.fmt.allocPrint(ally, "{}", .{i}),
            .float => |f| try std.fmt.allocPrint(ally, "{}", .{f}),
            .boolean => |b| try ally.dupe(u8, if (b) "true" else "false"),
            .list => |l| try std.fmt.allocPrint(ally, "[{any}]", .{l.items}),
            .record => unreachable,
        };
    }

    const Order = enum {
        less,
        greater,
        equal,
    };

    pub fn compare(self: *const Value, other: *const Value) !Order {
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
                .integer, .float, .boolean, .list, .record => return Errors.MismatchedTypeError,
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
                .string, .float, .boolean, .list, .record => return Errors.MismatchedTypeError,
            },
            .float => unreachable,
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
                .string, .float, .integer, .list, .record => return Errors.MismatchedTypeError,
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
                .float, .integer, .boolean, .string, .record => return Errors.MismatchedTypeError,
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
                            .equal => {},
                        }
                    }

                    return .equal;
                },
                .float, .integer, .boolean, .string, .list => return Errors.MismatchedTypeError,
            },
        }
        unreachable;
    }

    pub fn access(self: *const Value) !*Record {
        return switch (self.as) {
            .record => |*r| r,
            else => Errors.MembersNotAllowed,
        };
    }

    pub fn interpolate(self: *const Value, ally: std.mem.Allocator) !*Value {
        const str: []const u8 = switch (self.as) {
            .string => |str| str,
            else => return Errors.MismatchedTypeError,
        };

        var result = try std.ArrayList(u8).initCapacity(ally, str.len * 2);
        defer result.deinit();

        var idx: usize = 0;
        while (idx < str.len) {
            const lbrace = indexOfPos(u8, str, idx, "{") orelse {
                try result.appendSlice(str[idx..]);
                break;
            };

            if (lbrace + 1 < str.len and str[lbrace + 1] == '{') {
                // found escape sequence
                const escape_begin = lbrace + 2;
                const escape_end = indexOfPos(u8, str, lbrace + 1, "}}") orelse return Errors.MismatchedBraces;
                // include the last lbrace
                try result.appendSlice(str[idx .. lbrace + 1]);
                // include the first rbrace
                try result.appendSlice(str[escape_begin .. escape_end + 1]);
                idx = escape_end + 2;
                continue;
            }

            const rbrace = indexOfPos(u8, str, lbrace, "}") orelse return Errors.MismatchedBraces;

            const variable_name = str[lbrace + 1 .. rbrace];
            const variable_value = symtable.get(variable_name) orelse return Errors.BadLookup;

            //TODO: arena allocator candidate
            const variable_string = try variable_value.asStr(ally);
            defer ally.free(variable_string);

            try result.appendSlice(str[idx..lbrace]);
            try result.appendSlice(variable_string);
            idx = rbrace + 1;
        }
        return try gc.allocedString(try result.toOwnedSlice());
    }
};

pub const Result = union(enum) {
    value: *Value,
    nothing: void,
};

pub const nothing = Result{ .nothing = {} };

pub fn something(value: *Value) Result {
    return Result{ .value = value };
}

test "interpolate single value" {
    const ally = std.testing.allocator;

    try gc.init(ally);
    symtable.init(ally);
    defer gc.deinit();
    defer symtable.deinit();
    try symtable.pushFrame();

    try symtable.insert("y", try gc.string("x"));

    const str = try gc.string("x {y}");

    const result = try str.interpolate(ally);
    try std.testing.expectEqualStrings("x x", result.as.string);
}

test "interpolate multiple values" {
    const ally = std.testing.allocator;

    try gc.init(ally);
    symtable.init(ally);
    defer gc.deinit();
    defer symtable.deinit();
    try symtable.pushFrame();

    try symtable.insert("y", try gc.string("x"));

    const str = try gc.string("x {y} {y}");

    const result = try str.interpolate(ally);
    try std.testing.expectEqualStrings("x x x", result.as.string);
}

test "interpolate no values" {
    const ally = std.testing.allocator;

    try gc.init(ally);
    defer gc.deinit();

    const str = try gc.string("x");

    const result = try str.interpolate(ally);
    try std.testing.expectEqualStrings("x", result.as.string);
}

test "escape interpolation" {
    const ally = std.testing.allocator;

    try gc.init(ally);
    defer gc.deinit();

    const str = try gc.string("{{x}}");

    const result = try str.interpolate(ally);
    try std.testing.expectEqualStrings("{x}", result.as.string);
}
