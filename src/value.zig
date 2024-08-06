const std = @import("std");
const gc = @import("gc.zig");
const symtable = @import("symtable.zig");
const indexOfPos = std.mem.indexOfPos;

pub const Errors = error{ MismatchedTypeError, MismatchedBraces, BadLookup };

pub const List = std.ArrayList(*Value);

pub const Value = struct {
    as: union(enum) {
        // strings are immutable
        string: []const u8,
        // integer type
        integer: i64,
        float: f64,
        boolean: bool,
        list: List,
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
        }
    }

    pub fn asStr(self: Value, ally: std.mem.Allocator) ![]const u8 {
        return switch (self.as) {
            .string => |s| try ally.dupe(u8, s),
            .integer => |i| try std.fmt.allocPrint(ally, "{}", .{i}),
            .float => |f| try std.fmt.allocPrint(ally, "{}", .{f}),
            .boolean => |b| try ally.dupe(u8, if (b) "true" else "false"),
            .list => |l| try std.fmt.allocPrint(ally, "[{any}]", .{l.items}),
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
                .integer => return Errors.MismatchedTypeError,
                .float => return Errors.MismatchedTypeError,
                .boolean => return Errors.MismatchedTypeError,
                .list => return Errors.MismatchedTypeError,
            },
            .integer => |lhs| switch (other.as) {
                .string => return Errors.MismatchedTypeError,
                .integer => |rhs| {
                    if (lhs < rhs) {
                        return .less;
                    } else if (lhs > rhs) {
                        return .greater;
                    }
                    return .equal;
                },
                .float => return Errors.MismatchedTypeError,
                .boolean => return Errors.MismatchedTypeError,
                .list => return Errors.MismatchedTypeError,
            },
            .float => unreachable,
            .boolean => |lhs| switch (other.as) {
                .string => return Errors.MismatchedTypeError,
                .integer => return Errors.MismatchedTypeError,
                .float => return Errors.MismatchedTypeError,
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
                .list => return Errors.MismatchedTypeError,
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
                .float, .integer, .boolean, .string => return Errors.MismatchedTypeError,
            },
        }
        unreachable;
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
