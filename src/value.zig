const std = @import("std");

pub const Value = struct {
    as: union(enum) {
        // strings are immutable
        string: []const u8,
        // integer type
        integer: i64,
        float: f64,
        boolean: bool,
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
        }
    }

    pub fn unmark(self: *Value) void {
        self.marked = false;
    }

    pub fn deinit(self: *Value, ally: std.mem.Allocator) void {
        switch (self.as) {
            .integer, .float, .boolean => {},
            //TODO: eventually free this
            .string => |s| {
                ally.free(s);
            },
            // Visit children/etc
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
        }
    }

    pub fn asStr(self: Value, ally: std.mem.Allocator) ![]const u8 {
        return switch (self.as) {
            .string => |s| s,
            .integer => |i| try std.fmt.allocPrint(ally, "{}", .{i}),
            .float => |f| try std.fmt.allocPrint(ally, "{}", .{f}),
            .boolean => |b| if (b) "true" else "false",
        };
    }

    const Errors = error{MismatchedTypeError};

    const Order = enum {
        less,
        greater,
        equal,
    };

    pub fn compare(self: *const Value, other: *const Value) !Order {
        return switch (self.as) {
            //TODO: needs more comparisons
            .string => unreachable,
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
            },
            .float => unreachable,
            .boolean => switch (other.as) {
                .string => return Errors.MismatchedTypeError,
                .integer => return Errors.MismatchedTypeError,
                .float => return Errors.MismatchedTypeError,
                .boolean => unreachable,
            },
        };
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
