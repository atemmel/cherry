const std = @import("std");

pub const Value = union(enum) {
    // strings are immutable
    string: []const u8,
    // integer type
    integer: i64,
    float: f64,
    boolean: bool,

    pub fn str(string: []const u8) Value {
        return .{
            .string = string,
        };
    }

    pub fn int(i: i64) Value {
        return .{
            .integer = i,
        };
    }

    pub fn flt(float: f64) Value {
        return .{
            .float = float,
        };
    }

    pub fn bol(bl: bool) Value {
        return .{
            .boolean = bl,
        };
    }

    pub fn format(
        self: Value,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        switch (self) {
            .string => |s| try writer.print("{s}", .{s}),
            .integer => |i| try writer.print("{}", .{i}),
            .float => |f| try writer.print("{}", .{f}),
            .boolean => |b| try writer.print("{s}", .{if (b) "true" else "false"}),
        }
    }

    pub fn asStr(self: Value, ally: std.mem.Allocator) ![]const u8 {
        return switch (self) {
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

    pub fn compare(self: Value, other: Value) !Order {
        return switch (self) {
            //TODO: needs more comparisons
            .string => unreachable,
            .integer => |lhs| switch (other) {
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
            .boolean => switch (other) {
                .string => return Errors.MismatchedTypeError,
                .integer => return Errors.MismatchedTypeError,
                .float => return Errors.MismatchedTypeError,
                .boolean => unreachable,
            },
        };
    }
};

pub const Result = union(enum) {
    value: Value,
    nothing: void,
};

pub const nothing = Result{ .nothing = {} };

pub fn something(value: Value) Result {
    return Result{ .value = value };
}

pub fn integer(int: i64) Result {
    return something(Value{ .integer = int });
}

pub fn boolean(bl: bool) Result {
    return something(Value{ .boolean = bl });
}
