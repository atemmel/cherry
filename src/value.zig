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

    pub fn int(integer: i64) Value {
        return .{
            .integer = integer,
        };
    }

    pub fn flt(float: f64) Value {
        return .{
            .float = float,
        };
    }

    pub fn bol(boolean: bool) Value {
        return .{
            .boolean = boolean,
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
};
