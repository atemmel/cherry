const std = @import("std");
const Value = @import("symtable.zig").Value;

pub const Builtin = fn (args: []const Value) anyerror!void;

const builtins_table = std.StaticStringMap(*const Builtin).initComptime(&.{
    .{
        "say", say,
    },
});

pub fn lookup(str: []const u8) ?*const Builtin {
    return builtins_table.get(str);
}

fn say(args: []const Value) !void {
    const stdout = std.io.getStdOut().writer();
    for (args[1..]) |arg| {
        try stdout.print("{s} ", .{arg});
    }
    try stdout.print("\n", .{});
}
