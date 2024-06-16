const std = @import("std");
const fmt = std.fmt.format;

pub fn repl() !void {
    var array: [4096]u8 = undefined;
    const out = std.io.getStdOut().writer();
    const in = std.io.getStdIn().reader();
    while (true) {
        try fmt(out, "=> ", .{});
        const len = try in.read(&array);
        const slice = array[0..len];
        try fmt(out, "{s}\n", .{slice});
    }
}
