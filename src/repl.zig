const std = @import("std");
const fmt = std.fmt.format;

fn fmts(writer: anytype, comptime str: []const u8) void {
    fmt(writer, str, .{}) catch unreachable;
}

pub fn repl() !void {
    var array: [4096]u8 = undefined;
    const err = std.io.getStdErr().writer();
    const out = std.io.getStdOut().writer();
    const in = std.io.getStdIn().reader();
    while (true) {
        fmts(out, "=> ");
        const slice_result = in.readUntilDelimiter(&array, '\n');
        if (slice_result) |slice| {
            try fmt(out, "{s}\n", .{slice});
        } else |e| {
            switch (e) {
                // ^D -> close shell
                error.EndOfStream => return,
                // handle
                error.StreamTooLong => {
                    fmts(err, "Input was too long\n");
                },
                // unrecoverable
                else => unreachable,
            }
        }
    }
}
