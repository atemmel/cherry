const std = @import("std");
const fmt = std.fmt.format;
const terminal = @import("term.zig");
const Term = terminal.Term;

fn fmts(writer: anytype, comptime str: []const u8) void {
    fmt(writer, str, .{}) catch unreachable;
}

pub fn repl() !void {
    var term = try Term.init();
    defer term.restore() catch unreachable; // no balls

    var buffer: [4096]u8 = undefined;
    var length: usize = 0;
    var cursor: usize = 0;
    var buffered_writer = std.io.bufferedWriter(std.io.getStdOut().writer());
    const out = buffered_writer.writer();
    const prefix = "|> ";

    while (true) {
        terminal.clearLine(out, length + prefix.len + 1);
        fmts(out, prefix);
        try fmt(out, "{s}\r", .{buffer[0..length]});
        terminal.moveRight(out, prefix.len + cursor);
        try buffered_writer.flush();
        const event = try term.readEvent();
        switch (event) {
            .key => |key| {
                switch (key) {
                    '\n', '\r' => {
                        try out.print("\r\n", .{});
                        length = 0;
                        cursor = 0;
                    },
                    else => {
                        std.mem.rotate(u8, buffer[cursor .. length + 1], length - cursor);
                        buffer[cursor] = key;
                        cursor += 1;
                        length += 1;
                    },
                }
            },
            .ctrl => |ctrl| {
                switch (ctrl) {
                    'd' => {
                        try out.print("exit", .{});
                        return;
                    },
                    'c' => {
                        try out.print("^C\n\r", .{});
                        length = 0;
                        cursor = 0;
                    },
                    'u' => {
                        terminal.clearLine(out, length + prefix.len);
                        length = 0;
                        cursor = 0;
                    },
                    'l' => {
                        terminal.clear(out);
                    },
                    else => {}, // ignore
                }
            },
            .arrow_down => {},
            .arrow_up => {},
            .arrow_left => {
                if (cursor > 0) {
                    cursor -= 1;
                }
            },
            .arrow_right => {
                if (cursor < length) {
                    cursor += 1;
                }
            },
            .backspace => {
                if (cursor > 0) {
                    std.mem.rotate(u8, buffer[cursor - 1 .. length], length - cursor);
                    cursor -= 1;
                    length -= 1;
                }
            },
            .delete => {
                if (cursor < length) {
                    std.mem.rotate(u8, buffer[cursor..length], length - cursor);
                    length -= 1;
                }
            },
            .home => {
                cursor = 0;
            },
            .end => {
                cursor = length;
            },
            .alt, .escape, .insert, .page_down, .page_up, .unknown => {},
        }
    }
}
