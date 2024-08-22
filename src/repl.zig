const std = @import("std");
const pipeline = @import("pipeline.zig");
const terminal = @import("term.zig");
const Term = terminal.Term;

const History = std.ArrayList([]const u8);

/// fmt wrapper that doesn't care if it fails
fn fmt(writer: anytype, comptime str: []const u8, args: anytype) void {
    std.fmt.format(writer, str, args) catch {};
}

/// fmt wrapper for writing comptime strings
fn fmts(writer: anytype, comptime str: []const u8) void {
    fmt(writer, str, .{});
}

//TODO
// - run actual commands
// - display commands in a nice way (colors, etc)
// - history
// - tab completion

pub fn repl(state: *pipeline.State) !void {
    var history = try History.initCapacity(state.ally, 100);
    defer {
        for (history.items) |item| {
            state.ally.free(item);
        }
        history.deinit();
    }
    var term = try Term.init();
    defer term.restore() catch unreachable; // no balls

    var cwd_buffer: [1024]u8 = undefined;
    var buffer: [4096]u8 = undefined;
    var length: usize = 0;
    var cursor: usize = 0;
    var buffered_writer = std.io.bufferedWriter(std.io.getStdOut().writer());
    const out = buffered_writer.writer();

    while (true) {
        const cwd = try std.fs.cwd().realpath(".", &cwd_buffer);
        const prefix_len = cwd.len + 4;
        terminal.clearLine(out, prefix_len + 7);
        fmt(out, "{s} |> ", .{cwd});
        fmt(out, "{s}\r", .{buffer[0..length]});
        terminal.moveRight(out, prefix_len + cursor);
        try buffered_writer.flush();
        const event = try term.readEvent();
        switch (event) {
            .key => |key| {
                switch (key) {
                    '\n', '\r' => {
                        try out.print("\r\n", .{});
                        terminal.clearLine(out, prefix_len);
                        try term.restore();
                        try buffered_writer.flush();
                        defer {
                            term = Term.init() catch unreachable;
                            length = 0;
                            cursor = 0;
                        }

                        const cmd = buffer[0..length];

                        state.source = cmd;
                        pipeline.run(state) catch |e| {
                            switch (e) {
                                //TODO: error should not be handled here,
                                error.CommandNotFound => {
                                    _ = try out.write("Could not find command in system\r\n");
                                },
                                else => unreachable,
                            }
                        };
                        try appendHistory(state.ally, &history, cmd);
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
                        fmts(out, "exit");
                        return;
                    },
                    'c' => {
                        fmts(out, "^C\n\r");
                        length = 0;
                        cursor = 0;
                    },
                    'u' => {
                        terminal.clearLine(out, length + prefix_len);
                        length = 0;
                        cursor = 0;
                    },
                    'l' => {
                        terminal.moveCursor(term.tty.writer(), 0, 0);
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

fn appendHistory(ally: std.mem.Allocator, hist: *History, cmd: []const u8) !void {
    const cmd_copy = try ally.dupe(u8, cmd);
    try hist.append(cmd_copy);
    //TODO: append to histfile
}
