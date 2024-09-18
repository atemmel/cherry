const std = @import("std");
const pipeline = @import("pipeline.zig");
const terminal = @import("term.zig");
const Term = terminal.Term;
const utils = @import("utils.zig");

const History = std.ArrayList([]const u8);

const State = struct {
    pipeline_state: *pipeline.State,
    ally: std.mem.Allocator,
    out_writer: std.io.BufferedWriter(4096, std.fs.File.Writer),
    history: History,
    cwd_buffer: [1024]u8 = undefined,
    cwd: []const u8 = "",
    buffer: [4096]u8 = undefined,
    length: usize = 0,
    cursor: usize = 0,
    term: Term,
    home: []const u8 = "",
    histfile_path: []const u8 = "",
    history_scroll_idx: usize,

    pub fn deinit(self: *State) void {
        for (self.history.items) |item| {
            self.ally.free(item);
        }
        self.history.deinit();
        self.ally.free(self.home);
        self.ally.free(self.histfile_path);
        self.term.restore() catch unreachable; // no balls
    }

    pub fn writer(self: *State) @TypeOf(self.out_writer.writer()) {
        return self.out_writer.writer();
    }

    pub fn flush(self: *State) !void {
        try self.out_writer.flush();
    }

    pub fn line(self: *State) []const u8 {
        return self.buffer[0..self.length];
    }

    pub fn calcCwd(self: *State) ![]const u8 {
        self.cwd = try std.fs.cwd().realpath(".", &self.cwd_buffer);
        return self.cwd;
    }

    pub fn writeKeyAtCursor(self: *State, key: u8) void {
        std.mem.rotate(u8, self.buffer[self.cursor .. self.length + 1], self.length - self.cursor);
        self.buffer[self.cursor] = key;
        self.cursor += 1;
        self.length += 1;
    }

    pub fn removeKeyAtCursor(self: *State) void {
        if (self.cursor > 0) {
            std.mem.rotate(u8, self.buffer[self.cursor - 1 .. self.length], self.length - self.cursor);
            self.cursor -= 1;
            self.length -= 1;
        }
    }

    pub fn removeKeyBehindCursor(self: *State) void {
        if (self.cursor < self.length) {
            std.mem.rotate(u8, self.buffer[self.cursor..self.length], self.length - self.cursor);
            self.length -= 1;
        }
    }

    pub fn prefixLen(self: *State) usize {
        return self.cwd.len + 4;
    }
};

/// fmt wrapper that doesn't care if it fails
fn fmt(writer: anytype, comptime str: []const u8, args: anytype) void {
    std.fmt.format(writer, str, args) catch {};
}

/// fmt wrapper for writing comptime strings
fn fmts(writer: anytype, comptime str: []const u8) void {
    fmt(writer, str, .{});
}

//TODO
// - display commands in a nice way (colors, etc)
// - history search
// - aliases

pub fn repl(pipeline_state: *pipeline.State) !void {
    var state = State{
        .ally = pipeline_state.ally,
        .history = try History.initCapacity(pipeline_state.ally, 512),
        .out_writer = std.io.bufferedWriter(std.io.getStdOut().writer()),
        .pipeline_state = pipeline_state,
        .term = try Term.init(),
        .home = try std.process.getEnvVarOwned(pipeline_state.ally, "HOME"),
        .history_scroll_idx = 0,
    };
    defer state.deinit();

    state.histfile_path = try std.fs.path.join(state.ally, &.{ state.home, ".yash-hist" });

    try readHistory(&state);
    state.history_scroll_idx = state.history.items.len;

    const out = state.writer();

    while (true) {
        const cwd = try state.calcCwd();
        terminal.clearLine(out, state.prefixLen() + 7 + state.length);
        fmt(out, "{s} |> ", .{cwd});
        fmt(out, "{s}\r", .{state.line()});
        terminal.moveRight(out, state.prefixLen() + state.cursor);
        try state.flush();
        const event = try state.term.readEvent();
        switch (event) {
            .key => |key| {
                switch (key) {
                    '\n', '\r' => try eval(&state),
                    else => state.writeKeyAtCursor(key),
                }
            },
            .ctrl => |ctrl| {
                switch (ctrl) {
                    'd' => {
                        fmts(out, "exit");
                        try state.flush();
                        return;
                    },
                    'c' => {
                        fmts(out, "^C\n\r");
                        try state.flush();
                        state.length = 0;
                        state.cursor = 0;
                    },
                    'u' => {
                        terminal.clearLine(out, state.length + state.prefixLen());
                        state.length = 0;
                        state.cursor = 0;
                    },
                    'l' => {
                        terminal.moveCursor(state.term.tty.writer(), 0, 0);
                        terminal.clear(out);
                    },
                    'r' => {
                        try searchCommand(&state);
                    },
                    else => {}, // ignore
                }
            },
            .arrow_down => {
                nextCommand(&state);
            },
            .arrow_up => {
                previousCommand(&state);
            },
            .arrow_left => {
                if (state.cursor > 0) {
                    state.cursor -= 1;
                }
            },
            .arrow_right => {
                if (state.cursor < state.length) {
                    state.cursor += 1;
                }
            },
            .backspace => state.removeKeyAtCursor(),
            .delete => state.removeKeyBehindCursor(),
            .home => {
                state.cursor = 0;
            },
            .end => {
                state.cursor = state.length;
            },
            .tab => {
                try tryAutocomplete(&state);
            },
            .alt, .escape, .insert, .page_down, .page_up, .unknown => {},
        }
    }
}

fn eval(state: *State) !void {
    try state.writer().print("\r\n", .{});
    terminal.clearLine(state.writer(), state.prefixLen());
    try state.term.restore();
    try state.flush();
    defer {
        state.term = Term.init() catch unreachable;
        state.length = 0;
        state.cursor = 0;
    }

    const cmd = state.line();

    state.pipeline_state.source = cmd;
    pipeline.run(state.pipeline_state) catch |e| {
        switch (e) {
            //TODO: error should not be handled here,
            error.CommandNotFound => try state.writer().print("Could not find command in system\r\n", .{}),
            else => unreachable,
        }
    };
    try state.flush();
    try appendHistory(state);
    state.history_scroll_idx = state.history.items.len;

    appendHistory(state) catch |e| {
        try state.writer().print("Could not write history, {any}\r\n", .{e});
        try state.flush();
    };
}

fn appendHistory(state: *State) !void {
    if (state.line().len == 0) {
        return;
    }
    const cmd_copy = try state.ally.dupe(u8, state.line());
    try state.history.append(cmd_copy);

    var file = try std.fs.cwd().createFile(state.histfile_path, .{
        .truncate = false,
    });
    defer file.close();
    const stat = try file.stat();
    try file.seekTo(stat.size);
    try file.writeAll(cmd_copy);
    try file.writeAll("\n");
}

fn readHistory(state: *State) !void {
    const file = std.fs.cwd().openFile(state.histfile_path, .{}) catch |e|
        switch (e) {
        error.FileNotFound => return,
        else => {
            fmt(
                state.writer(),
                "Could not open histfile at {s}, error: {}\r\n",
                .{ state.histfile_path, e },
            );
            try state.flush();
            return;
        },
    };
    defer file.close();

    const underlying_reader = file.reader();
    var buffered_reader = std.io.bufferedReader(underlying_reader);
    const reader = buffered_reader.reader();
    while (true) {
        const line = reader.readUntilDelimiterAlloc(state.ally, '\n', state.buffer.len) catch return;
        try state.history.append(line);
    }
}

fn previousCommand(state: *State) void {
    if (state.history_scroll_idx == 0) {
        return;
    }
    state.history_scroll_idx -= 1;
    const cmd_slice = state.history.items[state.history_scroll_idx];
    replaceCommand(state, cmd_slice);
}

fn nextCommand(state: *State) void {
    if (state.history_scroll_idx == state.history.items.len - 1) {
        return;
    }
    state.history_scroll_idx += 1;
    const cmd_slice = state.history.items[state.history_scroll_idx];
    replaceCommand(state, cmd_slice);
}

fn replaceCommand(state: *State, cmd: []const u8) void {
    terminal.clearLine(state.writer(), state.prefixLen() + 7 + state.length);
    @memset(&state.buffer, 0);
    @memcpy(state.buffer[0..cmd.len], cmd);
    state.cursor = cmd.len;
    state.length = cmd.len;
}

fn tryAutocomplete(state: *State) !void {
    const has_spaces = std.mem.indexOfScalar(u8, state.line(), ' ') != null;

    if (has_spaces) {
        try tryAutocompletePath(state);
    } else {
        try tryAutocompleteCmd(state);
    }
}

fn tryAutocompleteCmd(state: *State) !void {
    if (state.length == 0) {
        return;
    }
    const path = utils.env.get("PATH") orelse {
        return;
    };

    var n_hits: usize = 0;
    var buf: [128]u8 = undefined;
    var slice: []const u8 = "";

    var it = std.mem.tokenize(u8, path, ":");
    const out = state.writer();
    while (it.next()) |p| {
        var dir = std.fs.cwd().openDir(p, .{ .iterate = true }) catch {
            continue;
        };
        defer dir.close();
        var dir_it = dir.iterate();
        while (try dir_it.next()) |entry| {
            if (entry.kind != .file and entry.kind != .sym_link) {
                continue;
            }

            const stat = dir.statFile(entry.name) catch {
                continue;
            };

            if (stat.mode & 0b1 == 0) {
                continue;
            }

            if (std.mem.startsWith(u8, entry.name, state.line())) {
                n_hits += 1;
                if (n_hits == 1) {
                    slice = try std.fmt.bufPrint(&buf, "{s}", .{entry.name});
                } else if (n_hits == 2) {
                    fmt(out, "\r\n{s} ", .{slice});
                }

                if (n_hits > 1) {
                    fmt(out, "\r\n{s} ", .{entry.name});
                }
            }
        }
    }
    if (n_hits > 1) {
        fmts(out, "\r\n");
        try state.flush();
    }

    if (n_hits == 1) {
        _ = try std.fmt.bufPrint(&state.buffer, "{s}", .{slice});
        state.length = slice.len;
        state.cursor = slice.len;
    }
}

fn tryAutocompletePath(state: *State) !void {
    var n_hits: usize = 0;
    var buf: [128]u8 = undefined;
    var slice: []const u8 = "";

    const line = state.line();

    var last_cmd_begin = std.mem.lastIndexOfScalar(u8, line, ' ') orelse line.len;
    if (last_cmd_begin < line.len) {
        last_cmd_begin += 1;
    }
    const last_cmd = line[last_cmd_begin..];

    const out = state.writer();
    var dir = try std.fs.cwd().openDir(".", .{ .iterate = true });
    defer dir.close();
    var dir_it = dir.iterate();
    while (try dir_it.next()) |entry| {
        if (std.mem.startsWith(u8, entry.name, last_cmd)) {
            n_hits += 1;
            if (n_hits == 1) {
                slice = try std.fmt.bufPrint(&buf, "{s}", .{entry.name});
            } else if (n_hits == 2) {
                fmt(out, "\r\n{s} ", .{slice});
            }

            if (n_hits > 1) {
                fmt(out, "\r\n{s} ", .{entry.name});
            }
        }
    }

    if (n_hits > 1) {
        fmts(out, "\r\n");
        try state.flush();
    }

    if (n_hits == 1) {
        const offset = line.len - last_cmd_begin;
        _ = try std.fmt.bufPrint(state.buffer[state.length - offset ..], "{s}", .{slice});
        state.length += slice.len - offset;
        state.cursor += slice.len - offset;
    }
}

fn searchCommand(state: *State) !void {
    const out = state.writer();
    while (true) {
        terminal.clearLine(out, state.prefixLen() + 7 + state.length);
        fmt(out, "(reverse-search) {s}\r", .{state.line()});
        terminal.moveRight(out, state.prefixLen() + state.cursor);
        try state.flush();
        const event = try state.term.readEvent();
        switch (event) {
            .key => |key| {
                switch (key) {
                    '\n', '\r' => {},
                    else => {},
                }
            },
            .ctrl => |ctrl| {
                switch (ctrl) {
                    'c' => {
                        fmts(out, "^C\n\r");
                        try state.flush();
                        state.length = 0;
                        state.cursor = 0;
                        break;
                    },
                    'u' => {
                        terminal.clearLine(out, state.length + state.prefixLen());
                        state.length = 0;
                        state.cursor = 0;
                    },
                    'r' => {},
                    else => {}, // ignore
                }
            },
            .arrow_down => {},
            .arrow_up => {},
            .arrow_left => {
                if (state.cursor > 0) {
                    state.cursor -= 1;
                }
            },
            .arrow_right => {
                if (state.cursor < state.length) {
                    state.cursor += 1;
                }
            },
            .backspace => state.removeKeyAtCursor(),
            .delete => state.removeKeyBehindCursor(),
            .home => {
                state.cursor = 0;
            },
            .end => {
                state.cursor = state.length;
            },
            .tab => {},
            .alt, .escape, .insert, .page_down, .page_up, .unknown => {},
        }
    }
}
