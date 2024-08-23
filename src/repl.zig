const std = @import("std");
const pipeline = @import("pipeline.zig");
const terminal = @import("term.zig");
const Term = terminal.Term;

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

    pub fn deinit(self: *State) void {
        for (self.history.items) |item| {
            self.ally.free(item);
        }
        self.history.deinit();
        self.term.restore() catch unreachable; // no balls
    }

    pub fn writer(self: *State) @TypeOf(self.out_writer.writer()) {
        return self.out_writer.writer();
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
// - history
// - tab completion

pub fn repl(pipeline_state: *pipeline.State) !void {
    var state = State{
        .ally = pipeline_state.ally,
        .history = try History.initCapacity(pipeline_state.ally, 100),
        .out_writer = std.io.bufferedWriter(std.io.getStdOut().writer()),
        .pipeline_state = pipeline_state,
        .term = try Term.init(),
    };
    defer state.deinit();

    var buffered_writer = std.io.bufferedWriter(std.io.getStdOut().writer());
    const out = buffered_writer.writer();

    while (true) {
        const cwd = try std.fs.cwd().realpath(".", &state.cwd_buffer);
        const prefix_len = cwd.len + 4;
        terminal.clearLine(out, prefix_len + 7);
        fmt(out, "{s} |> ", .{cwd});
        fmt(out, "{s}\r", .{state.line()});
        terminal.moveRight(out, prefix_len + state.cursor);
        try buffered_writer.flush();
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
                        return;
                    },
                    'c' => {
                        fmts(out, "^C\n\r");
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
            .alt, .escape, .insert, .page_down, .page_up, .unknown => {},
        }
    }
}

fn eval(state: *State) !void {
    try state.writer().print("\r\n", .{});
    terminal.clearLine(state.writer(), state.prefixLen());
    try state.term.restore();
    try state.out_writer.flush();
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
    try state.out_writer.flush();
    try appendHistory(state);
}

fn appendHistory(state: *State) !void {
    const cmd_copy = try state.ally.dupe(u8, state.line());
    try state.history.append(cmd_copy);
    //TODO: append to histfile
}
