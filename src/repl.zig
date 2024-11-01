const std = @import("std");
const algo = @import("algo.zig");
const pipeline = @import("pipeline.zig");
const terminal = @import("term.zig");
const symtable = @import("symtable.zig");
const Term = terminal.Term;
const Color = terminal.Color;
const Hi = terminal.Hi;
const BoldHi = terminal.BoldHi;

const startsWith = std.mem.startsWith;

const History = std.ArrayList([]const u8);

const Mode = enum {
    prompt,
    search,
};

const State = struct {
    pipeline_state: *pipeline.State,
    ally: std.mem.Allocator,
    out_writer: std.io.BufferedWriter(4096, std.fs.File.Writer),
    history: History,
    cwd_buffer: [std.fs.max_path_bytes]u8 = undefined,
    cwd: []const u8 = "",
    buffer: [4096]u8 = undefined,
    length: usize = 0,
    cursor: usize = 0,
    term: Term,
    home: []const u8 = "",
    histfile_path: []const u8 = "",
    history_scroll_idx: usize,
    search_idx: ?usize = null,
    search_reached_end: bool = false,
    rc_path: []const u8 = "",
    arena: std.heap.ArenaAllocator,
    mode: Mode = .prompt,

    auto_cd: bool = true,

    pub fn deinit(self: *State) void {
        for (self.history.items) |item| {
            self.ally.free(item);
        }
        self.history.deinit();
        self.arena.deinit();
        self.term.restore() catch unreachable; // no balls
    }

    pub fn writer(self: *State) @TypeOf(self.out_writer.writer()) {
        return self.out_writer.writer();
    }

    pub fn flush(self: *State) void {
        self.out_writer.flush() catch {};
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

    pub fn writePrefix(self: *State) !void {
        const out = self.writer();
        terminal.clearLine(out, self.prefixLen() + 7 + self.length);
        switch (self.mode) {
            .prompt => {
                const cwd = try self.calcCwd();
                if (startsWith(u8, cwd, self.home)) {
                    const cwd_after_home = cwd[self.home.len..];
                    fmt(out, "~{s} {s}|>{s} {s}\r", .{ cwd_after_home, BoldHi.white, Color.gray, self.line() });
                } else {
                    fmt(out, "{s} {s}|>{s} {s}\r", .{ cwd, BoldHi.white, Color.gray, self.line() });
                }
            },
            .search => {
                if (self.search_idx) |idx| {
                    if (self.search_reached_end) {
                        fmt(out, "(reverse-search) {s} -> {s} (Search reached end)\r", .{ self.line(), self.history.items[idx] });
                    } else {
                        fmt(out, "(reverse-search) {s} -> {s}\r", .{ self.line(), self.history.items[idx] });
                    }
                } else {
                    fmt(out, "(reverse-search) {s}\r", .{self.line()});
                }
            },
        }
        terminal.moveRight(out, self.prefixLen() + self.cursor);
        self.flush();
    }

    pub fn prefixLen(self: *State) usize {
        return switch (self.mode) {
            .prompt => if (startsWith(u8, self.cwd, self.home)) self.cwd.len - self.home.len + 1 + 4 else self.cwd.len + 4,
            .search => blk: {
                var sum = "(reverse-search) ".len;
                if (self.search_idx) |idx| {
                    sum += " -> ".len + self.history.items[idx].len;
                    if (self.search_reached_end) {
                        sum += " (Search reached end)".len;
                    }
                }
                break :blk sum;
            },
        };
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

pub fn repl(pipeline_state: *pipeline.State) !void {
    var arena = std.heap.ArenaAllocator.init(pipeline_state.ally);
    var state = State{
        .ally = pipeline_state.ally,
        .history = try History.initCapacity(pipeline_state.ally, 512),
        .out_writer = std.io.bufferedWriter(std.io.getStdOut().writer()),
        .pipeline_state = pipeline_state,
        .term = try Term.init(),
        .home = try std.process.getEnvVarOwned(arena.allocator(), "HOME"),
        .history_scroll_idx = 0,
        .arena = arena,
    };
    defer state.deinit();

    state.histfile_path = try std.fs.path.join(
        state.arena.allocator(),
        &.{ state.home, ".cherry-hist" },
    );
    state.rc_path = try std.fs.path.join(
        state.arena.allocator(),
        &.{ state.home, ".config/cherryrc" },
    );

    try readHistory(&state);
    state.history_scroll_idx = state.history.items.len;

    try readRc(&state);

    const out = state.writer();

    while (true) {
        try state.writePrefix();
        const event = try state.term.readEvent();
        switch (event) {
            .key => |key| {
                switch (key) {
                    '\r' => switch (state.mode) {
                        .prompt => try eval(&state),
                        .search => {
                            if (state.search_idx) |idx| { // if holds a match from history
                                state.flush();
                                const match = state.history.items[idx];
                                replaceCommand(&state, match);
                                state.mode = .prompt;
                                try state.writePrefix();
                                try eval(&state);
                            }
                            // otherwise, ignore
                        },
                    },
                    '\n' => {},
                    else => switch (state.mode) {
                        .prompt => state.writeKeyAtCursor(key),
                        .search => {
                            state.writeKeyAtCursor(key);
                            try reverseSearch(&state, .reset);
                        },
                    },
                }
            },
            .ctrl => |ctrl| {
                switch (ctrl) {
                    'd' => {
                        fmts(out, "exit");
                        state.flush();
                        return;
                    },
                    'c' => {
                        fmts(out, "^C\n\r");
                        state.flush();
                        state.length = 0;
                        state.cursor = 0;
                        state.mode = .prompt;
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
                        switch (state.mode) {
                            .prompt => {
                                terminal.clearLine(out, state.length + state.prefixLen());
                                state.length = 0;
                                state.cursor = 0;
                                state.mode = .search;
                            },
                            .search => {
                                try reverseSearch(&state, .forward);
                            },
                        }
                    },
                    else => {}, // ignore
                }
            },
            .arrow_down => switch (state.mode) {
                .prompt => {
                    nextCommand(&state);
                },
                .search => {},
            },
            .arrow_up => switch (state.mode) {
                .prompt => {
                    previousCommand(&state);
                },
                .search => {},
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
            .backspace => {
                state.removeKeyAtCursor();
                if (state.mode == .search) {
                    try reverseSearch(&state, .reset);
                }
            },
            .delete => {
                state.removeKeyBehindCursor();
                if (state.mode == .search) {
                    try reverseSearch(&state, .reset);
                }
            },
            .home => {
                state.cursor = 0;
            },
            .end => {
                state.cursor = state.length;
            },
            .tab => switch (state.mode) {
                .prompt => {
                    try tryAutocomplete(&state);
                },
                .search => {},
            },
            .alt, .escape, .insert, .page_down, .page_up, .unknown => {},
        }
    }
}

fn eval(state: *State) !void {
    defer _ = state.pipeline_state.arena_source.reset(.retain_capacity);
    try state.writer().print("\r\n", .{});
    terminal.clearLine(state.writer(), state.prefixLen());
    try state.term.restore();
    state.flush();
    defer {
        state.term = Term.init() catch unreachable;
        state.length = 0;
        state.cursor = 0;
    }

    const cmd = try aliasLookup(state, state.line());

    state.pipeline_state.source = cmd;
    pipeline.run(state.pipeline_state) catch |e| {
        const tokens = state.pipeline_state.tokens;
        for (tokens) |tok| {
            std.debug.print("{any}\n", .{tok.kind});
        }
        std.debug.print("{any}\n", .{e});
        if (state.auto_cd and tokens.len == 1 and tokens[0].kind == .Bareword) {
            tryAutoCd(state, tokens[0].value);
        } else {
            pipeline.writeError(state.pipeline_state, e) catch |err| {
                //TODO: handle these
                try state.writer().print("Error: {any}\r\n", .{err});
                state.flush();
            };
        }
    };
    state.flush();
    state.history_scroll_idx = state.history.items.len;

    appendHistory(state) catch |e| {
        try state.writer().print("Could not write history, {any}\r\n", .{e});
        state.flush();
    };
}

fn tryAutoCd(state: *State, path: []const u8) void {
    var dir = std.fs.cwd().openDir(path, .{}) catch |err| {
        state.writer().print("auto-cd failed to open directory: {any}\r\n", .{err}) catch {};
        state.flush();
        return;
    };
    defer dir.close();
    dir.setAsCwd() catch |err| {
        state.writer().print("auto-cd failed to change directory: {any}\r\n", .{err}) catch {};
        state.flush();
    };
}

fn appendHistory(state: *State) !void {
    if (state.line().len == 0) {
        return;
    }

    if (state.history.getLastOrNull()) |prev| {
        if (std.mem.eql(u8, prev, state.line())) {
            return;
        }
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
            state.flush();
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
        tryAutocompletePath2(state) catch {};
    } else {
        tryAutocompleteCmd(state) catch {};
    }
}

fn tryAutocompleteCmd(state: *State) !void {
    if (state.length == 0) {
        return;
    }
    const path = std.posix.getenv("PATH") orelse {
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

            if (startsWith(u8, entry.name, state.line())) {
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
        state.flush();
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

    var last_cmd_begin = std.mem.lastIndexOfScalar(u8, line, ' ') orelse 0;
    if (last_cmd_begin < line.len and last_cmd_begin != 0) {
        last_cmd_begin += 1;
    }
    const last_cmd = line[last_cmd_begin..];

    const out = state.writer();
    var dir = try std.fs.cwd().openDir(".", .{ .iterate = true });
    defer dir.close();
    var dir_it = dir.iterate();
    while (try dir_it.next()) |entry| {
        if (startsWith(u8, entry.name, last_cmd)) {
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
        state.flush();
    }

    if (n_hits == 1) {
        const offset = line.len - last_cmd_begin;
        _ = try std.fmt.bufPrint(state.buffer[state.length - offset ..], "{s}", .{slice});
        state.length += slice.len - offset;
        state.cursor += slice.len - offset;
    }
}

fn tryAutocompletePath2(state: *State) !void {
    var arena = std.heap.ArenaAllocator.init(state.ally);
    defer arena.deinit();

    const line = state.line();

    var last_cmd_begin = std.mem.lastIndexOfScalar(u8, line, ' ') orelse 0;
    if (last_cmd_begin < line.len and last_cmd_begin != 0) {
        last_cmd_begin += 1;
    }
    const last_cmd = line[last_cmd_begin..];
    const out = state.writer();

    var path_ctx: PathCompletionCtx = .{
        .source = last_cmd,
        .arena = arena.allocator(),
    };
    const result = try tryAutocompletePathImpl(&path_ctx);
    switch (result) {
        .none => {},
        .single_match => |str| {
            //TODO: single match sometimes needs multiple tabs to succeed properly
            // e.g.
            //      |> ls zig-o       <TAB>
            // (x)  |> ls zig-out     <TAB>
            //      |> ls zig-out/    <TAB>
            //      |> ls zig-out/bin <TAB>
            const offset = line.len - last_cmd_begin;

            defer {
                state.length += str.len - offset;
                state.cursor += str.len - offset;
            }

            const stat = std.fs.cwd().statFile(str) catch std.fs.Dir.Stat{
                .atime = undefined,
                .ctime = undefined,
                .inode = undefined,
                .kind = .file,
                .mode = undefined,
                .mtime = undefined,
                .size = undefined,
            };
            if (std.mem.eql(u8, last_cmd, str) and stat.kind == .directory) {
                //TODO: make bufPrint to state.buffer-pattern into separate function
                _ = try std.fmt.bufPrint(state.buffer[state.length - offset ..], "{s}/", .{str});
                state.length += 1;
                state.cursor += 1;
            } else {
                _ = try std.fmt.bufPrint(state.buffer[state.length - offset ..], "{s}", .{str});
            }
        },
        .multiple_match => |all| {
            const delta = all.shared_match.len - last_cmd.len;
            if (delta > 0) {
                const shared_match_substr = all.shared_match[last_cmd.len..];
                _ = try std.fmt.bufPrint(state.buffer[state.length..], "{s}", .{shared_match_substr});
                state.length += delta;
                state.cursor += delta;
            }
            for (all.potential_matches) |match| {
                fmt(out, "\r\n{s} ", .{match});
            }
            fmts(out, "\r\n");
            state.flush();
        },
    }
}

const PathCompletionCtx = struct {
    source: []const u8,
    arena: std.mem.Allocator,
};

const PathCompletionResult = union(enum) {
    single_match: []const u8,
    multiple_match: struct {
        shared_match: []const u8,
        potential_matches: []const []const u8,
    },
    none: void,
};

fn tryAutocompletePathImpl(ctx: *PathCompletionCtx) !PathCompletionResult {
    const dir_path = try dirLookup(ctx);
    var dir = try std.fs.cwd().openDir(dir_path.dir_to_open, .{ .iterate = true });
    defer dir.close();
    var dir_it = dir.iterate();

    var result_list = std.ArrayList([]const u8).init(ctx.arena);

    while (try dir_it.next()) |entry| {
        if (startsWith(u8, entry.name, dir_path.stem)) {
            if (dir_path.base) |base| {
                const s = try std.fmt.allocPrint(ctx.arena, "{s}{s}", .{ base, entry.name });
                try result_list.append(s);
            } else {
                try result_list.append(try ctx.arena.dupe(u8, entry.name));
            }
        }
    }

    std.mem.sort([]const u8, result_list.items, {}, algo.compareStrings);

    return switch (result_list.items.len) {
        0 => .{ .none = {} },
        1 => .{ .single_match = result_list.items[0] },
        else => blk: {
            const last_shared_idx = algo.indexOfDiffStrings(result_list.items);
            const shared_match_slice = result_list.items[0][0..last_shared_idx];
            break :blk .{
                .multiple_match = .{
                    .shared_match = shared_match_slice,
                    .potential_matches = result_list.items,
                },
            };
        },
    };
}

const DirLookup = struct {
    dir_to_open: []const u8,
    base: ?[]const u8,
    stem: []const u8,
};

fn dirLookup(ctx: *PathCompletionCtx) !DirLookup {
    const first_separator = std.mem.indexOfScalar(u8, ctx.source, '/');
    if (first_separator == null) {
        return .{
            .dir_to_open = ".",
            .base = null,
            .stem = ctx.source,
        };
    }
    const last_separator = std.mem.lastIndexOfScalar(u8, ctx.source, '/');
    const end_of_base = if (last_separator == null) ctx.source.len else last_separator.? + 1;
    const source_is_root = first_separator.? == 0;
    if (source_is_root) {
        return .{
            .dir_to_open = ctx.source[0..end_of_base],
            .base = ctx.source[0..end_of_base],
            .stem = ctx.source[end_of_base..],
        };
    }
    return .{
        .dir_to_open = try std.fmt.allocPrint(ctx.arena, "./{s}", .{ctx.source[0..end_of_base]}),
        .base = ctx.source[0..end_of_base],
        .stem = ctx.source[end_of_base..],
    };
}

fn reverseSearch(state: *State, reset: enum { reset, forward }) !void {
    state.search_reached_end = false;

    if (state.history.items.len == 0) {
        return;
    }

    terminal.clearLine(state.writer(), state.prefixLen() + 7 + state.length);

    if (state.line().len == 0) {
        state.search_idx = null;
        return;
    }

    if (reset == .reset) {
        state.search_idx = null;
    }

    var start_index = state.search_idx orelse state.history.items.len;

    if (start_index == 0) {
        state.search_reached_end = true;
        return;
    }

    while (true) : (start_index -= 1) {
        const has_substring = std.mem.indexOfPos(u8, state.history.items[start_index - 1], 0, state.line()) != null;
        if (has_substring) {
            state.search_idx = start_index - 1;
            return;
        }
        if (start_index == 1) {
            state.search_reached_end = true;
            if (reset == .reset) {
                state.search_idx = null;
            }
            break;
        }
    }
}

fn readRc(state: *State) !void {
    const file = std.fs.cwd().openFile(state.rc_path, .{}) catch |e|
        switch (e) {
        error.FileNotFound => return,
        else => {
            fmt(
                state.writer(),
                "Could not open histfile at {s}, error: {}\r\n",
                .{ state.rc_path, e },
            );
            state.flush();
            return;
        },
    };
    defer file.close();

    const rc_src = file.readToEndAlloc(state.arena.allocator(), 1_000_000_000) catch return;

    try state.term.restore();
    defer {
        state.term = Term.init() catch unreachable;
        state.length = 0;
        state.cursor = 0;
    }
    state.flush();

    state.pipeline_state.source = rc_src;
    pipeline.run(state.pipeline_state) catch |e| {
        try state.writer().print("Unexpected error when reading .cherryrc at {s}: {}\r\n", .{ state.rc_path, e });
    };
    state.flush();
}

fn aliasLookup(state: *State, cmd_arg: []const u8) ![]const u8 {
    var cmd = cmd_arg;

    const aliases = symtable.aliases.items;
    if (aliases.len == 0) {
        return cmd;
    }

    const static = struct {
        var replacement_buffer: [4096]u8 = undefined;
    };

    var it = std.mem.reverseIterator(aliases);
    while (it.next()) |alias| {
        if (!startsWith(u8, cmd, alias.from)) {
            continue;
        }
        if (cmd.len == alias.from.len or cmd[alias.from.len] == ' ') {
            cmd = try std.fmt.bufPrint(
                &static.replacement_buffer,
                "{s}{s}",
                .{ alias.to, cmd[alias.from.len..] },
            );
            @memcpy(state.buffer[0..cmd.len], static.replacement_buffer[0..cmd.len]);
            cmd = state.buffer[0..cmd.len];
        }
    }
    return cmd;
}

const expectEqual = std.testing.expectEqual;
const expectEqualStrings = std.testing.expectEqualStrings;

fn testState() State {
    return State{
        .arena = undefined,
        .ally = undefined,
        .out_writer = undefined,
        .history = undefined,
        .term = undefined,
        .history_scroll_idx = undefined,
        .pipeline_state = undefined,
    };
}

test "Single alias lookup" {
    symtable.init(std.testing.allocator);
    defer symtable.deinit();

    var state = testState();

    const cmd = "ls";

    try symtable.aliases.append(.{
        .from = "ls",
        .to = "ls \"--color=auto\"",
    });

    try expectEqualStrings("ls \"--color=auto\"", try aliasLookup(&state, cmd));
}

test "Single alias lookup failure" {
    symtable.init(std.testing.allocator);
    defer symtable.deinit();

    var state = testState();

    const cmd = "lsblk";

    try symtable.aliases.append(.{
        .from = "ls",
        .to = "ls \"--color=auto\"",
    });

    try expectEqualStrings("lsblk", try aliasLookup(&state, cmd));
}

test "Single alias lookup success but keeps arg" {
    symtable.init(std.testing.allocator);
    defer symtable.deinit();

    var state = testState();

    const cmd = "ls /tmp";

    try symtable.aliases.append(.{
        .from = "ls",
        .to = "ls \"--color=auto\"",
    });

    try expectEqualStrings("ls \"--color=auto\" /tmp", try aliasLookup(&state, cmd));
}

test "Nested alias lookup success" {
    symtable.init(std.testing.allocator);
    defer symtable.deinit();

    var state = testState();

    const cmd = "ll";

    try symtable.aliases.appendSlice(&.{
        .{ .from = "ls", .to = "ls \"--color=auto\"" },
        .{ .from = "ll", .to = "ls -l" },
    });

    try expectEqualStrings("ls \"--color=auto\" -l", try aliasLookup(&state, cmd));
}

test "autocomplete simple cases" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var ctx: PathCompletionCtx = .{
        .source = "",
        .arena = arena.allocator(),
    };

    const tmpDir = std.testing.tmpDir(.{});
    try tmpDir.dir.setAsCwd();
    _ = try std.fs.cwd().createFile("alice", .{});
    _ = try std.fs.cwd().createFile("bob", .{});
    _ = try std.fs.cwd().createFile("billy", .{});
    _ = try std.fs.cwd().createFile("charlie", .{});

    {
        ctx.source = "c";
        const result = try tryAutocompletePathImpl(&ctx);
        try expectEqualStrings("charlie", result.single_match);
    }

    {
        ctx.source = "d";
        const result = try tryAutocompletePathImpl(&ctx);
        switch (result) {
            .none => {},
            else => unreachable,
        }
    }

    {
        ctx.source = "b";
        const result = try tryAutocompletePathImpl(&ctx);
        try expectEqual(2, result.multiple_match.potential_matches.len);
        try expectEqualStrings("billy", result.multiple_match.potential_matches[0]);
        try expectEqualStrings("bob", result.multiple_match.potential_matches[1]);
    }
}

test "autocomplete nested cases" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var ctx: PathCompletionCtx = .{
        .source = "",
        .arena = arena.allocator(),
    };

    const tmpDir = std.testing.tmpDir(.{});
    try tmpDir.dir.setAsCwd();

    _ = try std.fs.cwd().makeDir("./my_dir");
    _ = try std.fs.cwd().createFile("./my_dir/alice", .{});
    _ = try std.fs.cwd().createFile("./my_dir/bob", .{});
    _ = try std.fs.cwd().createFile("./my_dir/billy", .{});
    _ = try std.fs.cwd().createFile("./my_dir/charlie", .{});

    {
        ctx.source = "my_dir/a";
        const result = try tryAutocompletePathImpl(&ctx);
        try expectEqualStrings("my_dir/alice", result.single_match);
    }
}

test "autocomplete from root" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var ctx: PathCompletionCtx = .{
        .source = "",
        .arena = arena.allocator(),
    };

    {
        ctx.source = "/";
        const result = try tryAutocompletePathImpl(&ctx);
        try std.testing.expect(result.multiple_match.potential_matches.len > 0);
    }

    {
        ctx.source = "/e";
        const result = try tryAutocompletePathImpl(&ctx);
        try expectEqualStrings("/etc", result.single_match);
    }
}

test "autocomplete closest suggested path" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var ctx: PathCompletionCtx = .{
        .source = "",
        .arena = arena.allocator(),
    };

    const tmpDir = std.testing.tmpDir(.{});
    try tmpDir.dir.setAsCwd();

    _ = try std.fs.cwd().makeDir("./my_dir");
    _ = try std.fs.cwd().createFile("./my_dir/alice", .{});
    _ = try std.fs.cwd().createFile("./my_dir/bob", .{});
    _ = try std.fs.cwd().createFile("./my_dir/bobby", .{});
    _ = try std.fs.cwd().createFile("./my_dir/bobbotron", .{});
    _ = try std.fs.cwd().createFile("./my_dir/charlie", .{});

    {
        ctx.source = "my_dir/b";
        const result = try tryAutocompletePathImpl(&ctx);
        try expectEqual(3, result.multiple_match.potential_matches.len);
        try expectEqualStrings("my_dir/bob", result.multiple_match.shared_match);
    }

    {
        ctx.source = "my_dir/";
        const result = try tryAutocompletePathImpl(&ctx);
        try expectEqual(5, result.multiple_match.potential_matches.len);
        try expectEqualStrings("my_dir/", result.multiple_match.shared_match);
    }

    {
        ctx.source = "my_dir/bob";
        const result = try tryAutocompletePathImpl(&ctx);
        try expectEqual(3, result.multiple_match.potential_matches.len);
        try expectEqualStrings("my_dir/bob", result.multiple_match.shared_match);
    }
}
