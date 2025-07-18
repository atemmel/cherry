const algo = @import("algo.zig");
const builtin = @import("builtin");
const gc = @import("gc.zig");
const pipeline = @import("pipeline.zig");
const std = @import("std");
const strings = @import("strings.zig");
const terminal = @import("term.zig");
const main = @import("main.zig");
const tokens = @import("tokens.zig");

const Token = tokens.Token;
const Term = terminal.Term;
const Color = terminal.Color;
const Hi = terminal.Hi;
const BoldHi = terminal.BoldHi;
const Underline = terminal.Underline;
const startsWith = std.mem.startsWith;

const History = std.ArrayList([]const u8);

const Mode = enum {
    prompt,
    multi_line_prompt,
    search,
};

const State = struct {
    out_writer: std.io.BufferedWriter(4096, std.fs.File.Writer),
    history: History,
    cwd_buffer: [std.fs.max_path_bytes]u8 = undefined,
    cwd: []const u8 = "",
    buffer: [4096]u21 = undefined,
    bytes_buffer: [4096]u8 = undefined,
    bytes_buffer_end_idx: ?usize = null,
    length: usize = 0, // in bytes
    cursor: usize = 0, // in codepoints
    term: Term,
    histfile_path: []const u8 = "",
    history_scroll_idx: usize,
    search_idx: ?usize = null,
    search_reached_end: bool = false,
    rc_path: []const u8 = "",
    persistent_arena: *std.heap.ArenaAllocator,
    repl_loop_arena: std.heap.ArenaAllocator,

    mode: Mode = .prompt,

    auto_cd: bool = true,

    pub fn deinit(self: *State) void {
        self.persistent_arena.deinit();
        self.repl_loop_arena.deinit();
        self.term.restore();
    }

    pub fn writer(self: *State) @TypeOf(self.out_writer.writer()) {
        return self.out_writer.writer();
    }

    pub fn print(self: *State, comptime str: []const u8, args: anytype) void {
        self.out_writer.writer().print(str, args) catch {};
    }

    pub fn flush(self: *State) void {
        self.out_writer.flush() catch {};
    }

    pub fn editIndex(self: *State) usize {
        return switch (self.mode) {
            Mode.prompt, Mode.search => self.cursor,
            Mode.multi_line_prompt => blk: {
                if (std.mem.lastIndexOfScalar(u21, self.line(), '\n')) |idx| {
                    break :blk idx + 1 + self.cursor;
                }
                break :blk self.cursor;
            },
        };
    }

    pub fn clearLine(self: *State) void {
        self.length = switch (self.mode) {
            Mode.prompt, Mode.search => 0,
            Mode.multi_line_prompt => blk: {
                if (std.mem.lastIndexOfScalar(u21, self.line(), '\n')) |idx| {
                    break :blk self.line().len - idx + 1;
                }
                break :blk 0;
            },
        };
    }

    pub fn line(self: *State) []const u21 {
        return self.buffer[0..self.length];
    }

    pub fn lineAsBytes(self: *State) []const u8 {
        // cache latest write to buffer
        if (self.bytes_buffer_end_idx) |idx| {
            return self.bytes_buffer[0..idx];
        }
        const idx = algo.writeU21SliceToU8Slice(self.line(), &self.bytes_buffer);
        self.bytes_buffer_end_idx = idx;
        return self.bytes_buffer[0..idx];
    }

    pub fn calcCwd(self: *State) ![]const u8 {
        self.cwd = try std.fs.cwd().realpath(".", &self.cwd_buffer);
        return self.cwd;
    }

    pub fn writeKeyAtCursor(self: *State, key: u21) void {
        if (self.length + 1 >= self.buffer.len) {
            return;
        }
        std.mem.rotate(u21, self.buffer[self.editIndex() .. self.length + 1], self.length - self.editIndex());
        self.buffer[self.editIndex()] = key;
        self.cursor += 1;
        self.length += 1;
    }

    pub fn removeKeyAtCursor(self: *State) void {
        if (self.cursor <= 0) return;

        const cursor_and_behind = self.buffer[self.editIndex() - 1 .. self.length];
        std.mem.rotate(u21, cursor_and_behind, 1);
        self.cursor -= 1;
        self.length -= 1;
    }

    pub fn removeKeyBehindCursor(self: *State) void {
        if (self.cursor >= self.length) return;

        const behind_cursor = self.buffer[self.editIndex()..self.length];
        std.mem.rotate(u21, behind_cursor, 1);
        self.length -= 1;
    }

    pub fn writePrefix(self: *State) !void {
        const out = self.writer();
        const size = self.term.size();
        terminal.clearLine(out, size.x);
        switch (self.mode) {
            .prompt => {
                try self.writePromptAndLine();
            },
            .multi_line_prompt => {
                try self.writePromptAndMultiline();
            },
            .search => {
                if (self.search_idx) |idx| {
                    if (self.search_reached_end) {
                        fmt(out, "(reverse-search) {s} -> {s} (Search reached end)\r", .{ self.lineAsBytes(), self.history.items[idx] });
                    } else {
                        fmt(out, "(reverse-search) {s} -> {s}\r", .{ self.lineAsBytes(), self.history.items[idx] });
                    }
                } else {
                    fmt(out, "(reverse-search) {s}\r", .{self.lineAsBytes()});
                }
            },
        }
        terminal.moveRight(out, self.prefixLen() + self.cursor);
        self.flush();
    }

    pub fn writePromptAndLine(self: *State) !void {
        const out = self.writer();
        try self.writePrompt();
        const src = self.lineAsBytes();

        const mod = pipeline.runOnlySemantics(.{
            .root_module_name = "interactive",
            .root_module_source = src,
            .arena = &self.repl_loop_arena,
        }) catch |e| switch (e) {
            error.UnterminatedStringLiteral => pipeline.Module{
                .tokens = &[_]Token{},
                .filename = undefined,
                .source = undefined,
                .ast = undefined,
            },
            else => {
                const info = pipeline.state.errorInfo();
                //TODO: check for COLORTERM=truecolor
                fmt(out, "{s}{s}{s}   {s}{s}\r", .{ Hi.red, src, Hi.black, info.msg, Color.white });
                return;
            },
        };
        if (mod.tokens.len == 0) {
            fmt(out, "{s}\r", .{src});
            return;
        }
        var n_token: usize = 0;
        var n_src: usize = 0;
        while (n_token < mod.tokens.len) : (n_token += 1) {
            const token = mod.tokens[n_token];
            const token_idc = switch (token.kind) {
                .StringLiteral => token.indiciesAndSurrounding(src),
                .Variable => token.indiciesAndPrefix(src),
                else => token.indicies(src),
            };
            fmt(out, "{s}", .{src[n_src..token_idc.from]});
            const color = switch (token.kind) {
                .If,
                .Else,
                .For,
                .Fn,
                .In,
                .Return,
                .Break,
                .Continue,
                .True,
                .False,
                .Import,
                .Pub,
                => Color.cyan,
                .Bang,
                .Colon,
                .Semicolon,
                .Assign,
                .Pipe,
                .LParens,
                .RParens,
                .LBrace,
                .RBrace,
                .LBracket,
                .RBracket,
                .RedirectOut,
                .RedirectIn,
                .SingleQuote,
                .Equals,
                .NotEquals,
                .AddAssign,
                .SubAssign,
                .MulAssign,
                .DivAssign,
                .PipeAssign,
                .RightArrow,
                => Hi.white,
                .StringLiteral => Color.green,
                .IntegerLiteral => Color.purple,
                .Variable => BoldHi.blue,
                .Bareword, .EmptyRecord, .Newline => "",
            };
            const color_end = if (color.len > 0) Color.white else "";
            fmt(out, "{s}{s}{s}", .{ color, src[token_idc.from..token_idc.to], color_end });
            n_src = token_idc.to;
        }
        const rem = src[n_src..];
        const maybe_idx = std.mem.indexOfScalar(u8, rem, '#');
        if (maybe_idx) |idx| {
            fmt(out, "{s}{s}{s}{s}\r", .{ rem[0..idx], Hi.black, rem[idx..], Color.white });
        } else {
            fmt(out, "{s}\r", .{rem});
        }
    }

    pub fn writePromptAndMultiline(self: *State) !void {
        const out = self.writer();
        const src = self.lineAsBytes();
        const n = std.mem.lastIndexOfScalar(u8, src, '\n') orelse 0;
        fmt(out, "{s}->{s} {s}\r", .{ Hi.black, Color.white, src[n + 1 ..] });
    }

    pub fn writePrompt(self: *State) !void {
        const home = pipeline.state.homeOrEmpty();
        const out = self.writer();
        const cwd = try self.calcCwd();
        if (startsWith(u8, cwd, home)) {
            const cwd_after_home = cwd[home.len..];
            fmt(out, "~{s} {s}|>{s} ", .{ cwd_after_home, BoldHi.white, Color.gray });
        } else {
            fmt(out, "{s} {s}|>{s} ", .{ cwd, BoldHi.white, Color.gray });
        }
    }

    pub fn prefixLen(self: *State) usize {
        const home = pipeline.state.homeOrEmpty();
        return switch (self.mode) {
            .prompt => if (startsWith(u8, self.cwd, home)) self.cwd.len - home.len + 1 + 4 else self.cwd.len + 4,
            .search => blk: {
                var sum = "(reverse-search) ".len;
                if (self.search_idx) |idx| {
                    sum += " -> ".len + (std.unicode.utf8CountCodepoints(self.history.items[idx]) catch 0);
                    if (self.search_reached_end) {
                        sum += " (Search reached end)".len;
                    }
                }
                break :blk sum;
            },
            .multi_line_prompt => "-> ".len,
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

pub fn repl(persistent_allocator: std.mem.Allocator) !void {
    var persistent_arena = std.heap.ArenaAllocator.init(persistent_allocator);
    var state = State{
        .history = try History.initCapacity(persistent_arena.allocator(), 512),
        .out_writer = std.io.bufferedWriter(std.io.getStdOut().writer()),
        .term = try Term.init(),
        .history_scroll_idx = 0,
        .persistent_arena = &persistent_arena,
        .repl_loop_arena = std.heap.ArenaAllocator.init(persistent_allocator),
    };
    defer state.deinit();

    main.active_term = &state.term;

    const home = pipeline.state.homeOrEmpty();

    if (state.histfile_path.len == 0) {
        state.histfile_path = try std.fs.path.join(
            state.persistent_arena.allocator(),
            &.{ home, ".local/state/cherry-hist" },
        );
    }

    state.rc_path = try std.fs.path.join(
        state.persistent_arena.allocator(),
        &.{ home, ".config/cherryrc" },
    );

    try readHistory(&state);
    state.history_scroll_idx = state.history.items.len;

    try gc.pushFrame();
    defer gc.popFrame();

    try readRc(&state);

    const out = state.writer();

    var sa = std.os.linux.Sigaction{
        .handler = .{ .handler = pipeline.sigintHandler },
        .mask = std.os.linux.empty_sigset,
        .flags = std.os.linux.SA.RESTART | std.os.linux.SA.ONSTACK,
    };

    _ = std.os.linux.sigaction(std.os.linux.SIG.INT, &sa, null);

    while (true) {
        state.bytes_buffer_end_idx = null;
        defer _ = state.repl_loop_arena.reset(.retain_capacity);
        try state.writePrefix();
        const event = try state.term.readEvent();
        switch (event) {
            .key => |k| switch (k) {
                .key => |key| {
                    switch (key) {
                        '\r' => switch (state.mode) {
                            .prompt, .multi_line_prompt => {
                                const result = try eval(&state);
                                switch (result) {
                                    .Ok => {
                                        state.mode = .prompt;
                                    },
                                    .StringNeverClosed => {
                                        state.writeKeyAtCursor('\n');
                                        state.cursor = 0;
                                        state.mode = .multi_line_prompt;
                                    },
                                }
                            },
                            .search => {
                                if (state.search_idx) |idx| { // if holds a match from history
                                    state.flush();
                                    const match = state.history.items[idx];
                                    replaceCommand(&state, match);
                                    state.mode = .prompt;
                                    try state.writePrefix();
                                    _ = try eval(&state);
                                }
                                // otherwise, ignore
                            },
                        },
                        '\n' => {},
                        else => switch (state.mode) {
                            .prompt, .multi_line_prompt => state.writeKeyAtCursor(key),
                            .search => {
                                state.writeKeyAtCursor(key);
                                try reverseSearch(&state, .reset);
                            },
                        },
                    }
                },
                .special => |s| switch (s) {
                    .arrow_down => switch (state.mode) {
                        .prompt, .multi_line_prompt => {
                            nextCommand(&state);
                        },
                        .search => {},
                    },
                    .arrow_up => switch (state.mode) {
                        .prompt, .multi_line_prompt => {
                            previousCommand(&state);
                        },
                        .search => {},
                    },
                    .arrow_left => switch (state.mode) {
                        .prompt, .multi_line_prompt => {
                            if (state.cursor > 0) {
                                state.cursor -= 1;
                            }
                        },
                        .search => {
                            if (state.search_idx) |idx| { // if holds a match from history
                                state.flush();
                                const match = state.history.items[idx];
                                replaceCommand(&state, match);
                                if (state.cursor > 0) {
                                    state.cursor -= 1;
                                }
                                state.mode = .prompt;
                            }
                        },
                    },
                    .arrow_right => switch (state.mode) {
                        .prompt, .multi_line_prompt => {
                            if (state.cursor < state.length) {
                                state.cursor += 1;
                            }
                        },
                        .search => {
                            state.mode = .prompt;
                            if (state.cursor < state.length) {
                                state.cursor += 1;
                            }
                        },
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
                        .prompt, .multi_line_prompt => {
                            try tryAutocomplete(&state);
                        },
                        .search => {},
                    },
                    .escape, .insert, .page_down, .page_up, .unknown => {},
                },
            },
            .ctrl => |c| switch (c) {
                .key => |ctrl| switch (ctrl) {
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
                    'h' => {
                        const last_space = std.mem.lastIndexOfScalar(u21, state.line(), ' ');
                        terminal.clearLine(out, state.length + state.prefixLen());
                        state.length = last_space orelse 0;
                        state.cursor = last_space orelse 0;
                    },

                    'u' => {
                        terminal.clearLine(out, state.length + state.prefixLen());
                        state.clearLine();
                        state.cursor = 0;
                    },
                    'l' => {
                        terminal.moveCursor(state.term.tty.?.writer(), 0, 0);
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
                            .multi_line_prompt => {},
                            .search => {
                                try reverseSearch(&state, .forward);
                            },
                        }
                    },
                    else => {}, // ignore
                },
                .special => |s| switch (s) {
                    .arrow_left => {
                        const idx = std.mem.lastIndexOfScalar(u21, state.buffer[0..state.cursor], ' ') orelse 0;
                        state.cursor = idx;
                    },
                    .arrow_right => {
                        const idx = std.mem.indexOfScalarPos(u21, state.buffer[0..state.length], state.cursor, ' ') orelse state.length - 1;
                        state.cursor = idx + 1;
                    },
                    else => {},
                },
            },
            .alt => {},
        }
    }
}

const EvalResult = enum {
    Ok,
    StringNeverClosed,
};

fn eval(state: *State) !EvalResult {
    // clearing the scratch arena resets GC debug info, among other things
    // only done for release builds
    defer if (builtin.mode != .Debug) {
        _ = pipeline.state.scratch_arena.reset(.retain_capacity);
    };
    appendHistory(state) catch |e| {
        state.print("Could not write history, {any}\r\n", .{e});
        state.flush();
    };
    state.print("\r\n", .{});
    terminal.clearLine(state.writer(), state.prefixLen());
    state.term.restore();
    state.flush();

    const cmd = try aliasLookup(state, state.lineAsBytes());
    const source = try pipeline.state.scratch_arena.allocator().dupe(u8, cmd);

    pipeline.run(.{
        .root_module_name = "interactive",
        .root_module_source = source,
        .root_scope_already_exists = true,
    }) catch |e| {
        if (e == error.UnterminatedStringLiteral) {
            state.term = Term.init() catch unreachable;
            return EvalResult.StringNeverClosed;
        }

        const maybe_module = pipeline.state.modules.get("interactive");
        const all_tokens = if (maybe_module != null) maybe_module.?.tokens else &.{};
        if (pipeline.state.verboseInterpretation) {
            std.debug.print("Error has occured while evaluating: {any}\nTokens: ", .{e});
            for (all_tokens) |tok| {
                std.debug.print("{any}\n", .{tok.kind});
            }
        }
        //TODO: autocd is currently broken, but this should be rewritten regardless
        const should_try_auto_cd = state.auto_cd and all_tokens.len == 1 and all_tokens[0].kind == .Bareword;
        if (should_try_auto_cd and tryAutoCd(state, all_tokens[0].value)) {
            // All ok
        } else {
            pipeline.writeError(e) catch |err| {
                //TODO: handle these
                state.print("Error: {any}\r\n", .{err});
                state.flush();
            };
        }
    };

    state.flush();
    state.term = Term.init() catch unreachable;
    state.length = 0;
    state.cursor = 0;
    return EvalResult.Ok;
}

// returns true if cwd is changed
fn tryAutoCd(state: *State, path: []const u8) bool {
    var dir = std.fs.cwd().openDir(path, .{}) catch {
        // fail silently, maybe the user didn't mean to auto-cd
        return false;
    };
    defer dir.close();
    dir.setAsCwd() catch |err| {
        state.print("auto-cd failed to change directory: {any}\r\n", .{err});
        state.flush();
        return false;
    };
    return true;
}

fn appendHistory(state: *State) !void {
    if (state.line().len == 0) {
        return;
    }

    if (state.history.getLastOrNull()) |prev| {
        if (std.mem.eql(u8, prev, state.lineAsBytes())) {
            return;
        }
    }

    const ally = state.persistent_arena.allocator();

    const cmd_copy = try ally.dupe(u8, state.lineAsBytes());
    try state.history.append(cmd_copy);
    state.history_scroll_idx = state.history.items.len;

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
    const file = std.fs.cwd().openFile(state.histfile_path, .{}) catch |e| switch (e) {
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

    const ally = state.persistent_arena.allocator();

    const underlying_reader = file.reader();
    var buffered_reader = std.io.bufferedReader(underlying_reader);
    const reader = buffered_reader.reader();
    while (true) {
        const line = reader.readUntilDelimiterAlloc(ally, '\n', state.buffer.len) catch return;
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
    if (state.history_scroll_idx >= state.history.items.len - 1) {
        return;
    }
    state.history_scroll_idx += 1;
    const cmd_slice = state.history.items[state.history_scroll_idx];
    replaceCommand(state, cmd_slice);
}

fn replaceCommand(state: *State, cmd: []const u8) void {
    terminal.clearLine(state.writer(), state.prefixLen() + 7 + state.length);
    const len = algo.writeU8SliceToU21Slice(cmd, &state.buffer);
    state.cursor = len;
    state.length = len;
    state.bytes_buffer_end_idx = null;
}

fn tryAutocomplete(state: *State) !void {
    const has_spaces = std.mem.indexOfScalar(u21, state.line(), ' ') != null;

    if (has_spaces) {
        tryAutocompletePath2(state) catch {};
    } else {
        tryAutocompleteCmd2(state) catch {};
    }
}

fn tryAutocompleteCmd2(state: *State) !void {
    const arena = state.repl_loop_arena.allocator();

    const line = state.line();

    var last_cmd_begin = std.mem.lastIndexOfScalar(u21, line, ' ') orelse 0;
    if (last_cmd_begin < line.len and last_cmd_begin != 0) {
        last_cmd_begin += 1;
    }
    const last_cmd = line[last_cmd_begin..];

    var cmp_ctx: CompletionContext = .{
        .source = algo.allocU8SliceFromU21Slice(last_cmd, arena),
        .arena = arena,
    };
    const result = try tryAutocompleteCmdImpl(&cmp_ctx);
    switch (result) {
        .none => {
            try tryAutocompletePath2(state);
            return;
        },
        else => {},
    }
    try displayCompletionResults(state, result);
}

fn tryAutocompletePath2(state: *State) !void {
    const arena = state.repl_loop_arena.allocator();

    const original_line = state.lineAsBytes();
    const processed_bareword = try strings.processBareword(&pipeline.state, arena, original_line);

    const line = switch (processed_bareword) {
        .string => |s| s,
        .glob => {
            return; //TODO: autocompleting this could be cool, maybe
        },
    };

    var last_cmd_begin = std.mem.lastIndexOfScalar(u8, line, ' ') orelse 0;
    if (last_cmd_begin < line.len and last_cmd_begin != 0) {
        last_cmd_begin += 1;
    }
    const last_cmd = line[last_cmd_begin..];

    var cmp_ctx: CompletionContext = .{
        .source = last_cmd,
        .arena = arena,
    };
    const result = try tryAutocompletePathImpl(&cmp_ctx);
    try displayCompletionResults(state, result);
}

fn displayCompletionResults(state: *State, result: CompletionResult) !void {
    const codepoints = std.unicode.utf8CountCodepoints;
    const line = state.line();

    var last_cmd_begin = std.mem.lastIndexOfScalar(u21, line, ' ') orelse 0;
    if (last_cmd_begin < line.len and last_cmd_begin != 0) {
        last_cmd_begin += 1;
    }
    const last_cmd = line[last_cmd_begin..];
    const out = state.writer();
    switch (result) {
        .none => {},
        .single_match => |str| {
            const offset = line.len - last_cmd_begin;

            defer {
                state.length += (codepoints(str) catch @panic("Bad unicode byte")) - offset;
                state.cursor += (codepoints(str) catch @panic("Bad unicode byte")) - offset;
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

            if (stat.kind == .directory) {
                //TODO: make bufPrint to state.buffer-pattern into separate function
                const len = algo.writeU8SliceToU21Slice(str, state.buffer[state.length - offset ..]);
                state.buffer[state.length - offset + len] = '/';

                state.length += 1;
                state.cursor += 1;
            } else {
                _ = algo.writeU8SliceToU21Slice(str, state.buffer[state.length - offset ..]);
            }
        },
        // currently presents completion results like bash, in a table-like format
        .multiple_match => |all| {
            const delta = all.shared_match.len - last_cmd.len;
            if (delta > 0) {
                const shared_match_substr = all.shared_match[last_cmd.len..];
                _ = algo.writeU8SliceToU21Slice(shared_match_substr, state.buffer[state.length..]);
                state.length += delta;
                state.cursor += delta;
            }

            const terminal_size = state.term.size();
            const longest_match = algo.lengthOfLongestSlice(all.potential_matches);

            const column_width = longest_match + 2;

            const n_columns = @divTrunc(terminal_size.x, column_width);
            const max_items_per_column = @divTrunc(all.potential_matches.len, @max(3, n_columns) - 2);

            for (0..max_items_per_column + 2) |_| {
                fmts(out, "\r\n");
            }
            terminal.moveUp(out, max_items_per_column + 2);
            state.flush();

            const cursor_position = try state.term.getCursor();
            const y_begin = cursor_position.y + 1;

            var x: usize = 0;
            var y: usize = y_begin;

            for (all.potential_matches) |match| {
                terminal.moveCursor(out, y, x);
                fmt(out, "{s}", .{match});

                y += 1;
                if (y > max_items_per_column + y_begin) {
                    x += column_width;
                    y = y_begin;
                }
            }

            terminal.moveCursor(out, y_begin + max_items_per_column + 1, 0);
            state.flush();
        },
    }
}

const CompletionContext = struct {
    source: []const u8,
    arena: std.mem.Allocator,
};

const CompletionResult = union(enum) {
    single_match: []const u8,
    multiple_match: struct {
        shared_match: []const u8,
        potential_matches: []const []const u8,
    },
    none: void,
};

fn tryAutocompletePathImpl(ctx: *CompletionContext) !CompletionResult {
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
    return listToResult(result_list);
}

fn tryAutocompleteCmdImpl(ctx: *CompletionContext) !CompletionResult {
    if (ctx.source.len == 0) {
        return .none;
    }
    const path = std.posix.getenv("PATH") orelse {
        return .none;
    };

    var result_list = std.ArrayList([]const u8).init(ctx.arena);
    var it = std.mem.tokenizeScalar(u8, path, ':');

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

            if (!startsWith(u8, entry.name, ctx.source)) {
                continue;
            }

            const stat = dir.statFile(entry.name) catch {
                continue;
            };

            if (stat.mode & 0b1 == 0) {
                continue;
            }
            try result_list.append(try ctx.arena.dupe(u8, entry.name));
        }
    }

    std.mem.sort([]const u8, result_list.items, {}, algo.compareStrings);
    var idx: usize = 1;
    while (idx < result_list.items.len) {
        const previous = result_list.items[idx - 1];
        const current = result_list.items[idx];

        if (std.mem.eql(u8, previous, current)) {
            _ = result_list.orderedRemove(idx);
        } else {
            idx += 1;
        }
    }
    return listToResult(result_list);
}

fn listToResult(list: std.ArrayList([]const u8)) CompletionResult {
    return switch (list.items.len) {
        0 => .none,
        1 => .{ .single_match = list.items[0] },
        else => blk: {
            const last_shared_idx = algo.indexOfDiffStrings(list.items);
            const shared_match_slice = list.items[0][0..last_shared_idx];
            break :blk .{
                .multiple_match = .{
                    .shared_match = shared_match_slice,
                    .potential_matches = list.items,
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

fn dirLookup(ctx: *CompletionContext) !DirLookup {
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
        const has_substring = std.mem.indexOfPos(u8, state.history.items[start_index - 1], 0, state.lineAsBytes()) != null;
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
    const file = std.fs.cwd().openFile(state.rc_path, .{}) catch |e| switch (e) {
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

    const ally = state.persistent_arena.allocator();
    const rc_src = file.readToEndAlloc(ally, 1_000_000_000) catch return;

    state.term.restore();
    defer {
        state.term = Term.init() catch unreachable;
        state.length = 0;
        state.cursor = 0;
    }
    state.flush();

    pipeline.run(.{
        .root_module_name = "cherryrc",
        .root_module_source = rc_src,
        .root_scope_already_exists = true,
    }) catch |e| {
        state.print("Unexpected error when reading .cherryrc at {s}: {}\r\n", .{ state.rc_path, e });
    };
    state.flush();
}

fn aliasLookup(state: *State, cmd_arg: []const u8) ![]const u8 {
    var cmd = cmd_arg;

    const aliases = gc.aliases.items;
    if (aliases.len == 0) {
        return cmd;
    }

    var it = std.mem.reverseIterator(aliases);
    while (it.next()) |alias| {
        if (!startsWith(u8, cmd, alias.from)) {
            continue;
        }
        if (cmd.len == alias.from.len or cmd[alias.from.len] == ' ') {
            cmd = try std.fmt.allocPrint(
                state.repl_loop_arena.allocator(),
                "{s}{s}",
                .{ alias.to, cmd[alias.from.len..] },
            );
        }
    }
    return cmd;
}

const pipelineTestState = pipeline.testState;
const expectEqual = std.testing.expectEqual;
const expectEqualStrings = std.testing.expectEqualStrings;

fn testReplState() State {
    var state = State{
        .persistent_arena = undefined,
        .repl_loop_arena = std.heap.ArenaAllocator.init(std.testing.allocator),
        .out_writer = undefined,
        .history = undefined,
        .term = Term{
            .tty = null,
            .original_termios = undefined,
            .state = undefined,
        },
        .history_scroll_idx = undefined,
    };
    state.persistent_arena = &state.repl_loop_arena;
    return state;
}

test "Single alias lookup" {
    pipelineTestState();
    try gc.init(std.testing.allocator);
    defer gc.deinit();

    var state = testReplState();
    defer state.deinit();

    const cmd = "ls";

    try gc.aliases.append(.{
        .from = "ls",
        .to = "ls \"--color=auto\"",
    });

    try expectEqualStrings("ls \"--color=auto\"", try aliasLookup(&state, cmd));
}

test "Single alias lookup failure" {
    pipelineTestState();
    try gc.init(std.testing.allocator);
    defer gc.deinit();
    var state = testReplState();
    defer state.deinit();

    const cmd = "lsblk";

    try gc.aliases.append(.{
        .from = "ls",
        .to = "ls \"--color=auto\"",
    });

    try expectEqualStrings("lsblk", try aliasLookup(&state, cmd));
}

test "Single alias lookup success but keeps arg" {
    pipelineTestState();
    try gc.init(std.testing.allocator);
    defer gc.deinit();
    var state = testReplState();
    defer state.deinit();

    const cmd = "ls /tmp";

    try gc.aliases.append(.{
        .from = "ls",
        .to = "ls \"--color=auto\"",
    });

    try expectEqualStrings("ls \"--color=auto\" /tmp", try aliasLookup(&state, cmd));
}

test "Nested alias lookup success" {
    pipelineTestState();
    try gc.init(std.testing.allocator);
    defer gc.deinit();
    var state = testReplState();
    defer state.deinit();

    const cmd = "ll";

    try gc.aliases.appendSlice(&.{
        .{ .from = "ls", .to = "ls \"--color=auto\"" },
        .{ .from = "ll", .to = "ls -l" },
    });

    try expectEqualStrings("ls \"--color=auto\" -l", try aliasLookup(&state, cmd));
}

test "autocomplete simple cases" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var ctx: CompletionContext = .{
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
    var ctx: CompletionContext = .{
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
    var ctx: CompletionContext = .{
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
    var ctx: CompletionContext = .{
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
