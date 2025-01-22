const std = @import("std");
const gc = @import("gc.zig");
const interpreter = @import("interpreter.zig");
const pipeline = @import("pipeline.zig");
const algo = @import("algo.zig");

const InterpreterError = interpreter.InterpreterError;
const indexOfPos = std.mem.indexOfPos;

// consider the string
//
// "~/my-parent/{target-dir}/*.txt"
//
// ~          - home directory
// target-dir - user defined variable
// *          - file glob

pub const StringOrGlob = union(enum) {
    string: []const u8,
    glob: []const []const u8,
};

// arena owns result
pub fn processStrLiteral(pipeline_state: *pipeline.State, arena: std.mem.Allocator, str: []const u8) ![]u8 {
    const escaped = try escape(arena, str, null);
    const interpolated = try interpolate(arena, escaped);
    const dealiased = try dealias(pipeline_state, arena, interpolated);
    return dealiased;
}

// arena owns result
pub fn processBareword(pipeline_state: *pipeline.State, arena: std.mem.Allocator, str: []const u8) ![]u8 {
    const dealiased = try dealias(pipeline_state, arena, str);
    return dealiased;
}

pub fn interpolate(arena: std.mem.Allocator, str: []const u8) ![]u8 {
    var result = try std.ArrayList(u8).initCapacity(arena, str.len * 2);
    defer result.deinit();

    var idx: usize = 0;
    while (idx < str.len) {
        const lbrace = indexOfPos(u8, str, idx, "{") orelse {
            try result.appendSlice(str[idx..]);
            break;
        };

        if (lbrace + 1 < str.len and str[lbrace + 1] == '{') {
            // found escape sequence
            const escape_begin = lbrace + 2;
            const escape_end = indexOfPos(u8, str, lbrace + 1, "}}") orelse return InterpreterError.MismatchedBraces;
            // include the last lbrace
            try result.appendSlice(str[idx .. lbrace + 1]);
            // include the first rbrace
            try result.appendSlice(str[escape_begin .. escape_end + 1]);
            idx = escape_end + 2;
            continue;
        }

        const rbrace = indexOfPos(u8, str, lbrace, "}") orelse return InterpreterError.MismatchedBraces;

        const variable_name = str[lbrace + 1 .. rbrace];
        const variable_value = gc.getSymbol(variable_name) orelse return InterpreterError.BadVariableLookup;

        const variable_string = try variable_value.asStr(arena);

        try result.appendSlice(str[idx..lbrace]);
        try result.appendSlice(variable_string);
        idx = rbrace + 1;
    }
    return try result.toOwnedSlice();
}

pub fn dealias(pipeline_state: *pipeline.State, arena: std.mem.Allocator, str: []const u8) ![]u8 {
    const home = pipeline_state.readEnv("HOME") orelse "";

    const buf_size = std.mem.replacementSize(u8, str, "~", home);
    const buffer = try arena.alloc(u8, buf_size);
    _ = std.mem.replace(u8, str, "~", home, buffer);
    return buffer;
}

pub fn escape(arena: std.mem.Allocator, str: []const u8, enclosing_char: ?u8) ![]u8 {
    var result = try std.ArrayList(u8).initCapacity(arena, str.len);
    defer result.deinit();

    //TODO:
    _ = enclosing_char;

    var idx: usize = 0;
    while (idx < str.len) {
        const escapee = indexOfPos(u8, str, idx, "\\") orelse {
            try result.appendSlice(str[idx..]);
            break;
        };

        if (escapee + 1 >= str.len) {
            unreachable; // error, escapes nothing
        }

        const escapes_into: u8 = switch (str[escapee + 1]) {
            '\\' => '\\',
            'n' => '\n',
            // should maybe never happen during runtime, instead is caught during lexing(?)
            else => unreachable,
        };

        try result.appendSlice(str[idx..escapee]);
        try result.append(escapes_into);

        idx = escapee + 2;
    }

    return try result.toOwnedSlice();
}

pub fn glob(arena: std.mem.Allocator, str: []const u8) !StringOrGlob {
    // iterate recursively through directories
    // stop if a directory is not a partial match using `match`
    // collect results into list
    _ = arena;
    return StringOrGlob{
        .string = str,
    };
}

//TODO: write more tests
fn match(pattern: []const u8, name: []const u8) bool {
    var px: usize = 0;
    var nx: usize = 0;
    var nextPx: usize = 0;
    var nextNx: usize = 0;
    while (px < pattern.len or nx < name.len) {
        if (px < pattern.len) {
            const c = pattern[px];
            switch (c) {
                else => { // ordinary character
                    if (nx < name.len and name[nx] == c) {
                        px += 1;
                        nx += 1;
                        continue;
                    }
                },
                '?' => { // single-character wildcard
                    if (nx < name.len) {
                        px += 1;
                        nx += 1;
                        continue;
                    }
                },
                '*' => { // zero-or-more-character wildcard
                    // Try to match at nx.
                    // If that doesn't work out,
                    // restart at nx+1 next.
                    nextPx = px;
                    nextNx = nx + 1;
                    px += 1;
                    continue;
                },
            }
        }
        // Mismatch. Maybe restart.
        if (0 < nextNx and nextNx <= name.len) {
            px = nextPx;
            nx = nextNx;
            continue;
        }
        return false;
    }
    // Matched all of pattern to all of name. Success.
    return true;
}

const GlobChunk = struct {
    star: bool,
    chunk: []const u8,
    rest: []const u8,
};

fn scanChunk(pattern: []const u8) GlobChunk {
    var my_pattern = pattern;
    var star = false;
    while (my_pattern.len > 0 and my_pattern[0] == '*') {
        my_pattern = my_pattern[1..];
        star = true;
    }
    var i: usize = 0;
    while (i < my_pattern.len) : (i += 1) {
        switch (my_pattern[i]) {
            '*' => break,
            else => {},
        }
    }
    return .{
        .star = star,
        .chunk = my_pattern[0..i],
        .rest = my_pattern[i..],
    };
}

const MatchChunkResult = struct {
    rest: []const u8,
    ok: bool,
};

fn matchChunk(chunk: []const u8, s: []const u8) MatchChunkResult {
    var failed = false;
    var my_chunk = chunk;
    var my_s = s;
    while (my_chunk.len > 0) {
        if (!failed and my_s.len == 0) {
            failed = true;
        }
        switch (my_chunk[0]) {
            '?' => {
                if (!failed) {
                    if (my_s[0] == '/') {
                        failed = true;
                    }
                    const n = algo.readFirstUtf8Len(my_s);
                    my_s = my_s[n..];
                }
                my_chunk = my_chunk[1..];
            },
            else => {
                if (!failed) {
                    if (my_chunk[0] != my_s[0]) {
                        failed = true;
                    }
                    my_s = my_s[1..];
                }
                my_chunk = my_chunk[1..];
            },
        }
    }
    if (failed) {
        return MatchChunkResult{
            .ok = false,
            .rest = "",
        };
    }
    return MatchChunkResult{
        .ok = true,
        .rest = my_s,
    };
}

test "escape newline" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const escaped = try escape(arena.allocator(), "hello\\nworld", null);

    try std.testing.expectEqualStrings("hello\nworld", escaped);
}

test "escape backslash" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const escaped = try escape(arena.allocator(), "hello\\\\world", null);

    try std.testing.expectEqualStrings("hello\\world", escaped);
}

test "dealias tilde" {
    var state = pipeline.testState();
    defer state.deinit();
    try state.env_map.put("HOME", "/home/person");
    defer state.env_map.deinit();

    const dealiased = try dealias(&state, state.scratch_arena.allocator(), "ls ~/config");

    try std.testing.expectEqualStrings("ls /home/person/config", dealiased);
}

test "dealias tilde 2" {
    var state = pipeline.testState();
    defer state.deinit();
    try state.env_map.put("HOME", "/home/person");
    defer state.env_map.deinit();

    const dealiased = try processBareword(&state, state.scratch_arena.allocator(), "~/config");

    try std.testing.expectEqualStrings("/home/person/config", dealiased);
}

test "interpolate single value" {
    const ally = std.testing.allocator;

    var state = pipeline.testState();
    defer state.deinit();
    try gc.init(ally, &state);
    defer gc.deinit();
    try gc.pushFrame();

    try gc.insertSymbol("y", try gc.string("x", undefined));

    const result = try interpolate(state.scratch_arena.allocator(), "x {y}");
    try std.testing.expectEqualStrings("x x", result);
}

test "interpolate multiple values" {
    const ally = std.testing.allocator;

    var state = pipeline.testState();
    defer state.deinit();
    try gc.init(ally, &state);
    defer gc.deinit();
    try gc.pushFrame();

    try gc.insertSymbol("y", try gc.string("x", undefined));

    const result = try interpolate(state.scratch_arena.allocator(), "x {y} {y}");
    try std.testing.expectEqualStrings("x x x", result);
}

test "interpolate no values" {
    const ally = std.testing.allocator;

    var state = pipeline.testState();
    defer state.deinit();
    try gc.init(ally, &state);
    defer gc.deinit();

    const result = try interpolate(state.scratch_arena.allocator(), "x");
    try std.testing.expectEqualStrings("x", result);
}

test "escape interpolation" {
    const ally = std.testing.allocator;

    var state = pipeline.testState();
    defer state.deinit();
    try gc.init(ally, &state);
    defer gc.deinit();

    const result = try interpolate(state.scratch_arena.allocator(), "{{x}}");
    try std.testing.expectEqualStrings("{x}", result);
}
