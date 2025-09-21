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
pub fn processStrLiteral(pipeline_state: *pipeline.State, arena: std.mem.Allocator, str: []const u8) ![]const u8 {
    const escaped = try escape(arena, str, null);
    const interpolated = try interpolate(arena, escaped);
    const dealiased = try dealias(pipeline_state, arena, interpolated);
    return dealiased;
}

// arena owns result
pub fn processBareword(pipeline_state: *pipeline.State, arena: std.mem.Allocator, str: []const u8) !StringOrGlob {
    const dealiased = try dealias(pipeline_state, arena, str);
    if (!hasMeta(dealiased)) {
        return .{ .string = dealiased };
    }
    const globbed = try glob(arena, dealiased);
    if (globbed.len == 0) {
        return .{ .string = dealiased };
    } else if (globbed.len == 1) {
        return .{ .string = globbed[0] };
    }
    return .{ .glob = globbed };
}

pub fn interpolate(arena: std.mem.Allocator, str: []const u8) ![]u8 {
    var result = try std.ArrayList(u8).initCapacity(arena, str.len * 2);
    defer result.deinit(arena);

    var idx: usize = 0;
    while (idx < str.len) {
        const lbrace = indexOfPos(u8, str, idx, "{") orelse {
            try result.appendSlice(arena, str[idx..]);
            break;
        };

        if (lbrace + 1 < str.len and str[lbrace + 1] == '{') {
            // found escape sequence
            const escape_begin = lbrace + 2;
            const escape_end = indexOfPos(u8, str, lbrace + 1, "}}") orelse return InterpreterError.MismatchedBraces;
            // include the last lbrace
            try result.appendSlice(arena, str[idx .. lbrace + 1]);
            // include the first rbrace
            try result.appendSlice(arena, str[escape_begin .. escape_end + 1]);
            idx = escape_end + 2;
            continue;
        }

        const rbrace = indexOfPos(u8, str, lbrace, "}") orelse return InterpreterError.MismatchedBraces;

        const variable_name = str[lbrace + 1 .. rbrace];
        const variable_value = gc.getSymbol(variable_name) orelse return InterpreterError.BadVariableLookup;

        const variable_string = try variable_value.asStr(arena);

        try result.appendSlice(arena, str[idx..lbrace]);
        try result.appendSlice(arena, variable_string);
        idx = rbrace + 1;
    }
    return try result.toOwnedSlice(arena);
}

pub fn dealias(pipeline_state: *pipeline.State, arena: std.mem.Allocator, str: []const u8) ![]u8 {
    const home = pipeline_state.readEnv("HOME") orelse "";
    const space_home = try std.fmt.allocPrint(arena, " {s}", .{home});

    var buf_len: usize = 0;

    const first_tilde = str.len > 0 and str[0] == '~';

    if (first_tilde) {
        buf_len = str.len + home.len;
        if (buf_len > 0) {
            buf_len -= 1;
        }
    } else {
        buf_len = std.mem.replacementSize(u8, str, " ~", space_home);
    }

    const buffer = try arena.alloc(u8, buf_len);

    if (first_tilde) {
        _ = try std.fmt.bufPrint(buffer, "{s}{s}", .{ home, str[1..] });
        // retry solving all other cases
        return dealias(pipeline_state, arena, buffer);
    }

    _ = std.mem.replace(u8, str, " ~", space_home, buffer);
    return buffer;
}

pub fn escape(arena: std.mem.Allocator, str: []const u8, enclosing_char: ?u8) ![]u8 {
    var result = try std.ArrayList(u8).initCapacity(arena, str.len);
    defer result.deinit(arena);

    //TODO:
    _ = enclosing_char;

    var idx: usize = 0;
    while (idx < str.len) {
        const escapee = indexOfPos(u8, str, idx, "\\") orelse {
            try result.appendSlice(arena, str[idx..]);
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

        try result.appendSlice(arena, str[idx..escapee]);
        try result.append(arena, escapes_into);

        idx = escapee + 2;
    }

    return try result.toOwnedSlice(arena);
}

const GlobChunk = struct {
    star: bool,
    chunk: []const u8,
    rest: []const u8,
};

pub fn match(pattern: []const u8, name: []const u8) bool {
    var my_pattern = pattern;
    var my_name = name;
    root: while (my_pattern.len > 0) {
        const scan = scanChunk(my_pattern);
        const star = scan.star;
        const chunk = scan.chunk;
        my_pattern = scan.rest;

        if (star and chunk.len == 0) {
            // Trailing * matches rest of string unless it has a /
            return std.mem.indexOfScalar(u8, my_name, '/') == null;
        }

        // look for match
        const my_match = matchChunk(chunk, my_name) catch {
            return false;
        };

        if (my_match.ok and (my_match.rest.len == 0 or my_pattern.len > 0)) {
            my_name = my_match.rest;
            continue;
        }

        if (star) {
            // look for match skipping i + 1 bytes
            // cannot skip '/'
            var i: usize = 0;
            while (i < my_name.len and my_name[i] != '/') : (i += 1) {
                const inner_match = matchChunk(chunk, my_name[i + 1 ..]) catch {
                    return false;
                };
                if (inner_match.ok) {
                    if (my_pattern.len == 0 and inner_match.rest.len > 0) {
                        continue;
                    }
                    my_name = inner_match.rest;
                    continue :root;
                }
            }
        }
        return false;
    }
    return my_name.len == 0;
}

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

fn matchChunk(chunk: []const u8, s: []const u8) !MatchChunkResult {
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
                    const n = try algo.readFirstUtf8Len(my_s);
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

pub fn glob(arena: std.mem.Allocator, pattern: []const u8) ![]const []const u8 {
    return try globWithLimit(arena, pattern, 0);
}

fn globWithLimit(arena: std.mem.Allocator, pattern: []const u8, depth: usize) ![]const []const u8 {
    const max_path_separators = 10000;
    if (depth >= max_path_separators) {
        return &.{};
    }

    if (!hasMeta(pattern)) {
        _ = std.fs.cwd().statFile(pattern) catch {
            return &.{};
        };
        return &.{pattern};
    }

    const split = algo.splitPathIntoDirAndFile(pattern);
    const dir = cleanGlobPath(split.dir);

    var matches = std.ArrayList([]const u8).empty;

    if (!hasMeta(dir)) {
        try globImpl(arena, dir, split.file, &matches);
        return try matches.toOwnedSlice(arena);
    }

    if (std.mem.eql(u8, dir, pattern)) {
        return &.{};
    }

    const child_matches = try globWithLimit(arena, dir, depth + 1);
    for (child_matches) |child_match| {
        globImpl(arena, child_match, pattern, &matches) catch {
            return try matches.toOwnedSlice(arena);
        };
    }
    return try matches.toOwnedSlice(arena);
}

fn globImpl(alloc: std.mem.Allocator, dir: []const u8, pattern: []const u8, matches: *std.ArrayList([]const u8)) !void {
    var d = std.fs.cwd().openDir(dir, .{ .iterate = true }) catch return;
    defer d.close();
    var it = d.iterate();
    while (try it.next()) |ent| {
        const path = try std.fs.path.join(alloc, &.{ dir, ent.name });
        if (match(pattern, ent.name)) {
            try matches.append(alloc, path);
        }
    }
    //TODO: maybe place sort elsewhere, idk
    std.mem.sort([]const u8, matches.items, {}, cmp);
}

fn cmp(_: void, lhs: []const u8, rhs: []const u8) bool {
    return std.mem.order(u8, lhs, rhs).compare(.lt);
}

fn cleanGlobPath(path: []const u8) []const u8 {
    if (path.len == 0) {
        return ".";
    } else if (std.mem.eql(u8, path, "/")) {
        return path;
    } else {
        return path[0 .. path.len - 1];
    }
}

fn hasMeta(pattern: []const u8) bool {
    return std.mem.indexOfAny(u8, pattern, "*?") != null;
}

const expect = std.testing.expect;
const expectEqualStrings = std.testing.expectEqualStrings;

test "escape newline" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const escaped = try escape(arena.allocator(), "hello\\nworld", null);

    try expectEqualStrings("hello\nworld", escaped);
}

test "escape backslash" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const escaped = try escape(arena.allocator(), "hello\\\\world", null);

    try expectEqualStrings("hello\\world", escaped);
}

test "dealias tilde" {
    pipeline.testState();
    defer pipeline.deinit();
    try pipeline.state.env_map.put("HOME", "/home/person");
    defer pipeline.state.env_map.deinit();

    const dealiased = try dealias(&pipeline.state, pipeline.state.scratch_arena.allocator(), "ls ~/config");

    try expectEqualStrings("ls /home/person/config", dealiased);
}

test "dealias tilde 2" {
    pipeline.testState();
    defer pipeline.deinit();
    try pipeline.state.env_map.put("HOME", "/home/person");
    defer pipeline.state.env_map.deinit();

    const dealiased = try processBareword(&pipeline.state, pipeline.state.scratch_arena.allocator(), "~/config");

    try expectEqualStrings("/home/person/config", dealiased.string);
}

test "interpolate single value" {
    const ally = std.testing.allocator;

    pipeline.testState();
    defer pipeline.deinit();
    try gc.init(ally);
    defer gc.deinit();
    try gc.pushFrame();

    try gc.insertSymbol("y", try gc.string("x", undefined));

    const result = try interpolate(pipeline.state.scratch_arena.allocator(), "x {y}");
    try expectEqualStrings("x x", result);
}

test "interpolate multiple values" {
    const ally = std.testing.allocator;

    pipeline.testState();
    defer pipeline.deinit();
    try gc.init(ally);
    defer gc.deinit();
    try gc.pushFrame();

    try gc.insertSymbol("y", try gc.string("x", undefined));

    const result = try interpolate(pipeline.state.scratch_arena.allocator(), "x {y} {y}");
    try expectEqualStrings("x x x", result);
}

test "interpolate no values" {
    const ally = std.testing.allocator;

    pipeline.testState();
    defer pipeline.deinit();
    try gc.init(ally);
    defer gc.deinit();

    const result = try interpolate(pipeline.state.scratch_arena.allocator(), "x");
    try expectEqualStrings("x", result);
}

test "escape interpolation" {
    const ally = std.testing.allocator;

    pipeline.testState();
    defer pipeline.deinit();
    try gc.init(ally);
    defer gc.deinit();

    const result = try interpolate(pipeline.state.scratch_arena.allocator(), "{{x}}");
    try expectEqualStrings("{x}", result);
}

test "match simple scenarios" {
    try expect(match("", ""));
    try expect(match("a/b", "a/b"));
    try expect(!match("a/b", "a/"));
    try expect(!match("a/b", "a/bb"));
    try expect(match("a/bb", "a/bb"));
    try expect(!match("aa/bb", "a/bb"));
    try expect(match("aa/bb", "aa/bb"));
}

test "match with '?'" {
    try expect(match("a/?b", "a/bb"));
    try expect(!match("a/?", "a/"));
}

test "match with '*'" {
    try expect(match("a/*.jpg", "a/b.jpg"));
    try expect(!match("a/*.jpg", "a/b.png"));
    try expect(match("*.jpg", "b.jpg"));
    try expect(match("*", "abc"));
    try expect(!match("*", "abc/def"));
    try expect(match("*/*", "abc/def"));
    try expect(match("*/*.png", "abc/def.png"));
}

fn mkfile(dir: std.fs.Dir, path: []const u8) !void {
    var file = try dir.createFile(path, .{});
    file.close();
}

test "dealias" {
    const state = &pipeline.state;
    pipeline.testState();
    defer state.deinit();

    const alloc = state.scratch_arena.allocator();
    try state.env_map.put("HOME", "/home");
    const home = state.readEnv("HOME").?;

    {
        const expected = try std.fmt.allocPrint(alloc, "{s}", .{home});
        const d = try dealias(state, alloc, "~");
        try std.testing.expectEqualStrings(expected, d);
    }

    {
        const expected = try std.fmt.allocPrint(alloc, "{s}x", .{home});
        const d = try dealias(state, alloc, "~x");
        try std.testing.expectEqualStrings(expected, d);
    }

    {
        const d = try dealias(state, alloc, "x~");
        try std.testing.expectEqualStrings("x~", d);
    }

    {
        const expected = try std.fmt.allocPrint(alloc, "x {s}", .{home});
        const d = try dealias(state, alloc, "x ~");
        try std.testing.expectEqualStrings(expected, d);
    }

    {
        const expected = try std.fmt.allocPrint(alloc, "x {s}y", .{home});
        const d = try dealias(state, alloc, "x ~y");
        try std.testing.expectEqualStrings(expected, d);
    }

    {
        const expected = try std.fmt.allocPrint(alloc, "x {s} y", .{home});
        const d = try dealias(state, alloc, "x ~ y");
        try std.testing.expectEqualStrings(expected, d);
    }

    {
        const expected = try std.fmt.allocPrint(alloc, "x {s} {s} y", .{ home, home });
        const d = try dealias(state, alloc, "x ~ ~ y");
        try std.testing.expectEqualStrings(expected, d);
    }

    {
        const expected = try std.fmt.allocPrint(alloc, "{s} {s} {s} {s}", .{ home, home, home, home });
        const d = try dealias(state, alloc, "~ ~ ~ ~");
        try std.testing.expectEqualStrings(expected, d);
    }
}

test "globbing" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();
    var tmpDir = std.testing.tmpDir(.{});
    try tmpDir.dir.setAsCwd();
    try tmpDir.dir.makePath("data/images");
    try tmpDir.dir.makePath("data/random");
    try mkfile(tmpDir.dir, "data/images/cat.png");
    try mkfile(tmpDir.dir, "data/images/dog.png");
    try mkfile(tmpDir.dir, "data/images/sprite.bmp");
    try mkfile(tmpDir.dir, "data/dat.txt");
    try mkfile(tmpDir.dir, "data/random/lol.bmp");

    {
        const expected: []const []const u8 = &.{
            "data/images/cat.png",
            "data/images/dog.png",
        };
        const g = glob(alloc, "data/images/*.png");
        try std.testing.expectEqualDeep(expected, g);
    }

    {
        const expected: []const []const u8 = &.{
            "data/dat.txt",
        };
        const g = glob(alloc, "data/*.txt");
        try std.testing.expectEqualDeep(expected, g);
    }

    {
        const expected: []const []const u8 = &.{};
        const g = glob(alloc, "data/*.png");
        try std.testing.expectEqualDeep(expected, g);
    }

    {
        const expected: []const []const u8 = &.{
            "data/images/cat.png",
            "data/images/dog.png",
        };
        const g = glob(alloc, "data/images/???.png");
        try std.testing.expectEqualDeep(expected, g);
    }

    {
        const expected: []const []const u8 = &.{
            "data/images/cat.png",
        };
        const g = glob(alloc, "data/images/??t.png");
        try std.testing.expectEqualDeep(expected, g);
    }

    {
        const expected: []const []const u8 = &.{
            "data/dat.txt",
            "data/images",
            "data/random",
        };
        const g = glob(alloc, "data/*");
        try std.testing.expectEqualDeep(expected, g);
    }
}
