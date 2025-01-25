const std = @import("std");

pub fn compareStrings(_: void, lhs: []const u8, rhs: []const u8) bool {
    return std.mem.order(u8, lhs, rhs) == .lt;
}

pub fn indexOfDiffStrings(strings: []const []const u8) usize {
    if (strings.len == 0) {
        return 0;
    }

    const first_string = strings[0];
    var deepest_common_diff = first_string.len;

    for (strings) |str| {
        const diff = std.mem.indexOfDiff(u8, first_string, str) orelse str.len;
        deepest_common_diff = @min(deepest_common_diff, diff);
    }

    return deepest_common_diff;
}

pub fn lengthOfLongestSlice(slices: []const []const u8) usize {
    var longest: usize = 0;
    for (slices) |slice| {
        if (slice.len > longest) {
            longest = slice.len;
        }
    }
    return longest;
}

pub fn readfile(ally: std.mem.Allocator, name: []const u8) ![]const u8 {
    const file = try std.fs.cwd().openFile(name, .{});
    defer file.close();
    return file.readToEndAlloc(ally, 1_000_000_000);
}

pub fn readFirstUtf8Len(slice: []const u8) !u64 {
    var view = try std.unicode.Utf8View.init(slice);
    var it = view.iterator();
    const codepoint = it.nextCodepointSlice() orelse {
        return 0;
    };
    return codepoint.len;
}

pub fn splitPathIntoDirAndFile(path: []const u8) struct {
    dir: []const u8,
    file: []const u8,
} {
    const idx = std.mem.lastIndexOfScalar(u8, path, '/') orelse {
        return .{
            .dir = "",
            .file = path,
        };
    };
    return .{
        .dir = path[0 .. idx + 1],
        .file = path[idx + 1 ..],
    };
}

const expectEqual = std.testing.expectEqual;
const expectEqualStrings = std.testing.expectEqualStrings;

test "index of diff strings" {
    const strings_0 = &.{
        "abc",
        "abb",
        "aaa",
    };

    const strings_1 = &.{
        "abc",
        "abb",
        "aba",
    };

    const strings_2 = &.{
        "abc",
        "abb",
        "x",
    };

    try expectEqual(1, indexOfDiffStrings(strings_0));
    try expectEqual(2, indexOfDiffStrings(strings_1));
    try expectEqual(0, indexOfDiffStrings(strings_2));
}

test "splitPathIntoDirAndFile" {
    const a = splitPathIntoDirAndFile("aaa/bbb");
    try expectEqualStrings("aaa/", a.dir);
    try expectEqualStrings("bbb", a.file);

    const b = splitPathIntoDirAndFile("bbb");
    try expectEqualStrings("", b.dir);
    try expectEqualStrings("bbb", b.file);
}
