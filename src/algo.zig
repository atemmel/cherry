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

const expectEqual = std.testing.expectEqual;

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
