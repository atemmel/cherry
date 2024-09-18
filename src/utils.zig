const std = @import("std");

pub const env = struct {
    pub fn get(key: []const u8) ?[]const u8 {
        const sentinel_slice = std.posix.getenv(key) orelse {
            return null;
        };
        return sentinel_slice;
    }
};
