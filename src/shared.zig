const std = @import("std");
const gc = @import("gc.zig");
const symtable = @import("symtable.zig");
const interpreter = @import("interpreter.zig");
const pipeline = @import("pipeline.zig");

const InterpreterError = interpreter.InterpreterError;
const indexOfPos = std.mem.indexOfPos;

// consider the string
//
// "~/my-parent/{target-dir}/*.txt"
//
// ~          - home directory
// target-dir - user defined variable
// *          - file glob

pub fn contextualizeStr(pipeline_state: *pipeline.State, str: []const u8) ![]u8 {
    var arena = std.heap.ArenaAllocator.init(pipeline_state.arena);
    defer arena.deinit();
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
        const variable_value = symtable.get(variable_name) orelse return InterpreterError.BadVariableLookup;

        //TODO: arena allocator candidate
        const variable_string = try variable_value.asStr(arena);
        defer arena.free(variable_string);

        try result.appendSlice(str[idx..lbrace]);
        try result.appendSlice(variable_string);
        idx = rbrace + 1;
    }
    return try result.toOwnedSlice();
}

pub fn dealias(pipeline_state: *pipeline.State, arena: std.mem.Allocator, str: []const u8) ![]u8 {
    const home = pipeline_state.readEnv("HOME") orelse "";
    if (!std.mem.startsWith(u8, str, "~")) {
        return str;
    }
    return try std.fmt.allocPrint(arena, "{s}{s}", .{ home, str });
}

pub fn escape(arena: std.mem.Allocator, str: []const u8) ![]u8 {
    var result = try std.ArrayList(u8).initCapacity(arena, str.len * 2);
    defer result.deinit();

    var idx: usize = 0;
    while (idx < str.len) {
        const escapee = indexOfPos(u8, str, idx, "\\") orelse {
            try result.appendSlice(str[idx..]);
            break;
        };

        if (escapee + 1 >= str.len) {
            unreachable; // error, escapes nothing
        }

        switch (str[escapee + 1]) {}
    }
}
