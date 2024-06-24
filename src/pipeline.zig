const std = @import("std");
const tokens = @import("tokens.zig");
const ast = @import("ast.zig");
const interpret = @import("interpreter.zig").interpret;

const Token = tokens.Token;

const print = std.debug.print;
const microTimestamp = std.time.microTimestamp;

pub const State = struct {
    arena: std.mem.Allocator,
    ally: std.mem.Allocator,
    source: []const u8,
    tokens: []Token = &.{},
    root: ast.Root = .{
        .statements = &.{},
    },
    verboseLexer: bool,
    verboseParser: bool,
    verboseCodegen: bool,
};

fn logTime(comptime prefix: []const u8, start_ms: i64, stop_ms: i64) void {
    const s = @as(f64, @floatFromInt(stop_ms - start_ms)) / std.time.ms_per_s;
    print(prefix ++ "{d:.3}s\n", .{s});
}

pub fn run(state: *State) !void {
    const lexer_start_ms = microTimestamp();
    state.tokens = try tokens.lex(state);
    const lexer_stop_ms = microTimestamp();
    if (state.verboseLexer) {
        logTime("Lexing:  ", lexer_start_ms, lexer_stop_ms);
        tokens.dump(state);
    }

    const ast_start_ms = microTimestamp();
    state.root = try ast.parse(state);
    const ast_stop_ms = microTimestamp();
    if (state.verboseParser) {
        logTime("Parsing: ", ast_start_ms, ast_stop_ms);
        ast.dump(state.root);
    }

    try interpret(state);
}
