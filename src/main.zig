const std = @import("std");
const ast = @import("ast.zig");
const builtin = @import("builtin");
const gc = @import("gc.zig");
const pipeline = @import("pipeline.zig");
const repl = @import("repl.zig").repl;
const symtable = @import("symtable.zig");
const tokens = @import("tokens.zig");

const assert = std.debug.assert;
const eq = std.mem.eql;

fn in(needle: []const u8, haystack: []const []const u8) bool {
    for (haystack) |hay| {
        if (eq(u8, hay, needle)) {
            return true;
        }
    }
    return false;
}

fn readfile(ally: std.mem.Allocator, name: []const u8) ![]const u8 {
    const file = try std.fs.cwd().openFile(name, .{});
    defer file.close();
    return file.readToEndAlloc(ally, 1_000_000_000);
}

pub fn main() !u8 {
    var base_allocator = switch (builtin.mode) {
        .Debug => std.heap.GeneralPurposeAllocator(.{}){},
        else => .{},
    };

    defer switch (builtin.mode) {
        .Debug => assert(base_allocator.deinit() == .ok),
        else => {},
    };

    const ally = switch (builtin.mode) {
        .Debug => base_allocator.allocator(),
        else => std.heap.page_allocator,
    };

    var arena = std.heap.ArenaAllocator.init(ally);
    defer arena.deinit();
    const arena_allocator = arena.allocator();
    const args = try std.process.argsAlloc(arena_allocator);

    var verboseLexer = false;
    var verboseParser = false;
    var verboseAnalysis = false;
    var verboseInterpretation = false;
    var useSemanticAnalysis = false;

    var file: ?[]const u8 = null;

    for (args[1..args.len]) |arg| {
        if (in(arg, &.{
            "--help",
            "-h",
            "-help",
            "help",
        })) {
            //TODO: show help
            return 0;
        } else if (eq(u8, "--verbose", arg)) {
            verboseLexer = true;
            verboseParser = true;
            verboseAnalysis = true;
            verboseInterpretation = true;
            std.debug.print("args: {s}\n", .{args});
        } else if (eq(u8, "--types", arg)) {
            useSemanticAnalysis = true;
        } else {
            file = arg;
        }
    }

    try gc.init(ally);
    defer gc.deinit();

    symtable.init(ally);
    defer symtable.deinit();

    var state = pipeline.State{
        .arena_source = arena,
        .arena = arena_allocator,
        .ally = ally,
        .source = "",
        .verboseLexer = verboseLexer,
        .verboseParser = verboseParser,
        .verboseAnalysis = verboseAnalysis,
        .verboseInterpretation = verboseInterpretation,
        .useSemanticAnalysis = useSemanticAnalysis,
        .color = std.io.tty.detectConfig(std.io.getStdOut()),
        .filename = file orelse "repl",
        .env_map = try std.process.getEnvMap(ally),
    };

    defer state.env_map.deinit();

    if (file == null) {
        try repl(&state);
        return 0;
    }

    state.source = try readfile(arena_allocator, file.?);

    pipeline.run(&state) catch |err| {
        try pipeline.writeError(&state, err);
        return 1;
    };
    return 0;
}

comptime {
    const refAllDecls = std.testing.refAllDecls;
    refAllDecls(@import("algo.zig"));
    refAllDecls(@import("ast.zig"));
    refAllDecls(@import("repl.zig"));
    refAllDecls(@import("strings.zig"));
    refAllDecls(@import("tokens.zig"));
    refAllDecls(@import("value.zig"));
}
