const std = @import("std");
const ast = @import("ast.zig");
const builtin = @import("builtin");
const gc = @import("gc.zig");
const pipeline = @import("pipeline.zig");
const repl = @import("repl.zig").repl;
const symtable = @import("symtable.zig");
const tokens = @import("tokens.zig");
const clap = @import("clap");

const assert = std.debug.assert;

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

    // First we specify what parameters our program can take.
    // We can use `parseParamsComptime` to parse a string into an array of `Param(Help)`.
    const params = comptime clap.parseParamsComptime(
        \\-h, --help             Display this help and exit.
        \\    --verbose          Log verbose messages.
        \\    --types            Enables experimental type-checking.
        \\<FILE>                  Script to run.
        \\
    );

    const parsers = comptime .{
        .FILE = clap.parsers.string,
    };

    // Initialize our diagnostics, which can be used for reporting useful errors.
    // This is optional. You can also pass `.{}` to `clap.parse` if you don't
    // care about the extra information `Diagnostics` provides.
    var diag = clap.Diagnostic{};
    var res = clap.parse(clap.Help, &params, parsers, .{
        .diagnostic = &diag,
        .allocator = arena_allocator,
    }) catch |err| {
        // Report useful error and exit.
        diag.report(std.io.getStdErr().writer(), err) catch {};
        return 0;
    };
    defer res.deinit();

    if (res.args.help != 0) {
        try clap.help(std.io.getStdErr().writer(), clap.Help, &params, .{});
        return 0;
    }

    if (res.args.verbose != 0) {
        verboseLexer = true;
        verboseParser = true;
        verboseAnalysis = true;
        verboseInterpretation = true;
        std.debug.print("args: {s}\n", .{args});
    }

    if (res.args.types != 0) {
        useSemanticAnalysis = true;
    }

    if (res.positionals.len > 0) {
        file = res.positionals[0];
    }

    try gc.init(ally);
    defer gc.deinit();

    symtable.init(ally);
    defer symtable.deinit();

    var state = pipeline.State{
        .arena_source = &arena,
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
