const std = @import("std");
const ast = @import("ast.zig");
const builtin = @import("builtin");
const gc = @import("gc.zig");
const pipeline = @import("pipeline.zig");
const repl = @import("repl.zig").repl;
const symtable = @import("symtable.zig");
const tokens = @import("tokens.zig");
const clap = @import("clap");
const build_options = @import("build_options");

const git_latest_commit_hash = std.mem.trim(u8, @embedFile("git_latest_commit_hash"), " \n\r\t");

const assert = std.debug.assert;

fn readfile(ally: std.mem.Allocator, name: []const u8) ![]const u8 {
    const file = try std.fs.cwd().openFile(name, .{});
    defer file.close();
    return file.readToEndAlloc(ally, 1_000_000_000);
}

pub fn main() !u8 {
    var base_allocator = comptime switch (builtin.mode) {
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
    var verboseGc = false;
    var useSemanticAnalysis = false;

    var file: ?[]const u8 = null;

    const params = comptime clap.parseParamsComptime(
        \\
        \\-h, --help             Display this help and exit.
        \\<FILE>                  Script to run.
        \\
        \\ Dev flags
        \\    --types               (dev) Enables experimental type-checking.
        \\-v, --verbose             (dev) Log verbose messages. (equal to --verbose-ast, --verbose-tokens)
        \\    --verbose-ast         (dev) Log the AST before interpreting, debug builds log a stacktrace of eventual parsing errors.
        \\    --verbose-tokens      (dev) Log the lexed tokens before parsing.
        \\    --verbose-analysis    (dev) Log a summary of the type analysis performed before interpreting.
        \\    --verbose-gc          (dev) Periodically log GC updates during runtime.
    );

    const parsers = comptime .{
        .FILE = clap.parsers.string,
    };

    // Initialize our diagnostics, which can be used for reporting useful errors.
    // This is optional. You can also pass `.{}` to `clap.parse` if you don't
    // care about the extra information `Diagnostics` provides.

    var cl_diagnostics = clap.Diagnostic{};
    const diagnostic = comptime switch (builtin.mode) {
        .Debug => &cl_diagnostics,
        else => null,
    };

    var res = clap.parse(clap.Help, &params, parsers, .{
        .diagnostic = diagnostic,
        .allocator = arena_allocator,
    }) catch |err| {
        // Report useful error and exit.
        if (builtin.mode == .Debug) {
            diagnostic.report(std.io.getStdErr().writer(), err) catch {};
        }
        return 0;
    };
    defer res.deinit();

    if (res.args.help != 0) {
        const writer = std.io.getStdErr().writer();
        writer.print(
            "\n +--------+\n | cherry |    version {s} (built {s})\n +--------+\n\n",
            .{ git_latest_commit_hash, build_options.build_date },
        ) catch {};
        clap.help(writer, clap.Help, &params, .{
            .description_on_new_line = false,
            .spacing_between_parameters = 0,
        }) catch {};
        return 0;
    }

    if (res.args.verbose != 0) {
        verboseLexer = true;
        verboseParser = true;
        verboseInterpretation = true;
        std.debug.print("args: {s}\n", .{args});
    }

    if (res.args.@"verbose-ast" != 0) {
        verboseParser = true;
    }

    if (res.args.@"verbose-tokens" != 0) {
        verboseLexer = true;
    }

    if (res.args.@"verbose-analysis" != 0) {
        verboseAnalysis = true;
    }

    if (res.args.@"verbose-gc" != 0) {
        verboseGc = true;
    }

    if (res.args.types != 0) {
        useSemanticAnalysis = true;
    }

    if (res.positionals.len > 0) {
        file = res.positionals[0];
    }

    var state = pipeline.State{
        .scratch_arena = std.heap.ArenaAllocator.init(ally),
        .modules = std.StringHashMap(pipeline.Module).init(arena_allocator),
        .verboseLexer = verboseLexer,
        .verboseParser = verboseParser,
        .verboseAnalysis = verboseAnalysis,
        .verboseInterpretation = verboseInterpretation,
        .verboseGc = verboseGc,
        .useSemanticAnalysis = useSemanticAnalysis,
        .color = std.io.tty.detectConfig(std.io.getStdOut()),
        .env_map = try std.process.getEnvMap(ally),
    };
    defer state.deinit();

    try gc.init(ally, &state);
    defer gc.deinit();

    symtable.init(ally);
    defer symtable.deinit();

    defer state.env_map.deinit();

    if (file == null) {
        try repl(&state, ally);
        return 0;
    }

    const source = try readfile(arena_allocator, file.?);

    pipeline.run(&state, .{
        .root_module_name = file.?,
        .root_module_source = source,
        .root_scope_already_exists = false,
    }) catch |err| {
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
