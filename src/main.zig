const std = @import("std");
const algo = @import("algo.zig");
const ast = @import("ast.zig");
const build_options = @import("build_options");
const builtin = @import("builtin");
const clap = @import("clap");
const gc = @import("gc.zig");
const pipeline = @import("pipeline.zig");
const repl = @import("repl.zig").repl;
const tokens = @import("tokens.zig");
const terminal = @import("term.zig");
const modules = @import("modules.zig");

const Value = @import("value.zig").Value;

const git_latest_commit_hash = std.mem.trim(u8, @embedFile("git_latest_commit_hash"), " \n\r\t");

const assert = std.debug.assert;

pub var active_term: ?*terminal.Term = null;

pub fn panic(msg: []const u8, _: ?*std.builtin.StackTrace, addr: ?usize) noreturn {
    if (active_term) |term| {
        term.restore();
    }
    std.debug.print("\n\n--- Runtime panic ---\n\n", .{});
    std.debug.defaultPanic(msg, addr);
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
    var remaining_args: []const []const u8 = &.{};

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
    const diagnostic = switch (builtin.mode) {
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
        if (res.positionals.len > 1) {
            remaining_args = res.positionals[1..];
        }
    }

    pipeline.init(pipeline.State{
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
        .remaining_args = remaining_args,
    });
    defer pipeline.deinit();

    try gc.init(ally);
    defer gc.deinit();

    try modules.init();
    // no cleanup, this is garbage collected

    if (file == null) {
        try repl(ally);
        return 0;
    }

    const source = try algo.readfile(arena_allocator, file.?);

    pipeline.run(.{
        .root_module_name = file.?,
        .root_module_source = source,
        .root_scope_already_exists = false,
    }) catch |err| {
        try pipeline.writeError(err);
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
