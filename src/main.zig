const std = @import("std");
const tokens = @import("tokens.zig");
const ast = @import("ast.zig");
const pipeline = @import("pipeline.zig");
const repl = @import("repl.zig").repl;
const symtable = @import("symtable.zig");
const gc = @import("gc.zig");
const builtin = @import("builtin");

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

pub fn main() !void {
    var base_allocator = switch (builtin.mode) {
        .Debug => std.heap.GeneralPurposeAllocator(.{}){},
        else => std.heap.page_allocator,
    };
    defer std.debug.assert(base_allocator.deinit() == .ok);
    const ally = base_allocator.allocator();
    var arena = std.heap.ArenaAllocator.init(ally);
    defer arena.deinit();
    const arena_allocator = arena.allocator();
    const args = try std.process.argsAlloc(arena_allocator);

    var verboseLexer = false;
    var verboseParser = false;
    var verboseCodegen = false;
    var file: ?[]const u8 = null;

    for (args[1..args.len]) |arg| {
        if (in(arg, &.{
            "--help",
            "-h",
            "-help",
            "help",
        })) {
            //TODO: show help
            return;
        } else if (eq(u8, "--verbose", arg)) {
            verboseLexer = true;
            verboseParser = true;
            verboseCodegen = true;
            std.debug.print("args: {s}\n", .{args});
        } else {
            file = arg;
        }
    }

    try gc.init(ally);
    defer gc.deinit();

    symtable.init(ally);
    defer symtable.deinit();

    if (file == null) {
        return repl(ally, arena_allocator);
    }

    const source = try readfile(arena_allocator, file.?);

    var state = pipeline.State{
        .arena = arena_allocator,
        .ally = ally,
        .source = source,
        .verboseCodegen = verboseCodegen,
        .verboseLexer = verboseLexer,
        .verboseParser = verboseParser,
    };

    try pipeline.run(&state);
}

comptime {
    const refAllDecls = std.testing.refAllDecls;

    refAllDecls(@import("tokens.zig"));
    refAllDecls(@import("ast.zig"));
    refAllDecls(@import("value.zig"));
}
