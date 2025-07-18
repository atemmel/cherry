const std = @import("std");
const tokens = @import("tokens.zig");
const ast = @import("ast.zig");
const builtins = @import("builtins.zig");
const pipeline = @import("pipeline.zig");
const gc = @import("gc.zig");
const values = @import("value.zig");

const Value = values.Value;
const Result = values.Result;
const State = pipeline.State;

const something = values.something;
const nothing = values.nothing;

const StaticSymbols = std.StaticStringMap(*Value);

const internal_token = tokens.Token{
    .kind = .Bareword,
    .value = "???",
};

const no_builtins = builtins.BuiltinsTable.initComptime(.{});
const no_symbols = StaticSymbols.initComptime(.{});
const no_tracing = gc.ValueOptions{ .origin_module = "internal", .origin = &internal_token };

pub const InternalModule = struct {
    builtins_table: builtins.BuiltinsTable,
    record: *values.Value,
    was_imported: bool = false,
};

const internal_module_table = std.StaticStringMap(*InternalModule).initComptime(&.{
    .{ "fs", &fs_module },
    .{ "completions", &completions_module },
});

pub fn lookup(module_name: []const u8) ?*InternalModule {
    return internal_module_table.get(module_name);
}

const ExportedVariable = struct {
    name: []const u8,
};

var fs_module = InternalModule{
    .builtins_table = builtins.BuiltinsTable.initComptime(.{
        .{ "exists", fs_exists },
        .{ "has-program", fs_has_program_info },
    }),
    .record = undefined,
};

const fs_has_program_info = builtins.BuiltinInfo{
    .func = fsHasProgram,
    .signature = .{
        .parameters = &.{
            .{
                .name = "program-name",
                .param_type = .{
                    .type_info = .string,
                },
            },
        },
        .produces = .boolean,
    },
};

fn fsHasProgram(state: *State, args: []const *Value, call: ast.Call) !Result {
    try builtins.validateArgsCount(state, &.{1}, args.len);
    const opt = gc.ValueOptions{
        .origin = call.token,
        .origin_module = state.current_module_in_process,
    };

    const arena = state.scratch_arena.allocator();

    const paths = state.env_map.get("PATH") orelse {
        return something(try gc.boolean(false, opt));
    };

    const program = switch (args[0].as) {
        .string => |s| s,
        else => return builtins.typeMismatchError(state, "string", args[0].kindName(), 0),
    };

    var it = std.mem.tokenizeScalar(u8, paths, ':');
    while (it.next()) |path| {
        const full_path = try std.fmt.allocPrint(arena, "{s}/{s}", .{ path, program });
        const stat_info = std.fs.cwd().statFile(full_path) catch {
            continue;
        };

        const ok_kind = stat_info.kind == .file or stat_info.kind == .sym_link;
        const ok_mode = stat_info.mode & 0o111 != 0;

        if (ok_kind and ok_mode) {
            return something(try gc.boolean(true, opt));
        }
    }
    return something(try gc.boolean(false, opt));
}

const fs_exists = builtins.BuiltinInfo{
    .func = fsExists,
    .signature = .{
        .parameters = &.{
            .{
                .name = "path",
                .param_type = .{
                    .type_info = .string,
                },
            },
        },
        .produces = .boolean,
    },
};

fn fsExists(state: *State, args: []const *Value, call: ast.Call) !Result {
    try builtins.validateArgsCount(state, &.{1}, args.len);
    const opt = gc.ValueOptions{
        .origin = call.token,
        .origin_module = state.current_module_in_process,
    };

    const path: []const u8 = switch (args[0].as) {
        .string => |s| s,
        else => return builtins.typeMismatchError(state, "string", args[0].kindName(), 0),
    };

    std.fs.cwd().access(path, .{}) catch {
        return something(try gc.boolean(false, opt));
    };
    return something(try gc.boolean(true, opt));
}

var completions_module = InternalModule{
    .builtins_table = no_builtins,
    .record = undefined,
};

pub fn init() !void {
    for (internal_module_table.values()) |*mod| {
        mod.*.record = try gc.emptyRecord(no_tracing);
    }

    // completions
    {
        const completions = internal_module_table.get("completions").?;
        try completions.record.as.record.putNoClobber("map", try gc.emptyRecord(no_tracing));
    }
}
