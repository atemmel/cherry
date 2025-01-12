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

pub const InternalModule = struct {
    builtins_table: builtins.BuiltinsTable,
    was_imported: bool = false,
};

const internal_module_table = std.StaticStringMap(*InternalModule).initComptime(&.{
    .{ "fs", &fs_module },
});

pub fn lookup(module_name: []const u8) ?*InternalModule {
    return internal_module_table.get(module_name);
}

var fs_module = InternalModule{
    .builtins_table = builtins.BuiltinsTable.initComptime(.{
        .{ "has-program", fs_has_program_info },
    }),
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
    const opt = gc.ValueOptions{
        .origin = call.token,
        .origin_module = state.current_module_in_process,
    };

    const arena = state.scratch_arena.allocator();

    const paths = state.env_map.get("PATH") orelse {
        return something(try gc.boolean(false, opt));
    };

    const program = args[0];

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

fn fsExists(state: *State, args: []const *Value, call: ast.Call) !Result {
    _ = args; // autofix

    const opt = gc.ValueOptions{
        .origin = call.token,
        .origin_module = state.current_module_in_process,
    };
    _ = opt; // autofix

    //std.fs.cwd().
}
