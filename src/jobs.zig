const std = @import("std");
const algo = @import("algo.zig");
const ast = @import("ast.zig");
const gc = @import("gc.zig");
const interpreter = @import("interpreter.zig");
const pipeline = @import("pipeline.zig");
const tokens = @import("tokens.zig");
const values = @import("value.zig");

const InterpreterError = interpreter.InterpreterError;
const Result = values.Result;
const State = pipeline.State;
const Value = values.Value;

const assert = std.debug.assert;
const multiple = values.multiple;
const nothing = values.nothing;

const EvalError = interpreter.EvalError;

pub fn exec(state: *State, call: ast.Call, arena: std.mem.Allocator) EvalError!Result {
    const capturing = call.capturing_external_cmd;
    var procs = try std.ArrayList(std.process.Child).initCapacity(arena, 4);
    var redirect_output_to_file: ?[]const u8 = null;
    var redirect_output_to_file_token: ?*const tokens.Token = null;
    const redirect_stdin_from_file = call.redirect_in;

    var ptr: ?*const ast.Call = &call;
    while (ptr) |call_ptr| {
        var new_proc = try proc(state, call_ptr, arena);
        new_proc.env_map = &state.env_map;

        try procs.append(arena, new_proc);

        if (call_ptr.redirect_out) |out| {
            redirect_output_to_file_token = out.token;
            redirect_output_to_file = (try interpreter.contextualizeToken(state.interpreter, out.token, arena)).as.string;
        }

        ptr = call_ptr.pipe;
    }

    const redirecting_out = redirect_output_to_file != null;
    const redirecting_in = redirect_stdin_from_file != null;

    for (procs.items, 0..) |*p, idx| {
        if (capturing or redirecting_out or procs.items.len > 1) {
            if (idx > 0 or redirecting_in) {
                p.stdin_behavior = .Pipe;
            }

            const piping = idx + 1 < procs.items.len;

            if (capturing or redirecting_out or piping) {
                p.stdout_behavior = .Pipe;
            }
        }
    }

    for (procs.items) |*p| {
        p.spawn() catch |e| {
            return errorProcFailed(state, call.token, p, e, arena);
        };
    }

    var capture: ?[]const u8 = null;
    errdefer if (capture) |c| gc.allocator().free(c);

    if (redirect_stdin_from_file) |stdin_file| {
        const expr = stdin_file.expression.*;
        const file_name = switch (try interpreter.evalExpression(state.interpreter, expr, .{})) {
            .value => |val| try val.asStr(gc.allocator()),
            .nothing => {
                return interpreter.errRequiresValue(state.interpreter, ast.tokenFromExpr(expr));
            },
            .values => {
                return interpreter.errRequiresSingleValue(state.interpreter, ast.tokenFromExpr(expr));
            },
        };
        defer gc.allocator().free(file_name);
        var file = std.fs.cwd().openFile(file_name, .{}) catch {
            state.reportError(.{
                .trailing = false,
                .offending_token = ast.tokenFromExpr(expr),
                .msg = try std.fmt.allocPrint(arena, "Could not open file '{s}' for redirection", .{file_name}),
            });
            return InterpreterError.UnableToOpenFileDuringRedirect;
        };
        defer file.close();

        const first_proc = &procs.items[0];
        var reader_buffer: [1024]u8 = undefined;
        var writer_buffer: [1024]u8 = undefined;
        var reader = file.reader(&reader_buffer);
        var writer = first_proc.stdin.?.writer(&writer_buffer);
        try algo.pump(&reader.interface, &writer.interface);
        first_proc.stdin.?.close();
        first_proc.stdin = null;
    }

    if (procs.items.len > 1) {
        var idx: usize = 0;
        while (idx < procs.items.len) : (idx += 1) {
            if (idx > 0) {
                var prev = &procs.items[idx - 1];
                var this = &procs.items[idx];
                var reader_buffer: [1024]u8 = undefined;
                var writer_buffer: [1024]u8 = undefined;
                var reader = prev.stdout.?.reader(&reader_buffer);
                var writer = this.stdin.?.writer(&writer_buffer);
                try algo.pump(&reader.interface, &writer.interface);
                if (this.stdin) |in| {
                    in.close();
                    this.stdin = null;
                }
            }
        }
    }

    if (capturing) {
        assert(procs.getLast().stdout != null);
        capture = try procs.getLast().stdout.?.readToEndAlloc(gc.allocator(), std.math.maxInt(u64));
    } else if (redirect_output_to_file) |output_file| {
        var file = std.fs.cwd().createFile(output_file, .{}) catch {
            state.reportError(.{
                .trailing = false,
                .offending_token = redirect_output_to_file_token.?,
                .msg = try std.fmt.allocPrint(arena, "Could not open file '{s}' for redirection", .{output_file}),
            });
            return InterpreterError.UnableToOpenFileDuringRedirect;
        };
        defer file.close();
        var reader_buffer: [1024]u8 = undefined;
        var writer_buffer: [1024]u8 = undefined;
        var reader = procs.getLast().stdout.?.reader(&reader_buffer);
        var writer = file.writer(&writer_buffer);
        try algo.pump(&reader.interface, &writer.interface);
    }

    var last_code: u8 = 0;

    for (procs.items) |*p| {
        const tm = p.wait() catch |e| return errorProcFailed(state, call.token, p, e, arena);
        last_code = switch (tm) {
            .Exited => |e| e,
            .Signal => |e| @truncate(e),
            .Unknown => |e| @truncate(e),
            .Stopped => |e| @truncate(e),
        };
    }

    if (capturing) {
        const opt = gc.ValueOptions{
            .origin = call.token,
            .origin_module = state.interpreter.root_module.filename,
        };
        const vals = try arena.alloc(*Value, 2);
        vals[0] = try gc.allocedString(capture.?, opt);
        vals[1] = try gc.integer(@intCast(last_code), opt);
        return multiple(vals);
    }
    return nothing;
}

pub fn proc(state: *State, call: *const ast.Call, arena: std.mem.Allocator) !std.process.Child {
    const name = call.token.value;
    var args = try std.ArrayList([]const u8).initCapacity(arena, call.arguments.len);

    try args.append(arena, name);
    for (call.arguments) |arg| {
        switch (try interpreter.evalExpression(state.interpreter, arg, .{})) {
            .value => |value| {
                switch (value.as) {
                    .list => |l| {
                        for (l.items) |e| {
                            try args.append(arena, try e.asStr(arena));
                        }
                    },
                    else => try args.append(arena, try value.asStr(arena)),
                }
            },
            .nothing => unreachable, // this construct expects only values
            .values => unreachable, //TODO: consider this
        }
    }

    return std.process.Child.init(args.items, arena);
}

pub fn procFromExprs(value_list: []const *Value, arena: std.mem.Allocator) !std.process.Child {
    var args = try std.ArrayList([]const u8).initCapacity(arena, value_list.len);
    for (value_list) |value| {
        switch (value.as) {
            .list => |l| {
                for (l.items) |e| {
                    try args.append(arena, try e.asStr(arena));
                }
            },
            else => try args.append(arena, try value.asStr(arena)),
        }
    }

    return std.process.Child.init(args.items, arena);
}

pub fn errorProcFailed(
    state: *State,
    token: *const tokens.Token,
    p: *std.process.Child,
    e: std.process.Child.WaitError,
    arena: std.mem.Allocator,
) (std.process.Child.WaitError || InterpreterError) {
    switch (e) {
        error.FileNotFound => {
            state.reportError(.{
                .trailing = false,
                .offending_token = token,
                .msg = try std.fmt.allocPrint(arena, "Could not find command in system", .{}),
            });
            return error.CommandNotFound;
        },
        error.AccessDenied => {
            state.reportError(.{
                .trailing = false,
                .offending_token = token,
                .msg = try std.fmt.allocPrint(arena, "Unable to run '{s}', permission denied", .{p.argv[0]}),
            });
            return error.CommandNotFound;
        },
        else => {},
    }
    return e;
}
