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
        .functions = undefined,
    },
    verboseLexer: bool = false,
    verboseParser: bool = false,
    verboseCodegen: bool = false,
    color: std.io.tty.Config = std.io.tty.Config.no_color,
    error_report: ?struct {
        offending_token: *const Token,
        msg: []const u8,
        trailing: bool,
    } = null,

    const ErrorInfo = struct {
        msg: []const u8,
        col: usize,
        row: usize,
        row_str: []const u8,
        row_error_token_idx: usize,
        trailing_error: bool,
    };

    pub fn errorInfo(state: *const State) ErrorInfo {
        const report = state.error_report orelse @panic("No error stored");
        const bad_token = report.offending_token;
        const src_ptr_begin = state.source.ptr;
        const src_ptr_offset = bad_token.value.ptr;
        const src_offset = @intFromPtr(src_ptr_offset) - @intFromPtr(src_ptr_begin);
        const left_slice = state.source[0..src_offset];
        const right_slice = state.source[src_offset..];

        var n_newlines_until_left_newline: usize = 0;
        var last_left_newline: usize = 0;
        var first_right_newline: usize = 0;

        for (left_slice, 0..) |c, i| {
            if (c != '\n') continue;
            n_newlines_until_left_newline += 1;
            last_left_newline = i;
        }

        if (last_left_newline > 0) {
            last_left_newline += 1;
        }

        for (right_slice, 0..) |c, i| {
            if (c != '\n') continue;
            first_right_newline = src_offset + i;
            break;
        }

        return .{
            .col = src_offset - last_left_newline,
            .row = n_newlines_until_left_newline + 1,
            .row_str = state.source[last_left_newline..first_right_newline],
            .row_error_token_idx = src_offset - last_left_newline,
            .msg = report.msg,
            .trailing_error = report.trailing,
        };
    }
};

fn logTime(comptime prefix: []const u8, start_ms: i64, stop_ms: i64) void {
    const s = @as(f64, @floatFromInt(stop_ms - start_ms)) / std.time.ms_per_s;
    print(prefix ++ "{d:.3}s\n", .{s});
}

pub fn writeAstError(state: *State, writer: anytype) !void {
    const info = state.errorInfo();
    //TODO: this should not be <somefile>
    try writer.print("<somefile>:{}:{}: syntax error: {s}\n{s}\n", .{ info.row, info.col, info.msg, info.row_str });
    try writeErrorSource(info, writer);
}

pub fn writeRuntimeError(state: *State, writer: anytype) !void {
    const info = state.errorInfo();
    //TODO: this should not be <somefile>
    try writer.print("<somefile>:{}:{}: runtime error: {s}\n{s}\n", .{ info.row, info.col, info.msg, info.row_str });
    try writeErrorSource(info, writer);
}

fn writeErrorSource(info: State.ErrorInfo, writer: anytype) !void {
    for (info.row_str[0..info.row_error_token_idx]) |_| {
        try writer.print(" ", .{});
    }
    try writer.print("^", .{});
    if (info.trailing_error) {
        for (info.row_str[info.row_error_token_idx..]) |_| {
            try writer.print("~", .{});
        }
    }
    try writer.print("\n", .{});
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
    state.root = ast.parse(state) catch |e| switch (e) {
        ast.errors.OutOfMemory => @panic("OOM"),
        ast.errors.ParseFailed => {
            try writeAstError(state, std.io.getStdErr().writer());
            return e;
        },
    };
    const ast_stop_ms = microTimestamp();
    if (state.verboseParser) {
        logTime("Parsing: ", ast_start_ms, ast_stop_ms);
        ast.dump(state.root);
    }

    interpret(state) catch |e| {
        switch (e) {
            error.ArgsCountMismatch,
            error.BadVariableLookup,
            error.CommandNotFound,
            error.MembersNotAllowed,
            error.MismatchedBraces,
            error.TypeMismatch,
            error.VariableAlreadyDeclared,
            => {
                try writeRuntimeError(state, std.io.getStdErr().writer());
            },
            error.OutOfMemory,
            error.FileNotFound,
            error.AccessDenied,
            error.NameTooLong,
            error.InvalidUtf8,
            error.InvalidWtf8,
            error.BadPathName,
            error.Unexpected,
            error.SymLinkLoop,
            error.ProcessFdQuotaExceeded,
            error.SystemFdQuotaExceeded,
            error.NoDevice,
            error.SystemResources,
            error.FileTooBig,
            error.IsDir,
            error.NoSpaceLeft,
            error.NotDir,
            error.DeviceBusy,
            error.FileBusy,
            error.WouldBlock,
            error.InputOutput,
            error.OperationAborted,
            error.BrokenPipe,
            error.ConnectionResetByPeer,
            error.ConnectionTimedOut,
            error.NotOpenForReading,
            error.SocketNotConnected,
            error.DiskQuota,
            error.InvalidArgument,
            error.NotOpenForWriting,
            error.LockViolation,
            error.AssertionFailed,
            error.CurrentWorkingDirectoryUnlinked,
            error.InvalidBatchScriptArg,
            error.InvalidExe,
            error.FileSystem,
            error.ResourceLimitReached,
            error.InvalidUserId,
            error.PermissionDenied,
            error.InvalidName,
            error.InvalidHandle,
            error.WaitAbandoned,
            error.WaitTimeOut,
            error.NetworkSubsystemFailed,
            error.StdoutStreamTooLong,
            error.StderrStreamTooLong,
            => return e,
        }
    };
}
