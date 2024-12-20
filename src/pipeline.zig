const std = @import("std");
const tokens = @import("tokens.zig");
const ast = @import("ast.zig");
const interpreter = @import("interpreter.zig");
const semantics = @import("semantics.zig");

const Token = tokens.Token;

const assert = std.debug.assert;
const print = std.debug.print;
const microTimestamp = std.time.microTimestamp;

pub const PipelineError = tokens.LexerError || ast.errors || semantics.SemanticsError || interpreter.EvalError;

pub const ErrorReport = struct {
    offending_token: *const Token,
    msg: []const u8,
    trailing: bool,
    offending_expr_idx: ?usize = null,
};

pub const State = struct {
    arena_source: *std.heap.ArenaAllocator,
    arena: std.mem.Allocator,
    ally: std.mem.Allocator,
    source: []const u8,
    tokens: []Token = &.{},
    root: ast.Root = .{
        .statements = &.{},
        .functions = undefined,
    },
    verboseLexer: bool,
    verboseParser: bool,
    verboseAnalysis: bool,
    verboseInterpretation: bool,
    verboseGc: bool,
    useSemanticAnalysis: bool,
    filename: []const u8,
    color: std.io.tty.Config = std.io.tty.Config.no_color,
    error_report: ?ErrorReport = null,
    analysis: semantics.Analysis = .{},
    env_map: std.process.EnvMap,

    pub fn readEnv(state: *const State, env: []const u8) ?[]const u8 {
        return state.env_map.get(env);
    }

    const ErrorInfo = struct {
        msg: []const u8,
        col: usize,
        row: usize,
        row_str: []const u8,
        row_error_token_idx: usize,
        trailing_error: bool,
    };

    pub fn errorInfo(state: *const State) ErrorInfo {
        const report = state.error_report orelse @panic("Developer error: No error stored");
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

        if (first_right_newline == 0) {
            first_right_newline = state.source.len;
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

    fn errorInfoPtr(state: *const State, token: *const Token) ErrorInfo {
        const src_ptr_begin = state.source.ptr;
        const src_ptr_offset = token.value.ptr;
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

        if (first_right_newline == 0) {
            first_right_newline = state.source.len;
        }

        return .{
            .col = src_offset - last_left_newline,
            .row = n_newlines_until_left_newline + 1,
            .row_str = state.source[last_left_newline..first_right_newline],
            .row_error_token_idx = src_offset - last_left_newline,
            .msg = "",
            .trailing_error = false,
        };
    }

    pub fn dumpSourceAtToken(state: *const State, token: *const Token) void {
        const source_info = errorInfoPtr(state, token);
        const writer = std.io.getStdErr().writer();
        writer.print("{s}\n", .{source_info.row_str}) catch {};
        writeErrorSource(source_info, writer) catch {};
    }
};

fn logTime(comptime prefix: []const u8, start_us: i64, stop_us: i64) void {
    //const s = @as(f64, @floatFromInt(stop_us - start_us)) / std.time.us_per_ms;
    var s = @as(f64, @floatFromInt(stop_us - start_us));
    if (s < 1000) {
        print(prefix ++ "{d:.3}µs\n", .{s});
        return;
    }
    s /= std.time.us_per_ms;
    if (s < 1000) {
        print(prefix ++ "{d:.3}ms\n", .{s});
        return;
    }
    s /= std.time.ms_per_s;
    if (s < 1000) {
        print(prefix ++ "{d:.3}s\n", .{s});
        return;
    }
}

pub fn writeError(state: *State, err: PipelineError) !void {
    const writer = std.io.getStdErr().writer();
    switch (err) {
        error.UnterminatedBlockComment => {
            try writeLexerError(state, writer);
        },
        error.ParseFailed => {
            try writeAstError(state, writer);
        },
        error.SemanticError => {
            try writeSemanticsError(state, writer);
        },
        error.ArgsCountMismatch,
        error.BadVariableLookup,
        error.CommandNotFound,
        error.MembersNotAllowed,
        error.MismatchedBraces,
        error.TypeMismatch,
        error.ValueRequired,
        error.VariableAlreadyDeclared,
        => {
            try writeRuntimeError(state, writer);
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
        error.Utf8ExpectedContinuation,
        error.Utf8OverlongEncoding,
        error.Utf8EncodesSurrogateHalf,
        error.Utf8CodepointTooLarge,
        error.Utf8InvalidStartByte,
        error.TruncatedInput,
        => return err,
    }
}

pub fn writeLexerError(state: *State, writer: anytype) !void {
    //TODO: handle error information better when there are no tokens
    //const info = state.errorInfo();
    //const file = state.filename;
    //try writer.print("<{s}>:{}:{}: lexer error: {s}\n{s}\n", .{ file, info.row, info.col, info.msg, info.row_str });
    //try writeErrorSource(info, writer);
    try writer.print("<{s}>: lexer error\n", .{state.filename});
}

pub fn writeAstError(state: *State, writer: anytype) !void {
    const info = state.errorInfo();
    const file = state.filename;
    try writer.print("<{s}>:{}:{}: syntax error: {s}\n{s}\n", .{ file, info.row, info.col, info.msg, info.row_str });
    try writeErrorSource(info, writer);
}

pub fn writeSemanticsError(state: *State, writer: anytype) !void {
    assert(state.analysis.errors.len > 0);
    for (state.analysis.errors) |error_report| {
        state.error_report = error_report;
        const info = state.errorInfo();
        const file = state.filename;
        try writer.print("<{s}>:{}:{}: semantic error: {s}\n{s}\n", .{ file, info.row, info.col, info.msg, info.row_str });
        try writeErrorSource(info, writer);
    }
}

pub fn writeRuntimeError(state: *State, writer: anytype) !void {
    const info = state.errorInfo();
    const file = state.filename;
    try writer.print("<{s}>:{}:{}: runtime error: {s}\n{s}\n", .{ file, info.row, info.col, info.msg, info.row_str });
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

pub fn run(state: *State) PipelineError!void {
    const lexer_start_us = microTimestamp();
    state.tokens = try tokens.lex(state);
    const lexer_stop_us = microTimestamp();
    if (state.verboseLexer) {
        logTime("Lexing:  ", lexer_start_us, lexer_stop_us);
        tokens.dump(state);
    }

    const ast_start_us = microTimestamp();
    state.root = try ast.parse(state);

    const ast_stop_us = microTimestamp();
    if (state.verboseParser) {
        logTime("Parsing: ", ast_start_us, ast_stop_us);
        ast.dump(state.root);
    }

    if (state.useSemanticAnalysis) {
        const analyze_start_us = microTimestamp();
        try semantics.analyze(state);

        const analyze_stop_us = microTimestamp();
        if (state.verboseAnalysis) {
            logTime("Analysis: ", analyze_start_us, analyze_stop_us);
        }
    }

    const interpret_start_us = microTimestamp();
    try interpreter.interpret(state);

    const interpret_stop_us = microTimestamp();
    if (state.verboseInterpretation) {
        logTime("Interpretation: ", interpret_start_us, interpret_stop_us);
    }
}
