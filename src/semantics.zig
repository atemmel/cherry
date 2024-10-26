const std = @import("std");
const ast = @import("ast.zig");
const Token = @import("tokens.zig").Token;
const PipelineState = @import("pipeline.zig").State;

pub const SemanticsError = std.mem.Allocator.Error;

pub const AnalysisError = struct {
    offending_token: *const Token,
    message: []const u8,
};

const Analysis = struct {
    errors: std.ArrayList(AnalysisError),
};

const Context = struct {
    arena: std.mem.Allocator,
    root: *const ast.Root,
    analysis: Analysis,
};

pub fn analyze(state: *PipelineState) SemanticsError!Analysis {
    var ctx: Context = .{
        .arena = state.arena,
        .root = &state.root,
        .analysis = .{
            .errors = std.ArrayList(AnalysisError).init(state.arena),
        },
    };
    try analyzeRoot(&ctx, state.root);
    return ctx.analysis;
}

fn analyzeRoot(ctx: *Context) !void {
    for (ctx.root.statements) |stmnt| {
        try analyzeStatement(ctx, stmnt);
    }
}

fn analyzeStatement(ctx: *Context, stmnt: ast.Statement) !void {
    _ = ctx;
    _ = stmnt;
    unreachable;
}
