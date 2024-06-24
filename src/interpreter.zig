const std = @import("std");
const PipelineState = @import("pipeline.zig").State;
const ast = @import("ast.zig");

const Value = []const u8;

const Context = struct {
    ally: std.mem.Allocator,
    root: ast.Root,
    values: std.ArrayList(Value),
};

pub fn interpret(state: *PipelineState) !void {
    var ctx = Context{
        .ally = state.ally,
        .root = state.root,
        .values = try std.ArrayList(Value).initCapacity(state.arena, 8),
    };
    defer ctx.values.deinit();

    try interpretRoot(&ctx);
}

fn interpretRoot(ctx: *Context) !void {
    for (ctx.root.statements) |stmnt| {
        try interpretStatement(ctx, stmnt);
    }
}

fn interpretStatement(ctx: *Context, stmnt: ast.Statement) !void {
    switch (stmnt) {
        .invocation => |inv| try interpretInvocation(ctx, inv),
    }
}

fn interpretInvocation(ctx: *Context, inv: ast.Invocation) !void {
    defer ctx.values.clearRetainingCapacity();
    const name = inv.token.value;
    try ctx.values.append(name);
    for (inv.arguments) |arg| {
        if (evalExpression(arg)) |value| {
            try ctx.values.append(value);
        } else unreachable; // this construct expects only values
    }

    var proc = std.process.Child.init(ctx.values.items, ctx.ally);
    const term = try proc.spawnAndWait();
    //TODO: handle result
    _ = term;
}

fn evalExpression(expr: ast.Expression) ?Value {
    return switch (expr) {
        .bareword => |bw| evalBareword(bw),
        .stringLiteral => |str| evalStringLiteral(str),
    };
}

fn evalBareword(bw: ast.Bareword) Value {
    return bw.token.value;
}

fn evalStringLiteral(str: ast.StringLiteral) Value {
    return str.token.value;
}
