const std = @import("std");
const PipelineState = @import("pipeline.zig").State;
const ast = @import("ast.zig");
const symtable = @import("symtable.zig");
const builtins = @import("builtins.zig");
const values = @import("value.zig");
const Value = values.Value;
const Result = values.Result;

const something = values.something;
const nothing = values.nothing;
const integer = values.integer;

const Context = struct {
    ally: std.mem.Allocator,
    arena: std.mem.Allocator,
    root: ast.Root,
    values: std.ArrayList(Value),
};

pub fn interpret(state: *PipelineState) !void {
    var ctx = Context{
        .ally = state.ally,
        .arena = state.arena,
        .root = state.root,
        .values = try std.ArrayList(Value).initCapacity(state.arena, 8),
    };
    defer ctx.values.deinit();

    symtable.init(ctx.ally);
    defer symtable.deinit();

    try interpretRoot(&ctx);
}

fn interpretRoot(ctx: *Context) !void {
    for (ctx.root.statements) |stmnt| {
        try interpretStatement(ctx, stmnt);
    }
}

fn interpretStatement(ctx: *Context, stmnt: ast.Statement) !void {
    switch (stmnt) {
        .invocation => |inv| _ = try evalInvocation(ctx, inv, .{
            .capturing = false,
        }),
        .var_decl => |var_decl| try interpretVarDecl(ctx, var_decl),
    }
}

fn interpretVarDecl(ctx: *Context, var_decl: ast.VarDecl) !void {
    const value = switch (try evalExpression(ctx, var_decl.expression)) {
        .value => |value| value,
        .nothing => unreachable,
    };
    try symtable.insert(var_decl.token.value, value);
}

const InvocationParams = struct {
    capturing: bool,
};

fn evalInvocation(
    ctx: *Context,
    inv: ast.Invocation,
    params: InvocationParams,
) !Result {
    _ = params;
    const name = inv.token.value;
    return if (builtins.lookup(name)) |builtin|
        try evalBuiltin(ctx, inv, builtin)
    else blk: {
        try evalProc(ctx, inv);
        break :blk nothing;
    };
}

fn evalBuiltin(ctx: *Context, inv: ast.Invocation, builtin: *const builtins.Builtin) !Result {
    defer ctx.values.clearRetainingCapacity();
    for (inv.arguments) |arg| {
        switch (try evalExpression(ctx, arg)) {
            .value => |value| try ctx.values.append(value),
            .nothing => unreachable, // this construct expects only values
        }
    }
    return try builtin(ctx.values.items);
}

fn evalProc(ctx: *Context, inv: ast.Invocation) !void {
    const name = inv.token.value;
    var arena = std.heap.ArenaAllocator.init(ctx.ally);
    defer arena.deinit();
    const ally = arena.allocator();

    var args = try std.ArrayList([]const u8).initCapacity(ally, inv.arguments.len);

    try args.append(name);
    for (inv.arguments) |arg| {
        switch (try evalExpression(ctx, arg)) {
            .value => |value| try args.append(try value.asStr(ally)),
            .nothing => unreachable, // this construct expects only values
        }
    }

    var proc = std.process.Child.init(args.items, ally);
    const term = try proc.spawnAndWait();
    //TODO: handle result
    _ = term;
}

pub const EvalError = builtins.BuiltinError || std.process.Child.RunError;

fn evalExpression(ctx: *Context, expr: ast.Expression) EvalError!Result {
    return switch (expr) {
        .bareword => |bw| something(evalBareword(bw)),
        .string_literal => |str| something(evalStringLiteral(str)),
        .bool_literal => |bl| something(evalBoolLiteral(bl)),
        .variable => |variable| something(evalVariable(variable)),
        .capturing_invocation => |cap_inv| try evalInvocation(ctx, cap_inv, .{
            .capturing = true,
        }),
    };
}

fn evalBareword(bw: ast.Bareword) Value {
    return Value.str(bw.token.value);
}

fn evalStringLiteral(str: ast.StringLiteral) Value {
    return Value.str(str.token.value);
}

fn evalBoolLiteral(bl: ast.BoolLiteral) Value {
    return Value.bol(bl.token.kind == .True);
}

fn evalVariable(variable: ast.Variable) Value {
    //TODO: handle error
    return symtable.get(variable.token.value) orelse unreachable;
}
