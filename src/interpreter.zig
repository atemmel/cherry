const std = @import("std");
const PipelineState = @import("pipeline.zig").State;
const ast = @import("ast.zig");
const symtable = @import("symtable.zig");
const builtins = @import("builtins.zig");
const Value = symtable.Value;

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
        .invocation => |inv| try interpretInvocation(ctx, inv),
        .var_decl => |var_decl| try interpretVarDecl(var_decl),
    }
}

fn interpretVarDecl(var_decl: ast.VarDecl) !void {
    const value = evalExpression(var_decl.expression) orelse unreachable;
    try symtable.insert(var_decl.token.value, value);
}

fn interpretInvocation(ctx: *Context, inv: ast.Invocation) !void {
    const name = inv.token.value;
    if (builtins.lookup(name)) |builtin| {
        try evalBuiltin(ctx, inv, builtin);
    } else {
        try evalProc(ctx, inv);
    }
}

fn evalBuiltin(ctx: *Context, inv: ast.Invocation, builtin: *const builtins.Builtin) !void {
    defer ctx.values.clearRetainingCapacity();
    for (inv.arguments) |arg| {
        if (evalExpression(arg)) |value| {
            try ctx.values.append(value);
        } else unreachable; // this construct expects only values
    }
    try builtin(ctx.values.items);
}

fn evalProc(ctx: *Context, inv: ast.Invocation) !void {
    const name = inv.token.value;
    var arena = std.heap.ArenaAllocator.init(ctx.ally);
    defer arena.deinit();
    const ally = arena.allocator();

    var args = try std.ArrayList([]const u8).initCapacity(ally, inv.arguments.len);

    try args.append(name);
    for (inv.arguments) |arg| {
        if (evalExpression(arg)) |value| {
            try args.append(try value.asStr(ally));
        } else unreachable; // this construct expects only values
    }

    var proc = std.process.Child.init(args.items, ally);
    const term = try proc.spawnAndWait();
    //TODO: handle result
    _ = term;
}

fn evalExpression(expr: ast.Expression) ?Value {
    return switch (expr) {
        .bareword => |bw| evalBareword(bw),
        .string_literal => |str| evalStringLiteral(str),
        .bool_literal => |bl| evalBoolLiteral(bl),
        .variable => |variable| evalVariable(variable),
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
