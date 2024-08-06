const std = @import("std");
const PipelineState = @import("pipeline.zig").State;
const ast = @import("ast.zig");
const symtable = @import("symtable.zig");
const gc = @import("gc.zig");
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
    stmntArena: std.heap.ArenaAllocator,
    stmntAlly: std.mem.Allocator,
    root: ast.Root,
};

pub fn interpret(state: *PipelineState) !void {
    var stmntArena = std.heap.ArenaAllocator.init(state.ally);
    defer stmntArena.deinit();
    var ctx = Context{
        .ally = state.ally,
        .arena = state.arena,
        .root = state.root,
        .stmntArena = stmntArena,
        .stmntAlly = stmntArena.allocator(),
    };

    try gc.init(ctx.ally);
    defer gc.deinit();

    symtable.init(ctx.ally);
    defer symtable.deinit();

    try interpretRoot(&ctx);
}

fn interpretRoot(ctx: *Context) !void {
    for (ctx.root.statements) |stmnt| {
        try interpretStatement(ctx, stmnt);
    }
}

const StatementError = EvalError || symtable.SymtableError;

fn interpretStatement(ctx: *Context, stmnt: ast.Statement) StatementError!void {
    defer _ = ctx.stmntArena.reset(.retain_capacity);
    switch (stmnt) {
        .invocation => |inv| _ = try evalInvocation(ctx, inv, .{
            .capturing = false,
        }),
        .var_decl => |var_decl| try interpretVarDecl(ctx, var_decl),
        .assignment => |assign| try interpretAssign(ctx, assign),
        .branches => |br| try interpretBranches(ctx, br),
    }
}

fn interpretVarDecl(ctx: *Context, var_decl: ast.VarDecl) !void {
    const value = switch (try evalExpression(ctx, var_decl.expression)) {
        .value => |value| value,
        .nothing => unreachable,
    };
    try symtable.insert(var_decl.token.value, value);
}

fn interpretAssign(ctx: *Context, assign: ast.Assignment) !void {
    _ = symtable.get(assign.token.value) orelse unreachable;
    const value = switch (try evalExpression(ctx, assign.expression)) {
        .value => |v| v,
        .nothing => unreachable, // Requires value
    };

    const was_owned = switch (assign.expression) {
        .variable => true,
        .list_literal,
        .bareword,
        .bool_literal,
        .integer_literal,
        .string_literal,
        .capturing_invocation,
        => false,
    };

    const value_to_insert = if (was_owned) try gc.cloneOrReference(value) else value;
    try symtable.put(assign.token.value, value_to_insert);
}

fn interpretBranches(ctx: *Context, branches: ast.Branches) !void {
    for (branches) |branch| {
        if (branch.condition) |expr| {
            const value = try evalExpression(ctx, expr);
            switch (value) {
                .value => |v| {
                    switch (v.as) {
                        .boolean => |b| {
                            if (b) {
                                try interpretScope(ctx, branch.scope);
                                break;
                            }
                        },
                        // needs boolean
                        else => unreachable,
                    }
                },
                // needs value
                .nothing => unreachable,
            }
        } else {
            try interpretScope(ctx, branch.scope);
            break;
        }
    }
}

fn interpretScope(ctx: *Context, scope: ast.Scope) !void {
    for (scope) |stmnt| {
        try interpretStatement(ctx, stmnt);
    }
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
    var args = try std.ArrayList(*Value).initCapacity(ctx.stmntAlly, inv.arguments.len);
    defer args.deinit();
    for (inv.arguments) |arg| {
        switch (try evalExpression(ctx, arg)) {
            .value => |value| try args.append(value),
            .nothing => unreachable, // this construct expects only values
        }
    }
    return try builtin(args.items);
}

fn evalProc(ctx: *Context, inv: ast.Invocation) !void {
    const name = inv.token.value;
    var args = try std.ArrayList([]const u8).initCapacity(ctx.stmntAlly, inv.arguments.len);

    try args.append(name);
    for (inv.arguments) |arg| {
        switch (try evalExpression(ctx, arg)) {
            .value => |value| try args.append(try value.asStr(ctx.stmntAlly)),
            .nothing => unreachable, // this construct expects only values
        }
    }

    var proc = std.process.Child.init(args.items, ctx.stmntAlly);
    const term = try proc.spawnAndWait();
    //TODO: handle result
    _ = term;
}

pub const EvalError = builtins.BuiltinError || std.process.Child.RunError || values.Errors;

fn evalExpression(ctx: *Context, expr: ast.Expression) EvalError!Result {
    return switch (expr) {
        .bareword => |bw| something(try evalBareword(bw)),
        .string_literal => |str| something(try evalStringLiteral(ctx, str)),
        .integer_literal => |int| something(try evalIntegerLiteral(int)),
        .bool_literal => |bl| something(try evalBoolLiteral(bl)),
        .variable => |variable| something(evalVariable(variable)),
        .capturing_invocation => |cap_inv| try evalInvocation(ctx, cap_inv, .{
            .capturing = true,
        }),
        .list_literal => |list| something(try evalListLiteral(ctx, list)),
    };
}

fn evalBareword(bw: ast.Bareword) !*Value {
    const value = try gc.string(bw.token.value);
    try symtable.appendRoot(value);
    return value;
}

fn evalStringLiteral(ctx: *Context, str: ast.StringLiteral) !*Value {
    if (str.interpolates) {
        const value = Value{
            .as = .{
                .string = str.token.value,
            },
        };
        const interpolated_value = try value.interpolate(ctx.ally);
        try symtable.appendRoot(interpolated_value);
        return interpolated_value;
    }
    const value = try gc.string(str.token.value);
    try symtable.appendRoot(value);
    return value;
}

fn evalIntegerLiteral(int: ast.IntegerLiteral) !*Value {
    //TODO: this should not be done here...
    const i = std.fmt.parseInt(i64, int.token.value, 10) catch unreachable;
    const value = try gc.integer(i);
    try symtable.appendRoot(value);
    return value;
}

fn evalBoolLiteral(bl: ast.BoolLiteral) !*Value {
    const value = try gc.boolean(bl.token.kind == .True);
    try symtable.appendRoot(value);
    return value;
}

fn evalVariable(variable: ast.Variable) *Value {
    //TODO: handle error
    return symtable.get(variable.token.value) orelse unreachable;
}

fn evalListLiteral(ctx: *Context, list_literal: ast.ListLiteral) !*Value {
    // Perhaps 'wrong' allocator? Perhaps does not matter? Who knows.
    var list = try values.List.initCapacity(ctx.ally, list_literal.items.len);
    errdefer list.deinit();
    for (list_literal.items) |item| {
        const value = try evalExpression(ctx, item);
        switch (value) {
            .value => |v| try list.append(v),
            .nothing => unreachable, // Construct needs value
        }
    }
    const value = try gc.list(list);
    try symtable.appendRoot(value);
    return value;
}
