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

pub const Error = error{CommandNotFound};

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

    interpretRoot(&ctx) catch |e| {
        switch (e) {
            error.FileNotFound => return error.CommandNotFound,
            else => {}, //TODO: go through these baddies
        }
    };
}

fn interpretRoot(ctx: *Context) !void {
    try symtable.pushFrame();
    defer symtable.popFrame();
    for (ctx.root.statements) |stmnt| {
        _ = try interpretStatement(ctx, stmnt);
    }
}

const StatementError = EvalError || symtable.SymtableError;

fn interpretStatement(ctx: *Context, stmnt: ast.Statement) StatementError!Result {
    defer _ = ctx.stmntArena.reset(.retain_capacity);
    switch (stmnt) {
        .call => |call| _ = try evalPipeline(ctx, call),
        .var_decl => |var_decl| try interpretVarDecl(ctx, var_decl),
        .assignment => |assign| try interpretAssign(ctx, assign),
        .branches => |br| try interpretBranches(ctx, br),
        .scope => |scope| try interpretScope(ctx, scope),
        .func => unreachable,
        .ret => |ret| {
            return try evalReturn(ctx, ret);
        },
    }
    return nothing;
}

fn interpretVarDecl(ctx: *Context, var_decl: ast.VarDecl) !void {
    const value = switch (try evalExpression(ctx, var_decl.expression)) {
        .value => |value| value,
        .nothing => unreachable,
    };
    try symtable.insert(var_decl.token.value, value);
}

fn interpretAssign(ctx: *Context, assign: ast.Assignment) !void {
    std.debug.assert(assign.accessor == null); //TODO:
    _ = symtable.get(assign.variable.token.value) orelse unreachable;
    const value = switch (try evalExpression(ctx, assign.expression)) {
        .value => |v| v,
        .nothing => unreachable, // Requires value
    };

    const was_owned = switch (assign.expression.as) {
        .variable => true,
        .list_literal,
        .record_literal,
        .bareword,
        .bool_literal,
        .integer_literal,
        .string_literal,
        .capturing_call,
        => false,
    };

    const value_to_insert = if (was_owned) try gc.cloneOrReference(value) else value;
    try symtable.put(assign.variable.token.value, value_to_insert);
}

fn interpretBranches(ctx: *Context, branches: ast.Branches) !void {
    try symtable.pushFrame();
    defer symtable.popFrame();
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
    try symtable.pushFrame();
    defer symtable.popFrame();
    for (scope) |stmnt| {
        _ = try interpretStatement(ctx, stmnt);
    }
}

fn evalReturn(ctx: *Context, ret: ast.Return) !Result {
    if (ret.expression) |expr| {
        return try evalExpression(ctx, expr);
    }
    return nothing;
}

//TODO: this only handles external programs
fn evalPipeline(ctx: *Context, call: ast.Call) !Result {
    const name = call.token.value;
    if (builtins.lookup(name)) |builtin| {
        return try evalBuiltin(ctx, call, builtin);
    } else if (ctx.root.functions.getPtr(name)) |func| {
        return try evalFunctionCall(ctx, func.*, call);
    } else {
        return try evalProc(ctx, call);
    }
}

fn evalBuiltin(ctx: *Context, call: ast.Call, builtin: *const builtins.Builtin) !Result {
    var args = try evalArgs(ctx, call.arguments);
    defer args.deinit();
    return try builtin(args.items);
}

fn evalFunctionCall(ctx: *Context, func: ast.Func, call: ast.Call) !Result {
    try symtable.pushFrame();
    defer symtable.popFrame();

    var args = try evalArgs(ctx, call.arguments);
    defer args.deinit();

    std.debug.assert(func.params.len == args.items.len);

    for (func.params, args.items) |par, arg| {
        try symtable.put(par.token.value, arg);
    }

    for (func.scope) |stmnt| {
        const result = try interpretStatement(ctx, stmnt);
        switch (result) {
            .nothing => {
                switch (stmnt) {
                    .ret => return nothing,
                    else => {},
                }
            },
            .value => |val| return something(val),
        }
    }
    return nothing;
}

fn evalProc(ctx: *Context, call: ast.Call) !Result {
    const capturing = call.capturing_external_cmd;
    var procs = try std.ArrayList(std.process.Child).initCapacity(ctx.stmntAlly, 4);

    var ptr: ?*const ast.Call = &call;
    while (ptr) |call_ptr| {
        try procs.append(try proc(ctx, call_ptr));
        ptr = call_ptr.pipe;
    }

    for (procs.items, 0..) |*p, idx| {
        if (procs.items.len > 1) {
            if (idx > 0) {
                p.stdin_behavior = .Pipe;
            }
            if (capturing or idx + 1 < procs.items.len) {
                p.stdout_behavior = .Pipe;
            }
        }
    }

    for (procs.items) |*p| {
        try p.spawn();
    }

    var capture: ?[]const u8 = null;
    errdefer if (capture) |c| ctx.ally.free(c);

    if (procs.items.len > 1) {
        var idx: usize = 0;
        while (idx < procs.items.len) : (idx += 1) {
            if (idx > 0) {
                var prev = &procs.items[idx - 1];
                var this = &procs.items[idx];
                var fifo = std.fifo.LinearFifo(u8, .{ .Static = 1024 }).init();
                try fifo.pump(prev.stdout.?.reader(), this.stdin.?.writer());
                if (this.stdin) |in| {
                    in.close();
                    this.stdin = null;
                }
            }
        }
    }

    if (capturing) {
        capture = try procs.getLast().stdout.?.readToEndAlloc(ctx.ally, std.math.maxInt(u64));
    }

    for (procs.items) |*p| {
        //TODO: handle term
        _ = try p.wait();
    }

    if (capturing) {
        return something(try gc.allocedString(capture.?));
    }
    return nothing;
}

fn proc(ctx: *Context, call: *const ast.Call) !std.process.Child {
    const name = call.token.value;
    var args = try std.ArrayList([]const u8).initCapacity(ctx.stmntAlly, call.arguments.len);

    try args.append(name);
    for (call.arguments) |arg| {
        switch (try evalExpression(ctx, arg)) {
            .value => |value| try args.append(try value.asStr(ctx.stmntAlly)),
            .nothing => unreachable, // this construct expects only values
        }
    }

    return std.process.Child.init(args.items, ctx.stmntAlly);
}

fn evalArgs(ctx: *Context, arguments: []const ast.Expression) !std.ArrayList(*Value) {
    var args = try std.ArrayList(*Value).initCapacity(ctx.stmntAlly, arguments.len);
    for (arguments) |arg| {
        switch (try evalExpression(ctx, arg)) {
            .value => |value| try args.append(value),
            .nothing => unreachable, // this construct expects only values
        }
    }
    return args;
}

pub const EvalError = builtins.BuiltinError || std.process.Child.RunError || values.Errors || symtable.SymtableError;

fn evalExpression(ctx: *Context, expr: ast.Expression) EvalError!Result {
    return switch (expr.as) {
        .bareword => |bw| something(try evalBareword(bw)),
        .string_literal => |str| something(try evalStringLiteral(ctx, str)),
        .integer_literal => |int| something(try evalIntegerLiteral(int)),
        .bool_literal => |bl| something(try evalBoolLiteral(bl)),
        .variable => |variable| something(evalVariable(variable)),
        .capturing_call => |cap_inv| try evalPipeline(ctx, cap_inv),
        .list_literal => |list| something(try evalListLiteral(ctx, list)),
        .record_literal => |record| something(try evalRecordLiteral(ctx, record)),
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

fn evalRecordLiteral(ctx: *Context, record_literal: ast.RecordLiteral) !*Value {
    _ = ctx; // autofix
    _ = record_literal; // autofix
    return gc.emptyRecord();
}
