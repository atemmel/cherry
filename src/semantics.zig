const std = @import("std");
const ast = @import("ast.zig");
const builtins = @import("builtins.zig");
const pipeline = @import("pipeline.zig");
const values = @import("value.zig");

const Token = @import("tokens.zig").Token;
const PipelineState = pipeline.State;
const ErrorReport = pipeline.ErrorReport;

// types used during semantic analysis
pub const TypeInfo = union(enum) {
    something,
    string,
    integer,
    float,
    boolean,
    list: struct {
        of: *const TypeInfo,
    },
    record,
    user_defined,
    nothing,
    generic: []const u8,
    either: []const TypeInfo,
};

pub const SemanticsError = error{SemanticError} || std.mem.Allocator.Error;

pub const Analysis = struct {
    errors: []const ErrorReport = &.{},
};

const Context = struct {
    pub const Scope = std.StringHashMap(TypeInfo);

    arena: std.mem.Allocator,
    errors: std.ArrayList(ErrorReport),
    scopes: std.ArrayList(Scope),
    state: *PipelineState,
    current_module: *pipeline.Module,

    pub fn errFmt(
        ctx: *Context,
        opt: struct {
            offending_expr_idx: ?usize = null,
            offending_token: *const Token,
            trailing: bool = false,
        },
        comptime fmt: []const u8,
        args: anytype,
    ) !void {
        try ctx.errors.append(.{
            .msg = try std.fmt.allocPrint(ctx.arena, fmt, args),
            .offending_expr_idx = opt.offending_expr_idx,
            .offending_token = opt.offending_token,
            .trailing = opt.trailing,
        });
    }

    pub fn typeMismatch(ctx: *Context, wants: []const u8, got: []const u8, offending_token: *const Token) !void {
        try ctx.errors.append(try typeMismatchReport(ctx.arena, wants, got, offending_token));
    }

    pub fn addScope(ctx: *Context) !void {
        try ctx.scopes.append(Scope.init(ctx.arena));
    }

    pub fn dropScope(ctx: *Context) void {
        _ = ctx.scopes.pop();
    }

    pub fn insertVariable(ctx: *Context, name: []const u8, type_info: TypeInfo) !void {
        var top = &ctx.scopes.items[ctx.scopes.items.len - 1];
        try top.put(name, type_info);
    }

    pub fn lookupVariable(ctx: *Context, name: []const u8) ?*TypeInfo {
        for (ctx.scopes.items) |scope| {
            if (scope.getPtr(name)) |type_info| {
                return type_info;
            }
        }
        return null;
    }
};

pub fn analyze(state: *PipelineState) SemanticsError!void {
    const arena = state.scratch_arena.allocator();
    var ctx: Context = .{
        .arena = arena,
        .errors = std.ArrayList(ErrorReport).init(arena),
        .scopes = std.ArrayList(Context.Scope).init(arena),
        .state = state,
        .current_module = state.modules.getPtr(state.current_module_in_process) orelse unreachable,
    };
    try analyzeRoot(&ctx);
    state.analysis = .{
        .errors = try ctx.errors.toOwnedSlice(),
    };

    if (state.analysis.errors.len > 0) {
        return SemanticsError.SemanticError;
    }
}

fn analyzeRoot(ctx: *Context) !void {
    var it = ctx.state.modules.valueIterator();
    while (it.next()) |module| {
        try analyzeModule(ctx, module.ast);
    }
}

fn analyzeModule(ctx: *Context, module: ast.Module) !void {
    try ctx.addScope();
    defer ctx.dropScope();
    for (module.statements) |stmnt| {
        try analyzeStatement(ctx, stmnt);
    }
}

fn analyzeStatement(ctx: *Context, stmnt: ast.Statement) !void {
    switch (stmnt) {
        .call => |call| _ = try analyzeCall(ctx, call),
        .var_decl => |var_decl| try analyzeVarDecl(ctx, var_decl),
        .assignment => |assign| try analyzeAssignment(ctx, assign),
        .branches => |br| try analyzeBranches(ctx, br),
        .scope => |scope| try analyzeScope(ctx, scope),
        .func, .import => unreachable, // this should never happen
        .ret => |ret| try analyzeReturn(ctx, ret),
        .loop => |loop| try analyzeLoop(ctx, loop),
        .brk => unreachable,
        .cont => unreachable,
    }
}

fn analyzeCall(ctx: *Context, call: ast.Call) !TypeInfo {
    const name = call.token.value;
    const builtin_info = builtins.lookup(name);
    if (builtin_info) |bi| {
        return analyzeBuiltinCall(ctx, call, bi);
    } else if (ctx.current_module.ast.functions.getPtr(name)) |func| {
        return func.signature.produces;
    }

    //TODO: call.pipe
    return .string;
}

fn analyzeBuiltinCall(ctx: *Context, call: ast.Call, builtin_info: builtins.BuiltinInfo) !TypeInfo {
    const name = call.token.value;
    const args = try analyzeArguments(ctx, call.arguments);
    const signature = builtin_info.signature;

    const required_len = signature.parameters.len;
    const provided_len = args.len;

    if (builtin_info.signature.last_parameter_is_variadic) {
        const n_non_variadic_parameters = required_len - 1;
        if (n_non_variadic_parameters > provided_len) {
            try ctx.errFmt(
                .{ .offending_token = call.token },
                "Call to '{s}' requires at least {} arguments, {} provided.",
                .{ name, required_len, provided_len },
            );
        }
    } else if (args.len != signature.parameters.len) {
        try ctx.errFmt(
            .{ .offending_token = call.token },
            "Call to '{s}' requires {} arguments, {} provided.",
            .{ name, required_len, provided_len },
        );
    }

    var idx: usize = 0;
    while (idx < required_len and idx < provided_len) : (idx += 1) {
        const param = signature.parameters[idx];
        const arg = args[idx];
        const arg_token = ast.tokenFromExpr(call.arguments[idx]);
        try analyzeSingleParam(ctx, param.param_type.type_info, arg, arg_token);
    }

    // if not variadic, cool, exit
    if (!signature.last_parameter_is_variadic) {
        return signature.produces;
    }

    // otherwise, more work
    const variadic_param = signature.parameters[signature.parameters.len - 1];

    while (idx < provided_len) : (idx += 1) {
        const arg = args[idx];
        const arg_token = ast.tokenFromExpr(call.arguments[idx]);
        try analyzeSingleParam(ctx, variadic_param.param_type.type_info, arg, arg_token);
    }

    return signature.produces;
}

fn analyzeSingleParam(ctx: *Context, param: TypeInfo, arg: TypeInfo, arg_token: *const Token) !void {
    switch (param) {
        .something => {
            switch (arg) {
                .boolean,
                .float,
                .integer,
                .list,
                .record,
                .string,
                .user_defined,
                .either,
                => {},
                .nothing => try ctx.typeMismatch("something", "nothing", arg_token),
                //TODO: An expression should never be 'something', it should always be specified
                //      ok for now as most builtins are unchecked
                .something => {},
                .generic => unreachable,
            }
        },
        // A builtin should never specify 'nothing' as the requested type, instead, the number of parameters should be 0
        .nothing => unreachable,
        .string => {
            switch (arg) {
                .boolean => try ctx.typeMismatch("string", "boolean", arg_token),
                .float => try ctx.typeMismatch("string", "float", arg_token),
                .integer => try ctx.typeMismatch("string", "integer", arg_token),
                .list => try ctx.typeMismatch("string", "list", arg_token),
                .nothing => try ctx.typeMismatch("string", "nothing", arg_token),
                .record => try ctx.typeMismatch("string", "record", arg_token),
                .string => {},
                .user_defined => unreachable, //TODO: this
                //TODO: An expression should never be 'something', it should always be specified
                //      ok for now as most builtins are unchecked
                .something => {},
                .generic => unreachable,
                .either => unreachable,
            }
        },
        .integer => {
            switch (arg) {
                .boolean => try ctx.typeMismatch("integer", "boolean", arg_token),
                .float => try ctx.typeMismatch("integer", "float", arg_token),
                .integer => {},
                .list => try ctx.typeMismatch("integer", "list", arg_token),
                .nothing => try ctx.typeMismatch("integer", "nothing", arg_token),
                .record => try ctx.typeMismatch("integer", "record", arg_token),
                .string => try ctx.typeMismatch("integer", "string", arg_token),
                .user_defined => unreachable, //TODO: this
                //TODO: An expression should never be 'something', it should always be specified
                //      ok for now as most builtins are unchecked
                .something => {},
                .generic => unreachable,
                .either => unreachable,
            }
        },
        .list => {
            switch (arg) {
                .boolean => try ctx.typeMismatch("list", "boolean", arg_token),
                .float => try ctx.typeMismatch("list", "float", arg_token),
                .integer => try ctx.typeMismatch("list", "integer", arg_token),
                .list => {}, //TODO: for now, a list is a list
                .nothing => try ctx.typeMismatch("list", "nothing", arg_token),
                .record => try ctx.typeMismatch("list", "record", arg_token),
                .string => try ctx.typeMismatch("list", "string", arg_token),
                .user_defined => unreachable, //TODO: this
                //TODO: An expression should never be 'something', it should always be specified
                //      ok for now as most builtins are unchecked
                .something => {},
                .generic => unreachable,
                .either => unreachable,
            }
        },
        .user_defined => unreachable,
        .boolean => {
            switch (arg) {
                .boolean => {},
                .float => try ctx.typeMismatch("boolean", "float", arg_token),
                .integer => try ctx.typeMismatch("boolean", "integer", arg_token),
                .list => try ctx.typeMismatch("boolean", "list", arg_token),
                .nothing => try ctx.typeMismatch("boolean", "nothing", arg_token),
                .record => try ctx.typeMismatch("boolean", "record", arg_token),
                .string => try ctx.typeMismatch("boolean", "string", arg_token),
                .user_defined => unreachable, //TODO: this
                //TODO: An expression should never be 'something', it should always be specified
                //      ok for now as most builtins are unchecked
                .something => {},
                .generic => unreachable,
                .either => unreachable,
            }
        },
        .record => {}, //TODO: this
        .float => unreachable,
        .generic => unreachable,
        .either => unreachable,
    }
}

fn analyzeVarDecl(ctx: *Context, decl: ast.VarDecl) !void {
    if (ctx.lookupVariable(decl.tokens[0].value)) |_| {
        //TODO: report error
    } else {
        const type_info = try analyzeExpression(ctx, decl.expression);
        try ctx.insertVariable(decl.tokens[0].value, type_info);
    }
}
fn analyzeAssignment(ctx: *Context, call: ast.Assignment) !void {
    _ = ctx;
    _ = call;
}

fn analyzeBranches(ctx: *Context, call: ast.Branches) !void {
    _ = ctx;
    _ = call;
}
fn analyzeScope(ctx: *Context, call: ast.Scope) !void {
    _ = ctx;
    _ = call;
}
fn analyzeFunc(ctx: *Context, call: ast.Func) !void {
    _ = ctx;
    _ = call;
}
fn analyzeReturn(ctx: *Context, call: ast.Return) !void {
    _ = ctx;
    _ = call;
}
fn analyzeLoop(ctx: *Context, call: ast.Loop) !void {
    _ = ctx;
    _ = call;
}

fn analyzeArguments(ctx: *Context, exprs: []const ast.Expression) SemanticsError![]const TypeInfo {
    var list = try std.ArrayList(TypeInfo).initCapacity(ctx.arena, exprs.len);
    for (exprs) |expr| {
        list.appendAssumeCapacity(try analyzeExpression(ctx, expr));
    }
    return try list.toOwnedSlice();
}

fn analyzeExpression(ctx: *Context, expr: ast.Expression) SemanticsError!TypeInfo {
    return switch (expr.as) {
        .bareword => .{ .string = {} },
        .bool_literal => .{ .boolean = {} },
        .capturing_call => |call| try analyzeCall(ctx, call),
        .integer_literal => .{ .integer = {} },
        .list_literal => |list| try analyzeList(ctx, list),
        .record_literal => .{ .record = {} }, //TODO: needs to understand what it is a record of
        .string_literal => .{ .string = {} }, //TODO: needs to semantically analyze substitutions
        .variable => |variable| blk: {
            if (ctx.lookupVariable(variable.token.value)) |type_info| {
                break :blk type_info.*;
            }
            try ctx.errFmt(.{ .offending_token = variable.token }, "Variable '{s}' referenced but never declared", .{variable.token.value});

            //TODO: report variable which does not exist
            break :blk .{ .nothing = {} };
        },
    };
}

fn analyzeList(ctx: *Context, list: ast.ListLiteral) SemanticsError!TypeInfo {
    if (list.items.len == 0) {
        const of = try ctx.arena.create(TypeInfo);
        of.*.nothing = {};
        return TypeInfo{
            .list = .{
                .of = of,
            },
        };
    }

    const first_expr = list.items[0];
    const first_type = try analyzeExpression(ctx, first_expr);
    for (list.items[1..]) |el| {
        const current_type = try analyzeExpression(ctx, el);
        const token = ast.tokenFromExpr(el);
        try analyzeSingleParam(ctx, first_type, current_type, token);
    }

    const of = try ctx.arena.create(TypeInfo);
    of.* = first_type;

    return TypeInfo{
        .list = .{
            .of = of,
        },
    };
}

pub fn typeMismatchReport(arena: std.mem.Allocator, wants: []const u8, got: []const u8, offending_token: *const Token) std.mem.Allocator.Error!ErrorReport {
    return typeMismatchReportImpl(arena, wants, got, .{ .token = offending_token });
}

pub fn typeMismatchReportIdx(arena: std.mem.Allocator, wants: []const u8, got: []const u8, offending_value_idx: usize) std.mem.Allocator.Error!ErrorReport {
    return typeMismatchReportImpl(arena, wants, got, .{ .expr_idx = offending_value_idx });
}

fn typeMismatchReportImpl(
    arena: std.mem.Allocator,
    wants: []const u8,
    got: []const u8,
    offender: union(enum) {
        token: *const Token,
        expr_idx: usize,
    },
) std.mem.Allocator.Error!ErrorReport {
    const msg = try std.fmt.allocPrint(arena, "Type mismatch, expected {s}, got {s}", .{ wants, got });
    return switch (offender) {
        .token => |tok| .{
            .msg = msg,
            .trailing = false,
            .offending_token = tok,
        },
        .expr_idx => |idx| .{
            .msg = msg,
            .trailing = false,
            .offending_token = undefined, // caller is meant to solve this using offending_expr_idx
            .offending_expr_idx = idx,
        },
    };
}
