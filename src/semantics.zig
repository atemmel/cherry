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
};

pub const Signature = struct {
    generics: []const []const u8 = &.{},
    parameters: []const TypeInfo,
    last_parameter_is_variadic: bool = false,
    produces: TypeInfo = .nothing,
};

pub const SemanticsError = error{SemanticError} || std.mem.Allocator.Error;

pub const Analysis = struct {
    errors: []const ErrorReport = &.{},
};

const Context = struct {
    arena: std.mem.Allocator,
    root: *const ast.Root,
    errors: std.ArrayList(ErrorReport),

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
};

pub fn analyze(state: *PipelineState) SemanticsError!void {
    var ctx: Context = .{
        .arena = state.arena,
        .root = &state.root,
        .errors = std.ArrayList(ErrorReport).init(state.arena),
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
    for (ctx.root.statements) |stmnt| {
        try analyzeStatement(ctx, stmnt);
    }
}

fn analyzeStatement(ctx: *Context, stmnt: ast.Statement) !void {
    switch (stmnt) {
        .call => |call| try analyzeCall(ctx, call),
        .var_decl => |var_decl| try analyzeVarDecl(ctx, var_decl),
        .assignment => |assign| try analyzeAssignment(ctx, assign),
        .branches => |br| try analyzeBranches(ctx, br),
        .scope => |scope| try analyzeScope(ctx, scope),
        .func => unreachable, // this should never happen
        .ret => |ret| try analyzeReturn(ctx, ret),
        .loop => |loop| try analyzeLoop(ctx, loop),
    }
}

fn analyzeCall(ctx: *Context, call: ast.Call) !void {
    const name = call.token.value;
    const builtin_info = builtins.lookup(name);
    if (builtin_info) |bi| {
        try analyzeBuiltinCall(ctx, call, bi);
    }
}

fn analyzeBuiltinCall(ctx: *Context, call: ast.Call, builtin_info: builtins.BuiltinInfo) !void {
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
        try analyzeSingleParam(ctx, param, arg, arg_token);
    }

    // if not variadic, cool, exit
    if (!signature.last_parameter_is_variadic) {
        return;
    }

    // otherwise, more work
    const type_of_variadic_param = signature.parameters[signature.parameters.len - 1];

    while (idx < provided_len) : (idx += 1) {
        const arg = args[idx];
        const arg_token = ast.tokenFromExpr(call.arguments[idx]);
        try analyzeSingleParam(ctx, type_of_variadic_param, arg, arg_token);
    }
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
                => {},
                .nothing => try ctx.typeMismatch("something", "nothing", arg_token),
                // An expression should never be 'something', it should always be specified
                .something => unreachable,
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
                // An expression should never be 'something', it should always be specified
                .something => unreachable,
                .generic => unreachable,
            }
        },
        .integer => unreachable,
        .list => unreachable,
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
                // An expression should never be 'something', it should always be specified
                .something => unreachable,
                .generic => unreachable,
            }
        },
        .record => unreachable,
        .float => unreachable,
        .generic => unreachable,
    }
}

fn analyzeVarDecl(ctx: *Context, call: ast.VarDecl) !void {
    _ = ctx;
    _ = call;
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

fn analyzeArguments(ctx: *Context, exprs: []const ast.Expression) ![]const TypeInfo {
    var list = try std.ArrayList(TypeInfo).initCapacity(ctx.arena, exprs.len);
    for (exprs) |expr| {
        list.appendAssumeCapacity(analyzeExpression(ctx, expr));
    }
    return try list.toOwnedSlice();
}

fn analyzeExpression(ctx: *Context, expr: ast.Expression) TypeInfo {
    _ = ctx;
    return switch (expr.as) {
        .bareword => .{ .string = {} },
        .bool_literal => .{ .boolean = {} },
        .capturing_call => unreachable, //TODO: needs to lookup function
        .integer_literal => .{ .integer = {} },
        .list_literal => .{ .list = {} }, //TODO: needs to understand what it is a list of
        .record_literal => .{ .record = {} }, //TODO: needs to understand what it is a record of
        .string_literal => .{ .string = {} }, //TODO: needs to semantically analyze substitutions
        .variable => unreachable, //TODO: needs to lookup variable from current scope
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
