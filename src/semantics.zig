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
    string,
    integer,
    float,
    boolean,
    list,
    record,
    user_defined,
    nothing,
};

pub const Signature = struct {
    generics: []const []const u8 = &.{},
    parameters: []const TypeInfo,
    last_parameter_is_trailing: bool = false,
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

    if (args.len != signature.parameters.len) {
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
        switch (param) {
            .string => {
                switch (arg) {
                    .string => {},
                    .integer => try ctx.typeMismatch("string", "integer", arg_token),
                    .float => try ctx.typeMismatch("string", "float", arg_token),
                    .boolean => try ctx.typeMismatch("string", "boolean", arg_token),
                    .list => try ctx.typeMismatch("string", "list", arg_token),
                    .record => try ctx.typeMismatch("string", "record", arg_token),
                    .user_defined => unreachable, //TODO: this
                    .nothing => try ctx.typeMismatch("string", "nothing", arg_token),
                }
            },
            .nothing => unreachable,
            .integer => unreachable,
            .list => unreachable,
            .user_defined => unreachable,
            .boolean => unreachable,
            .record => unreachable,
            .float => unreachable,
        }
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
            .offending_token = undefined, // callee is meant to solve this using offending_expr_idx
            .offending_expr_idx = idx,
        },
    };
}
