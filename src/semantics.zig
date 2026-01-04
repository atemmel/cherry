const std = @import("std");
const ast = @import("ast.zig");
const builtins = @import("builtins.zig");
const pipeline = @import("pipeline.zig");
const values = @import("value.zig");

const Token = @import("tokens.zig").Token;
const PipelineState = pipeline.State;
const ErrorReport = pipeline.ErrorReport;

const Table = std.StringHashMap(TypeInfo);

fn dump(table: Table) void {
    std.debug.print("Table size: {}\n", .{table.count()});
    var it = table.iterator();
    while (it.next()) |kv| {
        std.debug.print("{s} -> {any}\n", .{ kv.key_ptr.*, kv.value_ptr.* });
    }
}

const Collection = struct {
    of: *const TypeInfo,
};

// types used during semantic analysis
pub const TypeInfo = union(enum) {
    something,
    string,
    integer,
    float,
    boolean,
    list: Collection,
    record: Collection,
    user_defined,
    nothing,
    generic: []const u8,
    either: []const TypeInfo,

    pub fn str(self: TypeInfo, arena: std.mem.Allocator, table: Table) ![]const u8 {
        return switch (self) {
            .something => "something",
            .string => "string",
            .integer => "int",
            .float => "float",
            .boolean => "bool",
            .list => |l| try std.fmt.allocPrint(arena, "[]{s}", .{try l.of.str(arena, table)}),
            .record => |r| try std.fmt.allocPrint(arena, "[=]{s}", .{try r.of.str(arena, table)}),
            .user_defined => unreachable,
            .nothing => "nothing",
            .generic => |gen| blk: {
                if (table.get(gen)) |t| {
                    break :blk try std.fmt.allocPrint(arena, "{s} ({s})", .{ gen, try t.str(arena, table) });
                } else {
                    break :blk try std.fmt.allocPrint(arena, "{s}", .{gen});
                }
            },
            .either => |either| blk: {
                var type_str = std.ArrayList(u8){};
                for (either, 0..) |solution, idx| {
                    try type_str.appendSlice(arena, try solution.str(arena, table));
                    if (idx + 1 != either.len) {
                        try type_str.appendSlice(arena, " | ");
                    }
                }
                break :blk try type_str.toOwnedSlice(arena);
            },
        };
    }

    pub fn eql(lhs: TypeInfo, rhs: TypeInfo, table: Table) bool {
        return switch (lhs) {
            .something => switch (rhs) {
                .boolean, .float, .integer, .list, .record, .string, .user_defined, .either => true,
                .nothing => false,
                //TODO: An expression should never be 'something', it should always be specified
                //      ok for now as most builtins are unchecked
                .something => true,
                .generic => true,
            },
            // A builtin should never specify 'nothing' as the requested type, instead, the number of parameters should be 0
            .nothing => unreachable,
            .string => switch (rhs) {
                .string => true,
                .boolean, .float, .integer, .list, .nothing, .record => false,
                .user_defined => unreachable, //TODO: this
                //TODO: An expression should never be 'something', it should always be specified
                //      ok for now as most builtins are unchecked
                .something => true,
                .generic => unreachable,
                .either => unreachable,
            },
            .integer => switch (rhs) {
                .integer => true,
                .boolean, .float, .list, .nothing, .record, .string => false,
                .user_defined => unreachable, //TODO: this
                //TODO: An expression should never be 'something', it should always be specified
                //      ok for now as most builtins are unchecked
                .something => true,
                .generic => unreachable,
                .either => unreachable,
            },
            .list => |l| switch (rhs) {
                .list => |r| l.of.eql(r.of.*, table),
                .boolean, .float, .integer, .nothing, .record, .string => false,
                .user_defined => unreachable, //TODO: this
                //TODO: An expression should never be 'something', it should always be specified
                //      ok for now as most builtins are unchecked
                .something => true,
                .generic => unreachable,
                .either => unreachable,
            },
            .boolean => switch (rhs) {
                .boolean => true,
                .float, .integer, .list, .nothing, .record, .string => false,
                .user_defined => unreachable, //TODO: this
                //TODO: An expression should never be 'something', it should always be specified
                //      ok for now as most builtins are unchecked
                .something => true,
                .generic => unreachable,
                .either => unreachable,
            },
            .record => |l| switch (rhs) {
                .record => |r| l.of.eql(r.of.*, table),
                .boolean, .float, .integer, .nothing, .list, .string => false,
                .user_defined => unreachable, //TODO: this
                //TODO: An expression should never be 'something', it should always be specified
                //      ok for now as most builtins are unchecked
                .something => true,
                .generic => unreachable,
                .either => unreachable,
            },
            .user_defined => unreachable,
            .float => unreachable,
            .generic => |gen| blk: {
                const lookup = table.get(gen).?;
                break :blk lookup.eql(rhs, table);
            },
            .either => |either| blk: {
                for (either) |solution| {
                    if (solution.eql(rhs, table)) {
                        break :blk true;
                    }
                }
                break :blk false;
            },
        };
    }
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

    current_loop_depth: usize = 0,

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
        try ctx.errors.append(ctx.arena, .{
            .msg = try std.fmt.allocPrint(ctx.arena, fmt, args),
            .offending_expr_idx = opt.offending_expr_idx,
            .offending_token = opt.offending_token,
            .trailing = opt.trailing,
        });
    }

    pub fn typeMismatch(ctx: *Context, wants: []const u8, got: []const u8, offending_token: *const Token) !void {
        try ctx.errors.append(ctx.arena, try typeMismatchReport(ctx.arena, wants, got, offending_token));
    }

    pub fn addScope(ctx: *Context) !void {
        try ctx.scopes.append(ctx.arena, Scope.init(ctx.arena));
    }

    pub fn dropScope(ctx: *Context) void {
        _ = ctx.scopes.pop();
    }

    pub fn enterLoop(ctx: *Context) void {
        ctx.current_loop_depth += 1;
    }

    pub fn exitLoop(ctx: *Context) void {
        ctx.current_loop_depth -= 1;
    }

    fn isInsideLoop(ctx: *Context) bool {
        return ctx.current_loop_depth > 0;
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
        .errors = std.ArrayList(ErrorReport).empty,
        .scopes = std.ArrayList(Context.Scope).empty,
        .state = state,
        .current_module = state.modules.getPtr(state.current_module_in_process) orelse unreachable,
    };
    try analyzeRoot(&ctx);
    state.analysis = .{
        .errors = try ctx.errors.toOwnedSlice(ctx.arena),
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
        .brk => |brk| try analyzeBreak(ctx, brk),
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
    var generic_lookup_table = std.StringHashMap(TypeInfo).init(ctx.arena);
    const name = call.token.value;
    //const args = try analyzeArguments(ctx, call.arguments);
    const signature = builtin_info.signature;
    const required_len = signature.parameters.len;
    const provided_len = call.arguments.len;

    if (builtin_info.signature.last_parameter_is_variadic) {
        const n_non_variadic_parameters = required_len - 1;
        if (n_non_variadic_parameters > provided_len) {
            try ctx.errFmt(
                .{ .offending_token = call.token },
                "Call to '{s}' requires at least {} arguments, {} provided.",
                .{ name, required_len, provided_len },
            );
        }
    } else if (call.arguments.len != signature.parameters.len) {
        try ctx.errFmt(
            .{ .offending_token = call.token },
            "Call to '{s}' requires {} arguments, {} provided.",
            .{ name, required_len, provided_len },
        );
    }

    var idx: usize = 0;
    while (idx < required_len and idx < provided_len) : (idx += 1) {
        const param = signature.parameters[idx];
        const arg = call.arguments[idx];
        const arg_token = ast.tokenFromExpr(arg);
        const encountered_type = try analyzeExpression(ctx, arg);
        switch (param.param_type.type_info) {
            .generic => |gen| {
                try lookupAndInsertGeneric(ctx, &generic_lookup_table, gen, encountered_type, .{ .analyzed_token = arg_token });
            },
            .record, .list => |collection| {
                try collectGenericsWithinCollection(ctx, collection, encountered_type, &generic_lookup_table, .{ .analyzed_token = arg_token });
            },
            .either => |either| {
                try collectGenericsWithinEither(ctx, either, encountered_type, &generic_lookup_table, .{ .analyzed_token = arg_token });
            },
            else => {},
        }
        try analyzeSingleExpression(ctx, param.param_type.type_info, encountered_type, arg_token, generic_lookup_table);
    }

    // if not variadic: cool, exit
    if (!signature.last_parameter_is_variadic) {
        return signature.produces;
    }

    // otherwise, more work
    const variadic_param = signature.parameters[signature.parameters.len - 1];

    while (idx < provided_len) : (idx += 1) {
        const arg = try analyzeExpression(ctx, call.arguments[idx]);
        const arg_token = ast.tokenFromExpr(call.arguments[idx]);
        try analyzeSingleExpression(ctx, variadic_param.param_type.type_info, arg, arg_token, generic_lookup_table);
    }

    return signature.produces;
}

const ParentTypePair = struct {
    wants: ?TypeInfo = null,
    got: ?TypeInfo = null,
    analyzed_token: *const Token,
};

fn collectGenericsWithinCollection(ctx: *Context, collection: Collection, got: TypeInfo, table: *Table, parent: ParentTypePair) !void {
    switch (collection.of.*) {
        .generic => |gen| {
            switch (got) {
                .list, .record => |l| {
                    try lookupAndInsertGeneric(ctx, table, gen, l.of.*, parent);
                },
                else => {},
            }
        },
        .list, .record => |inner| try collectGenericsWithinCollection(ctx, inner, got, table, parent),
        else => {},
    }
}

fn collectGenericsWithinEither(ctx: *Context, either: []const TypeInfo, got: TypeInfo, table: *Table, parent: ParentTypePair) !void {
    for (either) |solution| {
        switch (solution) {
            .list, .record => |collection| try collectGenericsWithinCollection(ctx, collection, got, table, parent),
            .generic => unreachable, // Generic Either allowed?
            else => {},
        }
    }
}

fn lookupAndInsertGeneric(ctx: *Context, table: *Table, generic_name: []const u8, generic_alias: TypeInfo, parent: ParentTypePair) !void {
    _ = ctx;
    _ = parent;
    _ = table.get(generic_name) orelse {
        try table.put(generic_name, generic_alias);
        return;
    };

    //if (!lookup.eql(generic_alias, table.*)) {
    //const wants = parent.wants orelse lookup;
    //const got = parent.got orelse generic_alias;
    //try ctx.typeMismatch(try wants.str(ctx.arena, table.*), try got.str(ctx.arena, table.*), parent.analyzed_token);
    //}
}

fn analyzeSingleExpression(ctx: *Context, wants_type: TypeInfo, got_type: TypeInfo, got_token: *const Token, table: Table) !void {
    const arena = ctx.arena;
    if (!wants_type.eql(got_type, table)) {
        const want = try wants_type.str(arena, table);
        const got = try got_type.str(arena, table);
        return ctx.typeMismatch(want, got, got_token);
    }
}

fn analyzeVarDecl(ctx: *Context, decl: ast.VarDecl) !void {
    if (decl.tokens.len == 1) {
        try analyzeSingleVarDecl(ctx, decl);
    } else unreachable;
}

fn analyzeSingleVarDecl(ctx: *Context, decl: ast.VarDecl) !void {
    if (ctx.lookupVariable(decl.tokens[0].value)) |_| {
        try ctx.errFmt(.{ .offending_token = decl.tokens[0] }, "variable '{s}' is already declared within current scope", .{decl.tokens[0].value});
    } else {
        const type_info = try analyzeExpression(ctx, decl.expression);
        try ctx.insertVariable(decl.tokens[0].value, type_info);
    }
}

fn analyzeAssignment(ctx: *Context, assign: ast.Assignment) !void {
    const var_token = assign.variable.token;
    const v = ctx.lookupVariable(var_token.value) orelse {
        try ctx.errFmt(.{ .offending_token = var_token }, "variable '{s}' is never declared", .{var_token.value});
        return;
    };

    const empty_table = Table.init(ctx.arena);

    const type_info = try analyzeExpression(ctx, assign.expression);
    if (!v.eql(type_info, empty_table)) {
        const bad_token = ast.tokenFromExpr(assign.expression);
        const other_type_str = try type_info.str(ctx.arena, empty_table);
        const assignee_type_str = try v.str(ctx.arena, empty_table);
        try ctx.errFmt(.{ .offending_token = bad_token }, "variable '{s}' of type {s} cannot be assigned value of type {s}", .{
            var_token.value,
            assignee_type_str,
            other_type_str,
        });
    }
}

fn analyzeBranches(ctx: *Context, branches: ast.Branches) !void {
    for (branches) |branch| {
        if (branch.condition) |condition| {
            const type_info = try analyzeExpression(ctx, condition);
            if (type_info != .boolean) {
                try ctx.errFmt(.{ .offending_token = ast.tokenFromExpr(condition) }, "condition in if-statement must be boolean, but was {s}", .{
                    try type_info.str(ctx.arena, Table.init(ctx.arena)),
                });
            }
        }

        try analyzeScope(ctx, branch.scope);
    }
}

fn analyzeScope(ctx: *Context, scope: ast.Scope) SemanticsError!void {
    try ctx.addScope();
    ctx.dropScope();

    for (scope) |stmnt| {
        try analyzeStatement(ctx, stmnt);
    }
}

fn analyzeFunc(ctx: *Context, call: ast.Func) !void {
    _ = ctx;
    _ = call;
}

fn analyzeReturn(ctx: *Context, call: ast.Return) !void {
    _ = ctx;
    _ = call;
}

fn analyzeLoop(ctx: *Context, loop: ast.Loop) !void {
    ctx.enterLoop();
    defer ctx.exitLoop();

    switch (loop.kind) {
        .classic_loop => |l| try analyzeClassicLoop(ctx, l),
        .range_loop => unreachable,
    }

    try analyzeScope(ctx, loop.scope);
}

fn analyzeClassicLoop(ctx: *Context, loop: ast.ClassicLoop) !void {
    if (loop.init_op) |init_op| {
        switch (init_op) {
            .declaration => |decl| try analyzeVarDecl(ctx, decl),
            .assignment => |assgn| try analyzeAssignment(ctx, assgn),
        }
    }

    if (loop.post_op) |post_op| {
        switch (post_op) {
            .assignment => |assgn| try analyzeAssignment(ctx, assgn),
            // probably safe to discard
            .call => |call| _ = try analyzeCall(ctx, call),
        }
    }
}

fn analyzeBreak(ctx: *Context, brk: ast.Break) !void {
    if (ctx.isInsideLoop()) return;
    try ctx.errFmt(.{ .offending_token = brk.token }, "break encountered outside of loop", .{});
}

fn analyzeContinue(ctx: *Context, cnt: ast.Continue) !void {
    if (ctx.isInsideLoop()) return;
    try ctx.errFmt(.{ .offending_token = cnt.token }, "continue encountered outside of loop", .{});
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
        .record_literal => |record| try analyzeRecord(ctx, record),
        .string_literal => .{ .string = {} }, //TODO: needs to semantically analyze substitutions
        .variable => |variable| blk: {
            if (ctx.lookupVariable(variable.token.value)) |type_info| {
                break :blk type_info.*;
            }
            try ctx.errFmt(.{ .offending_token = variable.token }, "Variable '{s}' referenced but never declared", .{variable.token.value});

            //TODO: report variable which does not exist
            break :blk .{ .nothing = {} };
        },
        .unary_operator => |op| switch (op.token.kind) {
            //TODO: look inside expression
            .Bang => .{ .boolean = {} },
            // none of these should be set, if so, developer error
            else => unreachable,
        },
        .binary_operator => .{ .boolean = {} }, //TODO
        .closure => .{ .boolean = {} }, //TODO
    };
}

fn analyzeList(ctx: *Context, list: ast.ListLiteral) SemanticsError!TypeInfo {
    const empty_table = Table.init(ctx.arena);
    if (list.items.len == 0) {
        const of = try ctx.arena.create(TypeInfo);
        of.* = .nothing;
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
        try analyzeSingleExpression(ctx, first_type, current_type, token, empty_table);
    }

    const of = try ctx.arena.create(TypeInfo);
    of.* = first_type;

    return TypeInfo{
        .list = .{
            .of = of,
        },
    };
}

fn analyzeRecord(ctx: *Context, record: ast.RecordLiteral) SemanticsError!TypeInfo {
    _ = record;
    const of = try ctx.arena.create(TypeInfo);
    of.* = .nothing;
    return TypeInfo{
        .record = .{
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
