const std = @import("std");
const pipeline = @import("pipeline.zig");
const ast = @import("ast.zig");
const ast_dump = @import("ast_dump.zig");
const gc = @import("gc.zig");
const builtins = @import("builtins.zig");
const values = @import("value.zig");
const tokens = @import("tokens.zig");
const strings = @import("strings.zig");
const modules = @import("modules.zig");

const assert = std.debug.assert;

const PipelineState = pipeline.State;
const Module = pipeline.Module;
const Value = values.Value;
const Result = values.Result;

const multiple = values.multiple;
const something = values.something;
const nothing = values.nothing;
const integer = values.integer;

pub const InterpreterError = error{
    ArgsCountMismatch,
    BadVariableLookup,
    CommandNotFound,
    EntryNotFoundWithinRecord,
    FunctionNotFoundWithinModule,
    MembersNotAllowed,
    MismatchedBraces,
    ModuleNotFound,
    NonRecordAccessAttempt,
    TypeMismatch,
    UnableToOpenFileDuringRedirect,
    ValueRequired,
    VariableAlreadyDeclared,
    BadAssign,
};

pub const EvalError = builtins.BuiltinError || std.process.Child.RunError;

const Context = struct {
    state: *PipelineState,
    ally: std.mem.Allocator,
    stmnt_scratch_arena: std.heap.ArenaAllocator,
    stmnt_scratch: std.mem.Allocator,
    root_module: *const pipeline.Module,
};

const Returns = union(enum) {
    did_return: Result,
    did_not_return: void,
    did_break: void,
    did_continue: void,
};

pub const InterpreterOptions = struct {
    root_module_name: []const u8,
    root_scope_already_exists: bool,
};

pub fn interpret(state: *PipelineState, opt: InterpreterOptions) EvalError!void {
    var stmnt_scratch_arena = std.heap.ArenaAllocator.init(state.scratch_arena.allocator());
    defer stmnt_scratch_arena.deinit();
    var ctx = Context{
        .state = state,
        .ally = state.scratch_arena.allocator(),
        .stmnt_scratch_arena = stmnt_scratch_arena,
        .stmnt_scratch = stmnt_scratch_arena.allocator(),
        .root_module = state.modules.getPtr(opt.root_module_name).?,
    };

    if (!opt.root_scope_already_exists) {
        try gc.pushFrame();
    }
    defer if (!opt.root_scope_already_exists) gc.popFrame();

    if (!opt.root_scope_already_exists) {
        // make argv
        var list = try std.ArrayList(*Value).initCapacity(gc.allocator(), state.remaining_args.len);
        for (state.remaining_args) |arg| {
            list.appendAssumeCapacity(try gc.string(arg, .{ .origin = undefined, .origin_module = opt.root_module_name }));
        }
        const gc_args = try gc.list(list, .{ .origin = undefined, .origin_module = opt.root_module_name });
        try gc.insertSymbol("argv", gc_args);
    }

    try interpretRoot(&ctx);
}

fn interpretRoot(ctx: *Context) EvalError!void {
    for (ctx.root_module.ast.statements) |stmnt| {
        _ = try interpretStatement(ctx, stmnt);
    }
}

fn interpretStatement(ctx: *Context, stmnt: ast.Statement) EvalError!Returns {
    defer _ = ctx.stmnt_scratch_arena.reset(.retain_capacity);
    switch (stmnt) {
        .call => |call| _ = try evalCall(ctx, call),
        .var_decl => |var_decl| try interpretVarDecl(ctx, var_decl),
        .assignment => |assign| try interpretAssign(ctx, assign),
        .branches => |br| return try interpretBranches(ctx, br),
        .scope => |scope| return try interpretScope(ctx, scope, .{ .new_frame = true }),
        .func => unreachable, // this should never happen
        .import => unreachable, // - " -
        .ret => |ret| return try evalReturn(ctx, ret),
        .loop => |loop| return try interpretLoop(ctx, loop),
        .brk => return .did_break,
        .cont => return .did_continue,
    }
    return .did_not_return;
}

fn interpretVarDecl(ctx: *Context, var_decl: ast.VarDecl) !void {
    switch (try evalExpression(ctx, var_decl.expression)) {
        .value => |value| {
            assert(var_decl.tokens.len == 1);
            try gc.insertSymbol(var_decl.tokens[0].value, value);
        },
        .nothing => {
            const ptr = ptrAdd(var_decl.tokens[0], 3);
            return errRequiresValue(ctx, ptr);
        },
        .values => |vals| {
            assert(var_decl.tokens.len == vals.len);
            for (var_decl.tokens, 0..) |tok, i| {
                const value = vals[i];
                try gc.insertSymbol(tok.value, value);
            }
        },
    }
}

fn interpretAssign(ctx: *Context, assign: ast.Assignment) !void {
    if (try interpretAssignToModuleSucceded(ctx, assign)) return;
    const entry = gc.getEntry(assign.variable.token.value) orelse {
        return errNeverDeclared(ctx, assign.variable.token);
    };

    const value_to_insert = try interpretExpressionToAssign(ctx, assign);
    if (assign.accessor) |*accessor| {
        const original_record = entry.value_ptr.*;
        try assignToAccessor(accessor, original_record, value_to_insert);
    } else {
        entry.value_ptr.* = value_to_insert;
    }
}

fn interpretAssignToModuleSucceded(ctx: *Context, assign: ast.Assignment) !bool {
    const module = modules.lookup(assign.variable.token.value) orelse {
        return false;
    };
    if (!module.was_imported) {
        return false;
    }
    const record = module.record;
    const value_to_assign = try interpretExpressionToAssign(ctx, assign);

    if (assign.accessor) |*accessor| {
        try assignToAccessor(accessor, record, value_to_assign);
        return true;
    }
    return err(ctx, .{
        .msg = try std.fmt.allocPrint(ctx.ally, "'{s}' is a module and cannot be overwritten", .{assign.variable.token.value}),
        .offending_token = assign.token,
    }, InterpreterError.BadAssign);
}

fn interpretExpressionToAssign(ctx: *Context, assign: ast.Assignment) !*Value {
    const value = switch (try evalExpression(ctx, assign.expression)) {
        .value => |v| v,
        .nothing => {
            const ptr = ptrAdd(assign.variable.token, 2);
            return errRequiresValue(ctx, ptr);
        },
        .values => unreachable, //TODO: consider this
    };

    const was_owned = switch (assign.expression.as) {
        .variable, .closure => true,
        .list_literal,
        .record_literal,
        .bareword,
        .bool_literal,
        .integer_literal,
        .string_literal,
        .capturing_call,
        .unary_operator,
        .binary_operator,
        => false,
    };

    return if (was_owned) try gc.cloneOrReference(value) else value;
}

fn assignToAccessor(accessor: *const ast.Accessor, original_record: *Value, value_to_insert: *Value) !void {
    var current = accessor;
    var record = original_record;
    while (true) {
        if (current.child == null) {
            try record.as.record.put(current.member.token.value, value_to_insert);
            break;
        }
        record = record.as.record.get(current.member.token.value) orelse unreachable;
        current = current.child.?;
    }
}

fn interpretBranches(ctx: *Context, branches: ast.Branches) !Returns {
    try gc.pushFrame();
    defer gc.popFrame();

    for (branches) |branch| {
        if (branch.condition) |expr| {
            const value = try evalExpression(ctx, expr);
            switch (value) {
                .value => |v| {
                    switch (v.as) {
                        .boolean => |b| {
                            if (b) {
                                return try interpretScope(ctx, branch.scope, .{ .new_frame = true });
                            }
                        },
                        // needs boolean
                        else => unreachable,
                    }
                },
                // needs value
                .nothing => return errRequiresValue(ctx, ast.tokenFromExpr(expr)),
                .values => unreachable, //TODO: consider
            }
        } else {
            return try interpretScope(ctx, branch.scope, .{ .new_frame = true });
        }
    }
    return .did_not_return;
}

fn interpretScope(ctx: *Context, scope: ast.Scope, opt: struct { new_frame: bool }) !Returns {
    if (opt.new_frame) {
        try gc.pushFrame();
    }
    defer if (opt.new_frame) gc.popFrame();

    for (scope) |stmnt| {
        const returns = try interpretStatement(ctx, stmnt);
        switch (returns) {
            .did_return => |r| {
                try appendParentRootIfReturnedValue(r);
                return returns;
            },
            .did_not_return => {},
            .did_break => return .did_break,
            .did_continue => return .did_continue,
        }
    }
    return .did_not_return;
}

fn interpretLoop(ctx: *Context, loop: ast.Loop) !Returns {
    try gc.pushFrame();
    defer gc.popFrame();

    if (loop.init_op) |init_op| {
        switch (init_op) {
            .assignment => |assign| try interpretAssign(ctx, assign),
            .declaration => |decl| try interpretVarDecl(ctx, decl),
        }
    }

    while (true) {
        if (loop.expr) |expr| {
            const value = try evalExpression(ctx, expr);
            switch (value) {
                .value => |val| {
                    switch (val.as) {
                        .boolean => |b| {
                            if (!b) {
                                break;
                            }
                        },
                        // needs boolean
                        else => unreachable,
                    }
                },
                // needs value
                .nothing => return errRequiresValue(ctx, ast.tokenFromExpr(expr)),
                .values => unreachable, //TODO: consider
            }
        }

        const returned = try interpretScope(ctx, loop.scope, .{ .new_frame = true });
        switch (returned) {
            .did_return => |r| {
                try appendParentRootIfReturnedValue(r);
                return returned;
            },
            .did_not_return, .did_continue => {},
            .did_break => return .did_not_return,
        }

        if (loop.post_op) |post_op| {
            switch (post_op) {
                .assignment => |assign| try interpretAssign(ctx, assign),
                .call => |call| _ = try evalCall(ctx, call),
            }
        }
    }

    return .did_not_return;
}

fn evalReturn(ctx: *Context, ret: ast.Return) !Returns {
    if (ret.expression) |expr| {
        const eval_expr: Result = try evalExpression(ctx, expr);
        switch (eval_expr) {
            .nothing => {},
            .value => |value| {
                // share frame with parent
                try gc.appendParentRoot(value);
            },
            .values => unreachable, //TODO: consider
        }
        return .{ .did_return = eval_expr };
    }
    return .{ .did_return = nothing };
}

fn evalCall(ctx: *Context, call: ast.Call) !Result {
    const name = call.token.value;

    if (call.accessor) |accessor| {
        const owning_module_name = call.token.value;
        if (modules.lookup(owning_module_name)) |internal_module| {
            return evalInternalModuleCall(ctx, call, internal_module);
        }
        const module = ctx.state.modules.get(owning_module_name) orelse {
            return errNeverImported(ctx, call.token, owning_module_name);
        };
        const func_name = accessor.member.token.value;
        const func = module.ast.functions.get(func_name) orelse {
            return errNeverExported(ctx, call.token, owning_module_name, func_name);
        };
        return try evalFunctionCall(ctx, func, call);
    }

    if (builtins.lookup(name)) |builtin_info| {
        const result = try evalBuiltin(ctx, call, builtin_info.func);
        switch (result) {
            .value => |v| {
                try gc.appendRoot(v);
            },
            .nothing => {},
            .values => unreachable, //TODO: consider this
        }
        return result;
    } else if (ctx.root_module.ast.functions.getPtr(name)) |func| {
        return try evalFunctionCall(ctx, func.*, call);
    } else if (gc.getEntry(name)) |entry| {
        switch (entry.value_ptr.*.as) {
            .closure => |c| {
                return try evalClosureCall(ctx, c, call);
            },
            else => unreachable,
        }
    } else {
        return try evalProc(ctx, call);
    }
}

fn evalInternalModuleCall(ctx: *Context, call: ast.Call, module: *const modules.InternalModule) !Result {
    const module_name = call.token.value;
    const fn_name = call.accessor.?.member.token.value;

    if (!module.was_imported) {
        return errNeverImported(ctx, call.token, module_name);
    }

    const builtin = module.builtins_table.get(fn_name) orelse {
        return errNeverExported(ctx, call.token, module_name, fn_name);
    };

    return evalBuiltin(ctx, call, builtin.func);
}

//TODO: this
fn isLocalScript(ctx: *Context, call: ast.Call) !bool {
    // local must be prefixed with './'
    if (std.mem.startsWith(u8, call.token.value, "./")) {
        return false;
    }
    //TODO: check what kind of local file it is, if it contains cherry script, then read the script and execute
    const file = try std.fs.cwd().openFile(call.token.value, .{});
    defer file.close();
    _ = ctx; // autofix
}

fn evalBuiltin(ctx: *Context, call: ast.Call, builtin: *const builtins.BuiltinFn) !Result {
    var args = try evalArgs(ctx, call.arguments);
    defer args.deinit();
    return builtin(ctx.state, args.items, call) catch |e| {
        switch (e) {
            error.TypeMismatch, error.ArgsCountMismatch => {
                if (ctx.state.error_report.?.offending_expr_idx) |idx| {
                    const expr = call.arguments[idx];
                    ctx.state.error_report.?.offending_token = ast.tokenFromExpr(expr);
                } else {
                    ctx.state.error_report.?.offending_token = call.token;
                }
            },
            else => {},
        }
        return e;
    };
}

fn evalFunctionCall(ctx: *Context, func: ast.Func, call: ast.Call) !Result {
    try gc.pushFrame();
    defer gc.popFrame();

    var args = try evalArgs(ctx, call.arguments);
    defer args.deinit();

    assert(func.signature.parameters.len == args.items.len);

    for (func.signature.parameters, args.items) |par, arg| {
        try gc.insertSymbol(par.name, arg);
    }

    for (func.scope) |stmnt| {
        const returned = try interpretStatement(ctx, stmnt);
        switch (returned) {
            .did_return => |what_it_returned| {
                return switch (what_it_returned) {
                    .nothing => nothing,
                    .value => |val| something(val),
                    .values => unreachable, //TODO: consider this
                };
            },
            .did_not_return => {},
            .did_break, .did_continue => unreachable, //TODO: handle this
        }
    }
    return nothing;
}

fn evalClosureCall(ctx: *Context, closure: values.Closure, call: ast.Call) !Result {
    try gc.pushFrame();
    defer gc.popFrame();

    var args = try evalArgs(ctx, call.arguments);
    defer args.deinit();

    assert(closure.ast.arguments.len == args.items.len);

    for (closure.ast.arguments, args.items) |par, arg| {
        try gc.insertSymbol(par.token.value, arg);
    }

    switch (closure.ast.as) {
        .expression => |expr| {
            return try evalExpression(ctx, expr.*);
        },
        .body => |body| {
            for (body) |stmnt| {
                const returned = try interpretStatement(ctx, stmnt);
                switch (returned) {
                    .did_return => |what_it_returned| {
                        return switch (what_it_returned) {
                            .nothing => nothing,
                            .value => |val| something(val),
                            .values => unreachable, //TODO: consider this
                        };
                    },
                    .did_not_return => {},
                    .did_break, .did_continue => unreachable, //TODO: handle this
                }
            }
        },
    }

    return nothing;
}

fn evalProc(ctx: *Context, call: ast.Call) !Result {
    const capturing = call.capturing_external_cmd;
    var procs = try std.ArrayList(std.process.Child).initCapacity(ctx.stmnt_scratch, 4);
    var redirect_output_to_file: ?*const tokens.Token = null;
    const redirect_stdin_from_file = call.redirect_in;

    var ptr: ?*const ast.Call = &call;
    while (ptr) |call_ptr| {
        var new_proc = try proc(ctx, call_ptr);
        new_proc.env_map = &ctx.state.env_map;

        try procs.append(new_proc);

        if (call_ptr.redirect_out) |out| {
            redirect_output_to_file = out.token;
        }

        ptr = call_ptr.pipe;
    }

    const redirecting_out = redirect_output_to_file != null;
    const redirecting_in = redirect_stdin_from_file != null;

    for (procs.items, 0..) |*p, idx| {
        if (capturing or redirecting_out or procs.items.len > 1) {
            if (idx > 0 or redirecting_in) {
                p.stdin_behavior = .Pipe;
            }

            const piping = idx + 1 < procs.items.len;

            if (capturing or redirecting_out or piping) {
                p.stdout_behavior = .Pipe;
            }
        }
    }

    for (procs.items) |*p| {
        p.spawn() catch |e| {
            switch (e) {
                error.FileNotFound => {
                    ctx.state.error_report = .{
                        .trailing = false,
                        .offending_token = call.token,
                        .msg = try std.fmt.allocPrint(ctx.ally, "Could not find command in system", .{}),
                    };
                    return error.CommandNotFound;
                },
                else => {},
            }
            return e;
        };
    }

    var capture: ?[]const u8 = null;
    errdefer if (capture) |c| gc.allocator().free(c);

    if (redirect_stdin_from_file) |stdin_file| {
        const expr = stdin_file.expression.*;
        const file_name = switch (try evalExpression(ctx, expr)) {
            .value => |val| try val.asStr(gc.allocator()),
            .nothing => {
                return errRequiresValue(ctx, ast.tokenFromExpr(expr));
            },
            .values => {
                return errRequiresSingleValue(ctx, ast.tokenFromExpr(expr));
            },
        };
        defer gc.allocator().free(file_name);
        //_ = result; // autofix
        //std.fs.cwd().openFile(in_token.expression.
        var file = std.fs.cwd().openFile(file_name, .{}) catch {
            ctx.state.reportError(.{
                .trailing = false,
                .offending_token = ast.tokenFromExpr(expr),
                .msg = try std.fmt.allocPrint(ctx.ally, "Could not open file '{s}' for redirection", .{file_name}),
            });
            return InterpreterError.UnableToOpenFileDuringRedirect;
        };
        defer file.close();

        const first_proc = &procs.items[0];
        const reader = file.reader();
        const writer = first_proc.stdin.?.writer();
        var fifo = std.fifo.LinearFifo(u8, .{ .Static = 4096 }).init();
        try fifo.pump(reader, writer);
        first_proc.stdin.?.close();
        first_proc.stdin = null;
    }

    if (procs.items.len > 1) {
        var idx: usize = 0;
        while (idx < procs.items.len) : (idx += 1) {
            if (idx > 0) {
                var prev = &procs.items[idx - 1];
                var this = &procs.items[idx];
                var fifo = std.fifo.LinearFifo(u8, .{ .Static = 4096 }).init();
                try fifo.pump(prev.stdout.?.reader(), this.stdin.?.writer());
                if (this.stdin) |in| {
                    in.close();
                    this.stdin = null;
                }
            }
        }
    }

    if (capturing) {
        assert(procs.getLast().stdout != null);
        capture = try procs.getLast().stdout.?.readToEndAlloc(gc.allocator(), std.math.maxInt(u64));
    } else if (redirect_output_to_file) |output_file| {
        var file = std.fs.cwd().createFile(output_file.value, .{}) catch {
            ctx.state.reportError(.{
                .trailing = false,
                .offending_token = output_file,
                .msg = try std.fmt.allocPrint(ctx.ally, "Could not open file '{s}' for redirection", .{output_file.value}),
            });
            return InterpreterError.UnableToOpenFileDuringRedirect;
        };
        defer file.close();
        const reader = procs.getLast().stdout.?.reader();
        const writer = file.writer();
        var fifo = std.fifo.LinearFifo(u8, .{ .Static = 4096 }).init();
        try fifo.pump(reader, writer);
    }

    var last_code: u8 = 0;

    for (procs.items) |*p| {
        //TODO: handle term
        const tm = p.wait() catch |e| {
            switch (e) {
                error.FileNotFound => {
                    ctx.state.reportError(.{
                        .trailing = false,
                        .offending_token = call.token,
                        .msg = try std.fmt.allocPrint(ctx.ally, "Could not find command in system", .{}),
                    });
                    return error.CommandNotFound;
                },
                error.AccessDenied => {
                    ctx.state.reportError(.{
                        .trailing = false,
                        .offending_token = call.token,
                        .msg = try std.fmt.allocPrint(ctx.ally, "Unable to run '{s}', permission denied", .{p.argv[0]}),
                    });
                    return error.CommandNotFound;
                },
                else => {},
            }
            return e;
        };
        last_code = tm.Exited;
    }

    if (capturing) {
        const opt = gc.ValueOptions{
            .origin = call.token,
            .origin_module = ctx.root_module.filename,
        };
        const vals = try ctx.stmnt_scratch.alloc(*Value, 2);
        vals[0] = try gc.allocedString(capture.?, opt);
        vals[1] = try gc.integer(@intCast(last_code), opt);
        return multiple(vals);
    }
    return nothing;
}

fn proc(ctx: *Context, call: *const ast.Call) !std.process.Child {
    const name = call.token.value;
    var args = try std.ArrayList([]const u8).initCapacity(ctx.stmnt_scratch, call.arguments.len);

    try args.append(name);
    for (call.arguments) |arg| {
        switch (try evalExpression(ctx, arg)) {
            .value => |value| {
                switch (value.as) {
                    .list => |l| {
                        for (l.items) |e| {
                            try args.append(try e.asStr(ctx.stmnt_scratch));
                        }
                    },
                    else => try args.append(try value.asStr(ctx.stmnt_scratch)),
                }
            },
            .nothing => unreachable, // this construct expects only values
            .values => unreachable, //TODO: consider this
        }
    }

    return std.process.Child.init(args.items, ctx.stmnt_scratch);
}

fn evalArgs(ctx: *Context, arguments: []const ast.Expression) !std.ArrayList(*Value) {
    var args = try std.ArrayList(*Value).initCapacity(ctx.stmnt_scratch, arguments.len);
    for (arguments) |arg| {
        switch (try evalExpression(ctx, arg)) {
            .value => |value| try args.append(value),
            .nothing => unreachable, // this construct expects only values
            .values => |vals| {
                //TODO: this is like this to allow for external processes to be
                // piped, but it perhaps shouldn't be like this for user-defined
                // functions, at least not if we are allowing people to return
                // multiple values (which we probably should)
                try args.append(vals[0]);
            },
        }
    }
    return args;
}

fn evalUnaryOperator(ctx: *Context, op: ast.UnaryOperator) EvalError!*Value {
    return switch (op.token.kind) {
        .Bang => try evalNot(ctx, op),
        // none of these should be set, if so, developer error
        else => unreachable,
    };
}

fn evalNot(ctx: *Context, op: ast.UnaryOperator) EvalError!*Value {
    const expr_result = try evalExpression(ctx, op.expression.*);
    const val = switch (expr_result) {
        .value => |val| val,
        .nothing => {
            return errRequiresValue(ctx, ast.tokenFromExpr(op.expression.*));
        },
        .values => {
            return errRequiresSingleValue(ctx, ast.tokenFromExpr(op.expression.*));
        },
    };

    const boolean = switch (val.as) {
        .string, .integer, .float, .list, .record, .closure => unreachable, //TODO: handle error
        .boolean => |b| b,
    };

    const opt: gc.ValueOptions = .{
        .origin = op.token,
        .origin_module = ctx.state.current_module_in_process,
    };

    return try gc.boolean(!boolean, opt);
}

fn evalBinaryOperator(ctx: *Context, op: ast.BinaryOperator) EvalError!*Value {
    return switch (op.token.kind) {
        .Equals => try evalEq(ctx, op),
        .NotEquals => try evalNE(ctx, op),
        else => unreachable, // programmer error
    };
}

fn evalEq(ctx: *Context, op: ast.BinaryOperator) EvalError!*Value {
    const opt = gc.ValueOptions{
        .origin = op.token,
        .origin_module = ctx.state.current_module_in_process,
    };
    return try gc.boolean(try evalEqImpl(ctx, op), opt);
}

fn evalNE(ctx: *Context, op: ast.BinaryOperator) EvalError!*Value {
    const opt = gc.ValueOptions{
        .origin = op.token,
        .origin_module = ctx.state.current_module_in_process,
    };
    return try gc.boolean(!try evalEqImpl(ctx, op), opt);
}

fn evalEqImpl(ctx: *Context, op: ast.BinaryOperator) EvalError!bool {
    const lhs = try evalExpression(ctx, op.lhs.*);
    const rhs = try evalExpression(ctx, op.rhs.*);

    //TODO: handle type error
    const order = lhs.value.compare(rhs.value) catch unreachable;
    return switch (order) {
        .equal => true,
        .failure => |fail| return builtins.typeMismatchError(ctx.state, fail.wants, fail.got, 1),
        else => false,
    };
}

fn evalExpression(ctx: *Context, expr: ast.Expression) EvalError!Result {
    const base_expr = switch (expr.as) {
        .bareword => |bw| something(try evalBareword(ctx, bw)),
        .string_literal => |str| something(try evalStringLiteral(ctx, str)),
        .integer_literal => |int| something(try evalIntegerLiteral(ctx, int)),
        .bool_literal => |bl| something(try evalBoolLiteral(ctx, bl)),
        .variable => |variable| something(try evalVariable(ctx, variable)),
        .capturing_call => |cap_inv| try evalCall(ctx, cap_inv),
        .list_literal => |list| something(try evalListLiteral(ctx, list)),
        .record_literal => |record| something(try evalRecordLiteral(ctx, record)),
        .unary_operator => |op| something(try evalUnaryOperator(ctx, op)),
        .binary_operator => |op| something(try evalBinaryOperator(ctx, op)),
        .closure => |closure| something(try evalClosure(ctx, closure)),
    };

    if (expr.accessor) |*accessor| {
        var current = accessor;
        var record = base_expr.value;
        while (true) {
            if (current.child == null) {
                switch (record.as) {
                    .record => |r| return something(r.get(current.member.token.value) orelse unreachable),
                    else => {},
                }
                //TODO: keep working here
                ctx.state.error_report = .{
                    .offending_token = current.member.token,
                    .msg = try std.fmt.allocPrint(ctx.ally, "'{s}' is not a record, and so has no member named '{s}'.", .{ record.origin.value, current.member.token.value }),
                };
                return InterpreterError.NonRecordAccessAttempt;
            }
            record = record.as.record.get(current.member.token.value) orelse {
                ctx.state.error_report = .{
                    .offending_token = current.member.token,
                    .msg = try std.fmt.allocPrint(ctx.ally, "record has no member '{s}'.", .{current.member.token.value}),
                };
                return InterpreterError.EntryNotFoundWithinRecord;
            };
            current = current.child.?;
        }
    } else {
        return base_expr;
    }
}

fn evalClosure(ctx: *Context, closure_ast: ast.Closure) !*Value {
    const opt = gc.ValueOptions{
        .origin = closure_ast.token,
        .origin_module = ctx.root_module.filename,
    };
    return try gc.appendRootV(try gc.closure(closure_ast, opt));
}

fn evalBareword(ctx: *Context, bw: ast.Bareword) !*Value {
    const opt = gc.ValueOptions{
        .origin = bw.token,
        .origin_module = ctx.root_module.filename,
    };

    const contextualized = try strings.processBareword(ctx.state, ctx.stmnt_scratch, bw.token.value);
    const value = switch (contextualized) {
        .string => |str| try gc.string(str, opt),
        .glob => |globbed| blk: {
            var list = values.List.init(gc.allocator());
            errdefer list.deinit();
            for (globbed) |g| {
                try list.append(try gc.string(g, opt));
            }
            break :blk try gc.list(list, opt);
        },
    };
    try gc.appendRoot(value);
    return value;
}

fn evalStringLiteral(ctx: *Context, str: ast.StringLiteral) !*Value {
    const opt = gc.ValueOptions{
        .origin = str.token,
        .origin_module = ctx.root_module.filename,
    };
    const contextualized = try strings.processStrLiteral(ctx.state, ctx.stmnt_scratch, str.token.value);
    const value = try gc.string(contextualized, opt);
    try gc.appendRoot(value);
    return value;
}

fn evalIntegerLiteral(ctx: *Context, int: ast.IntegerLiteral) !*Value {
    const opt = gc.ValueOptions{
        .origin = int.token,
        .origin_module = ctx.root_module.filename,
    };
    //TODO: this should not be done here...
    const i = std.fmt.parseInt(i64, int.token.value, 10) catch unreachable;
    const value = try gc.integer(i, opt);
    try gc.appendRoot(value);
    return value;
}

fn evalBoolLiteral(ctx: *Context, bl: ast.BoolLiteral) !*Value {
    const opt = gc.ValueOptions{
        .origin = bl.token,
        .origin_module = ctx.root_module.filename,
    };
    const value = try gc.boolean(bl.token.kind == .True, opt);
    try gc.appendRoot(value);
    return value;
}

fn evalVariable(ctx: *Context, variable: ast.Variable) !*Value {
    if (modules.lookup(variable.token.value)) |module| {
        if (module.was_imported) {
            return module.record;
        }
    }
    return gc.getSymbol(variable.token.value) orelse {
        return errNeverDeclared(ctx, variable.token);
    };
}

fn evalListLiteral(ctx: *Context, list_literal: ast.ListLiteral) !*Value {
    const opt = gc.ValueOptions{
        .origin = list_literal.token,
        .origin_module = ctx.root_module.filename,
    };
    var list = try values.List.initCapacity(gc.allocator(), list_literal.items.len);
    errdefer list.deinit();
    for (list_literal.items) |item| {
        const value = try evalExpression(ctx, item);
        switch (value) {
            .value => |v| try list.append(v),
            .nothing => unreachable, // Construct needs value
            .values => unreachable, //TODO: consider
        }
    }
    const value = try gc.list(list, opt);
    try gc.appendRoot(value);
    return value;
}

fn evalRecordLiteral(ctx: *Context, record_literal: ast.RecordLiteral) !*Value {
    const opt = gc.ValueOptions{
        .origin = record_literal.token,
        .origin_module = ctx.root_module.filename,
    };
    if (record_literal.items.len == 0) {
        return gc.emptyRecord(opt);
    }

    var record = values.Record.init(gc.allocator());
    for (record_literal.items) |item| {
        const value = switch (try evalExpression(ctx, item.value)) {
            .value => |v| v,
            .nothing => unreachable,
            .values => unreachable,
        };
        try record.put(item.key.value, value);
    }
    const value = try gc.record(record, opt);
    try gc.appendRoot(value);
    return value;
}

fn appendParentRootIfReturnedValue(result: Result) !void {
    switch (result) {
        .value => |v| try gc.appendParentRoot(v),
        .nothing => {},
        .values => unreachable, //TODO: consider
    }
}

fn errNeverDeclared(ctx: *Context, token: *const tokens.Token) InterpreterError {
    ctx.state.error_report = .{
        .msg = "Variable is used but never declared",
        .trailing = false,
        .offending_token = token,
    };
    return InterpreterError.BadVariableLookup;
}

fn errRequiresValue(ctx: *Context, token: *const tokens.Token) InterpreterError {
    ctx.state.error_report = .{
        .msg = "Context requires value, but the expression was unable to produce it",
        .trailing = false,
        .offending_token = token,
    };
    return InterpreterError.ValueRequired;
}

fn errRequiresSingleValue(ctx: *Context, token: *const tokens.Token) InterpreterError {
    ctx.state.error_report = .{
        .msg = "Context requires value, but the expression produces multiple",
        .trailing = false,
        .offending_token = token,
    };
    return InterpreterError.ValueRequired; //TODO: should this be a unique error? hmmm...
}

fn errNeverImported(ctx: *Context, token: *const tokens.Token, owning_module_name: []const u8) (EvalError || std.mem.Allocator.Error) {
    ctx.state.error_report = .{
        .trailing = false,
        .offending_token = token,
        .msg = try std.fmt.allocPrint(ctx.ally, "Module '{s}' was never imported.", .{owning_module_name}),
    };
    return EvalError.ModuleNotFound;
}

fn errNeverExported(ctx: *Context, token: *const tokens.Token, owning_module_name: []const u8, func_name: []const u8) (EvalError || std.mem.Allocator.Error) {
    ctx.state.error_report = .{
        .trailing = false,
        .offending_token = token,
        .msg = try std.fmt.allocPrint(ctx.ally, "Module '{s}' does not export a function or variable named '{s}'.", .{ owning_module_name, func_name }),
    };
    return EvalError.FunctionNotFoundWithinModule;
}

fn err(ctx: *Context, error_report: pipeline.ErrorReport, e: InterpreterError) InterpreterError {
    ctx.state.error_report = error_report;
    return e;
}

fn ptrAdd(token: *const tokens.Token, steps: usize) *const tokens.Token {
    const original_adress = @intFromPtr(token);
    const offset_adress = original_adress + steps * @sizeOf(tokens.Token);
    return @ptrFromInt(offset_adress);
}
