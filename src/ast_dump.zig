const std = @import("std");
const ast = @import("ast.zig");

const print = std.debug.print;

var depth: u32 = 0;

fn up() void {
    std.debug.assert(depth > 0);
    depth -= 1;
}

fn down() void {
    depth += 1;
}

fn indent() void {
    for (0..depth) |_| {
        print("  ", .{});
    }
}

fn leaf(comptime str: []const u8, args: anytype) void {
    indent();
    print(str, args);
    down();
}

pub fn dump(module: ast.Module) void {
    defer up();
    leaf("Module: {s}\n", .{module.name});

    var it = module.functions.valueIterator();
    while (it.next()) |func| {
        dumpFunc(func.*);
    }

    for (module.statements) |stmnt| {
        dumpStatement(stmnt);
    }
}

pub fn dumpStatement(stmnt: ast.Statement) void {
    defer up();
    leaf("Statement:\n", .{});
    switch (stmnt) {
        .call => |inv| dumpCall(inv),
        .var_decl => |var_decl| dumpVarDecl(var_decl),
        .assignment => |assign| dumpAssign(assign),
        .branches => |br| dumpBranches(br),
        .scope => |scope| dumpScope(scope),
        .func => |func| dumpFunc(func),
        .import => |import| dumpImport(import),
        .ret => |ret| dumpReturn(ret),
        .loop => |loop| dumpLoop(loop),
        .brk => |brk| dumpBreak(brk),
        .cont => |cont| dumpContinue(cont),
    }
}

fn dumpVarDecl(var_decl: ast.VarDecl) void {
    defer up();
    leaf("Variable declaration:", .{});
    for (var_decl.tokens) |tok| {
        print(" '{s}' ", .{tok.value});
    }
    print("\n", .{});
    dumpExpression(var_decl.expression);
}

fn dumpAssign(assign: ast.Assignment) void {
    defer up();
    leaf("Assignment to: '{s}'\n", .{assign.variable.token.value});
    if (assign.accessor) |acc| {
        dumpAccessor(acc);
    }
    dumpExpression(assign.expression);
}

fn dumpBranches(branches: ast.Branches) void {
    defer up();
    leaf("Branches:\n", .{});
    for (branches) |branch| {
        dumpBranch(branch);
    }
}

fn dumpBranch(branch: ast.Branch) void {
    defer up();
    leaf("Branch, condition:\n", .{});
    if (branch.condition) |c| {
        dumpExpression(c);
    }
    dumpScope(branch.scope);
}

fn dumpScope(scope: ast.Scope) void {
    defer up();
    leaf("Scope:\n", .{});
    for (scope) |stmnt| {
        dumpStatement(stmnt);
    }
}

fn dumpFunc(func: ast.Func) void {
    defer up();
    leaf("Func: {s}\n", .{func.token.value});
    leaf("Params:\n", .{});
    for (func.signature.parameters) |param| {
        //TODO: needs more work to print all kinds of types
        leaf("{s}: {s}\n", .{ param.name, @tagName(param.param_type.type_info) });
        up();
    }
    up();
    if (func.signature.produces != .nothing) {
        //TODO: needs more work to print all kinds of types
        leaf("Produces: {s}\n", .{@tagName(func.signature.produces)});
        up();
    }
    dumpScope(func.scope);
}

fn dumpImport(import: ast.Import) void {
    defer up();
    leaf("Import: {s}\n", .{import.name});
    if (import.alias) |alias| {
        defer up();
        leaf("Alias: {s}\n", .{alias});
    }
}

fn dumpReturn(ret: ast.Return) void {
    defer up();
    leaf("Return\n", .{});
    if (ret.expression) |expr| {
        dumpExpression(expr);
    }
}

fn dumpInitOp(init_op: ast.InitOp) void {
    defer up();
    leaf("Init op\n", .{});
    switch (init_op) {
        .assignment => |assign| dumpAssign(assign),
        .declaration => |decl| dumpVarDecl(decl),
    }
}

fn dumpPostOp(post_op: ast.PostOp) void {
    defer up();
    leaf("Post op\n", .{});
    switch (post_op) {
        .assignment => |assign| dumpAssign(assign),
        .call => |call| dumpCall(call),
    }
}

fn dumpLoop(loop: ast.Loop) void {
    defer up();
    leaf("Loop\n", .{});
    if (loop.init_op) |init_op| {
        dumpInitOp(init_op);
    }
    if (loop.expr) |expr| {
        dumpExpression(expr);
    }
    if (loop.post_op) |post_op| {
        dumpPostOp(post_op);
    }
    dumpScope(loop.scope);
}

fn dumpBreak(_: ast.Break) void {
    defer up();
    leaf("Break\n", .{});
}

fn dumpContinue(_: ast.Continue) void {
    defer up();
    leaf("Continue\n", .{});
}

fn dumpCall(inv: ast.Call) void {
    defer up();
    leaf("Call: '{s}'\n", .{inv.token.value});
    for (inv.arguments) |expr| {
        dumpExpression(expr);
    }
    if (inv.pipe) |pipe| {
        leaf("|\n", .{});
        defer up();
        dumpCall(pipe.*);
    }
}

fn dumpUnaryOp(op: ast.UnaryOperator) void {
    defer up();
    leaf("UnaryOperator: {} ({s})\n", .{ op.token.kind, op.token.value });
    dumpExpression(op.expression.*);
}

fn dumpBinaryOp(op: ast.BinaryOperator) void {
    defer up();
    leaf("BinaryOperator: {} ({s})\n", .{ op.token.kind, op.token.value });

    leaf("lhs:\n", .{});
    dumpExpression(op.lhs.*);
    up();

    leaf("rhs:\n", .{});
    dumpExpression(op.rhs.*);
    up();
}

fn dumpExpression(expr: ast.Expression) void {
    defer up();
    leaf("Expression:\n", .{});
    switch (expr.as) {
        .bareword => |bw| dumpBareword(bw),
        .string_literal => |str| dumpStringLiteral(str),
        .integer_literal => |int| dumpIntegerLiteral(int),
        .bool_literal => |bl| dumpBoolLiteral(bl),
        .variable => |variable| dumpVariable(variable),
        .capturing_call => |inv| dumpCall(inv),
        .list_literal => |list| dumpListLiteral(list),
        .record_literal => |record| dumpRecordLiteral(record),
        .unary_operator => |op| dumpUnaryOp(op),
        .binary_operator => |op| dumpBinaryOp(op),
    }
    if (expr.accessor) |accessor| {
        dumpAccessor(accessor);
    }
}

fn dumpBareword(bw: ast.Bareword) void {
    defer up();
    leaf("Bareword: '{s}'\n", .{bw.token.value});
}

fn dumpStringLiteral(str: ast.StringLiteral) void {
    defer up();
    leaf("StringLiteral: '{s}'\n", .{str.token.value});
}

fn dumpIntegerLiteral(int: ast.IntegerLiteral) void {
    defer up();
    leaf("IntegerLiteral: '{s}'\n", .{int.token.value});
}

fn dumpBoolLiteral(bl: ast.BoolLiteral) void {
    defer up();
    leaf("BoolLiteral: '{s}'\n", .{if (bl.token.kind == .True) "true" else "false"});
}

fn dumpVariable(variable: ast.Variable) void {
    defer up();
    leaf("Variable: '${s}'\n", .{variable.token.value});
}

fn dumpListLiteral(list: ast.ListLiteral) void {
    defer up();
    leaf("ListLiteral: []\n", .{});
    for (list.items) |expr| {
        dumpExpression(expr);
    }
}

fn dumpRecordLiteral(record: ast.RecordLiteral) void {
    leaf("RecordLiteral: [=]\n", .{});
    defer up();
    for (record.items) |pair| {
        leaf("Key: {s}\n", .{pair.key.value});
        up();
        leaf("Value:\n", .{});
        dumpExpression(pair.value);
        up();
    }
}

fn dumpAccessor(acc: ast.Accessor) void {
    leaf("Accessor:\n", .{});
    defer up();
    dumpBareword(acc.member);
    if (acc.child) |child| {
        dumpAccessor(child.*);
    }
}
