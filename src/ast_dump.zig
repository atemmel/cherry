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

pub fn dump(root: ast.Root) void {
    defer up();
    leaf("Root:\n", .{});
    for (root.statements) |stmnt| {
        dumpStatement(stmnt);
    }
}

fn dumpStatement(stmnt: ast.Statement) void {
    defer up();
    leaf("Statement:\n", .{});
    switch (stmnt) {
        .invocation => |inv| dumpInvocation(inv),
        .var_decl => |var_decl| dumpVarDecl(var_decl),
        .assignment => |assign| dumpAssign(assign),
    }
}

fn dumpVarDecl(var_decl: ast.VarDecl) void {
    defer up();
    leaf("Variable declaration: '{s}'\n", .{var_decl.token.value});
    dumpExpression(var_decl.expression);
}

fn dumpAssign(assign: ast.Assignment) void {
    defer up();
    leaf("Assignment to: '{s}'\n", .{assign.token.value});
    dumpExpression(assign.expression);
}

fn dumpInvocation(inv: ast.Invocation) void {
    defer up();
    leaf("Invocation: '{s}'\n", .{inv.token.value});
    for (inv.arguments) |expr| {
        dumpExpression(expr);
    }
}

fn dumpExpression(expr: ast.Expression) void {
    defer up();
    leaf("Expression:\n", .{});
    switch (expr) {
        .bareword => |bw| dumpBareword(bw),
        .string_literal => |str| dumpStringLiteral(str),
        .integer_literal => |int| dumpIntegerLiteral(int),
        .bool_literal => |bl| dumpBoolLiteral(bl),
        .variable => |variable| dumpVariable(variable),
        .capturing_invocation => |inv| dumpInvocation(inv),
        .list_literal => |list| dumpListLiteral(list),
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
