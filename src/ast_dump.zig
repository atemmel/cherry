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
    leaf("Root:\n", .{});
    defer up();
    for (root.statements) |stmnt| {
        dumpStatement(stmnt);
    }
}

fn dumpStatement(stmnt: ast.Statement) void {
    leaf("Statement:\n", .{});
    defer up();
    switch (stmnt) {
        .invocation => |inv| dumpInvocation(inv),
    }
}

fn dumpInvocation(inv: ast.Invocation) void {
    leaf("Invocation: {s}\n", .{inv.token.value});
    defer up();
    for (inv.arguments) |expr| {
        dumpExpression(expr);
    }
}

fn dumpExpression(expr: ast.Expression) void {
    leaf("Expression:\n", .{});
    defer up();
    switch (expr) {
        .bareword => |bw| dumpBareword(bw),
        .stringLiteral => |str| dumpStringLiteral(str),
    }
}

fn dumpBareword(bw: ast.Bareword) void {
    leaf("Bareword: {s}\n", .{bw.token.value});
    up();
}

fn dumpStringLiteral(str: ast.StringLiteral) void {
    leaf("StringLiteral: {s}\n", .{str.token.value});
    up();
}
