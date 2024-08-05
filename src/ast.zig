const std = @import("std");
const Token = @import("tokens.zig").Token;
const PipelineState = @import("pipeline.zig").State;
pub const dump = @import("ast_dump.zig").dump;

pub const Bareword = struct {
    token: *const Token,
};

pub const StringLiteral = struct {
    token: *const Token,
    interpolates: bool,
};

pub const IntegerLiteral = struct {
    token: *const Token,
};

pub const BoolLiteral = struct {
    token: *const Token,
};

pub const Variable = struct {
    token: *const Token,
};

pub const Expression = union(enum) {
    bareword: Bareword,
    string_literal: StringLiteral,
    integer_literal: IntegerLiteral,
    bool_literal: BoolLiteral,
    variable: Variable,
    capturing_invocation: Invocation,
};

pub const Invocation = struct {
    token: *const Token,
    arguments: []Expression,
};

pub const VarDecl = struct {
    token: *const Token, // contains identifier
    expression: Expression,
};

pub const Statement = union(enum) {
    invocation: Invocation,
    var_decl: VarDecl,
};

pub const Root = struct {
    statements: []Statement,
};

const Context = struct {
    tokens: []const Token,
    idx: usize = 0,
    ally: std.mem.Allocator,

    pub fn eot(c: Context) bool {
        return c.idx >= c.tokens.len;
    }

    pub fn next(c: *Context) void {
        c.idx += 1;
    }

    pub fn peek(c: Context) *const Token {
        return &c.tokens[c.idx];
    }

    pub fn getIf(c: *Context, kind: Token.Kind) ?*const Token {
        if (c.eot() or kind != c.peek().kind) {
            return null;
        }
        defer c.next();
        return c.peek();
    }
};

pub fn parse(state: *PipelineState) !Root {
    var ctx = Context{
        .tokens = state.tokens,
        .ally = state.arena,
    };

    var statements = std.ArrayList(Statement).init(ctx.ally);
    defer statements.deinit();

    while (try parseStatement(&ctx)) |stmnt| {
        try statements.append(stmnt);
    }

    // being unable to parse statements without running out of tokens is an error

    return Root{
        .statements = try statements.toOwnedSlice(),
    };
}

fn parseStatement(ctx: *Context) !?Statement {
    if (try parseInvocation(ctx)) |inv| {
        return Statement{
            .invocation = inv,
        };
    } else if (try parseVarDeclaration(ctx)) |var_decl| {
        return Statement{
            .var_decl = var_decl,
        };
    } else return null;
}

fn parseVarDeclaration(ctx: *Context) !?VarDecl {
    if (ctx.getIf(.Var) == null) {
        return null;
    }

    // 'var' MUST be followed by a identifer/bareword
    const id = ctx.getIf(.Bareword) orelse unreachable;

    // the identifer must be followed by an assignment (as of now)
    if (ctx.getIf(.Assign) == null) unreachable;

    // the assignment operation must be followed by an expression
    const expr = try parseExpression(ctx) orelse unreachable;

    // the assignment must be followed by a terminating newline or eot
    if (ctx.getIf(.Newline) == null and !ctx.eot()) unreachable;

    return VarDecl{
        .token = id,
        .expression = expr,
    };
}

fn parseInvocation(ctx: *Context) !?Invocation {
    const token = ctx.getIf(.Bareword);
    if (token == null) {
        return null;
    }

    var args = std.ArrayList(Expression).init(ctx.ally);
    defer args.deinit();
    while (try parseExpression(ctx)) |expr| {
        try args.append(expr);
    }

    if (ctx.getIf(.Newline) != null or ctx.eot()) {
        return Invocation{
            .token = token.?,
            .arguments = try args.toOwnedSlice(),
        };
    }
    std.debug.print("{}\n", .{ctx.peek()});
    unreachable;
}

fn parseCapturingInvocation(ctx: *Context) std.mem.Allocator.Error!?Invocation {
    const left = ctx.getIf(.LParens);
    if (left == null) {
        return null;
    }

    const token = ctx.getIf(.Bareword);
    if (token == null) {
        return null;
    }

    var args = std.ArrayList(Expression).init(ctx.ally);
    defer args.deinit();
    while (try parseExpression(ctx)) |expr| {
        try args.append(expr);
    }

    if (ctx.getIf(.RParens) != null) {
        return Invocation{
            .token = token.?,
            .arguments = try args.toOwnedSlice(),
        };
    }
    unreachable;
}

fn parseExpression(ctx: *Context) !?Expression {
    if (try parseCapturingInvocation(ctx)) |capturing_inv| {
        return Expression{
            .capturing_invocation = capturing_inv,
        };
    } else if (parseBareword(ctx)) |bareword| {
        return Expression{
            .bareword = bareword,
        };
    } else if (parseStringLiteral(ctx)) |string_literal| {
        return Expression{
            .string_literal = string_literal,
        };
    } else if (parseVariable(ctx)) |variable| {
        return Expression{
            .variable = variable,
        };
    } else if (parseIntegerLiteral(ctx)) |integer_literal| {
        return Expression{
            .integer_literal = integer_literal,
        };
    } else if (parseBoolLiteral(ctx)) |bool_literal| {
        return Expression{
            .bool_literal = bool_literal,
        };
    }
    return null;
}

fn parseBareword(ctx: *Context) ?Bareword {
    const token = ctx.getIf(.Bareword);
    if (token == null) {
        return null;
    }
    return Bareword{
        .token = token.?,
    };
}

fn parseStringLiteral(ctx: *Context) ?StringLiteral {
    const token = ctx.getIf(.StringLiteral);
    if (token == null) {
        return null;
    }
    return StringLiteral{
        .token = token.?,
        //TODO: this should handle escapes
        .interpolates = std.mem.indexOf(u8, token.?.value, "{") != null,
    };
}

fn parseIntegerLiteral(ctx: *Context) ?IntegerLiteral {
    const token = ctx.getIf(.IntegerLiteral);
    if (token == null) {
        return null;
    }
    return IntegerLiteral{
        .token = token.?,
    };
}

fn parseBoolLiteral(ctx: *Context) ?BoolLiteral {
    const token = ctx.getIf(.True) orelse ctx.getIf(.False);
    if (token == null) {
        return null;
    }
    return BoolLiteral{
        .token = token.?,
    };
}

fn parseVariable(ctx: *Context) ?Variable {
    const token = ctx.getIf(.Variable);
    if (token == null) {
        return null;
    }
    return Variable{
        .token = token.?,
    };
}
