const std = @import("std");
const builtin = @import("builtin");
const Token = @import("tokens.zig").Token;
const PipelineState = @import("pipeline.zig").State;
pub const dump = @import("ast_dump.zig").dump;

pub const errors = error{ParseFailed} || std.mem.Allocator.Error;

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

pub const ListLiteral = struct {
    token: *const Token,
    items: []const Expression,
};

pub const RecordLiteral = struct {
    pub const Pair = struct {
        key: union(enum) {
            bareword: Bareword,
            string: StringLiteral,
        },
        value: Expression,
    };
    token: *const Token,
    items: []const Pair,
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
    capturing_call: Call,
    list_literal: ListLiteral,
};

pub const Call = struct {
    token: *const Token,
    arguments: []Expression,
    pipe: ?*Call,
    capturing_external_cmd: bool,
};

pub const VarDecl = struct {
    token: *const Token, // contains identifier
    expression: Expression,
};

pub const Assignment = struct {
    token: *const Token, // contains variable
    expression: Expression,
};

pub const Scope = []const Statement;

pub const Branch = struct {
    condition: ?Expression,
    scope: Scope,
};

pub const Branches = []const Branch;

pub const Statement = union(enum) {
    call: Call,
    var_decl: VarDecl,
    assignment: Assignment,
    branches: Branches,
    scope: Scope,
};

pub const Root = struct {
    statements: []Statement,
};

const Context = struct {
    tokens: []const Token,
    idx: usize = 0,
    ally: std.mem.Allocator,
    state: *PipelineState,

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

    pub const Pair = struct {
        first: *const Token,
        second: *const Token,
    };

    pub fn getIfBoth(c: *Context, first: Token.Kind, second: Token.Kind) ?Pair {
        const checkpoint = c.idx;

        const token_0 = c.getIf(first) orelse return null;
        const token_1 = c.getIf(second) orelse {
            c.idx = checkpoint;
            return null;
        };
        return .{
            .first = token_0,
            .second = token_1,
        };
    }

    pub fn err(c: *Context, s: struct { error_source: *const Token, msg: []const u8, trailing: bool = false }) errors {
        if (builtin.mode == .Debug) {
            const bar = "===============================================";
            std.debug.print("Encountered an error while parsing the ast:\n\n{s}\n\n", .{bar});
            std.debug.dumpCurrentStackTrace(null);
            std.debug.print("\n{s}\n\nThe actual error message produced is as follows:\n\n", .{bar});
        }
        c.state.error_report = .{
            .offending_token = s.error_source,
            .msg = s.msg,
            .trailing = s.trailing,
        };
        return errors.ParseFailed;
    }
};

pub fn parse(state: *PipelineState) !Root {
    var ctx = Context{
        .tokens = state.tokens,
        .ally = state.arena,
        .state = state,
    };

    var statements = std.ArrayList(Statement).init(ctx.ally);
    defer statements.deinit();

    while (try parseStatement(&ctx)) |stmnt| {
        try statements.append(stmnt);
    }

    // being unable to parse statements without running out of tokens is an error
    if (!ctx.eot()) unreachable;

    return Root{
        .statements = try statements.toOwnedSlice(),
    };
}

fn parseStatement(ctx: *Context) errors!?Statement {
    if (try parseVarDeclaration(ctx)) |var_decl| {
        return Statement{
            .var_decl = var_decl,
        };
    } else if (try parsePipeline(ctx)) |inv| {
        return Statement{
            .call = inv,
        };
    } else if (try parseVarDeclaration(ctx)) |var_decl| {
        return Statement{
            .var_decl = var_decl,
        };
    } else if (try parseAssignment(ctx)) |assign| {
        return Statement{
            .assignment = assign,
        };
    } else if (try parseBranches(ctx)) |branches| {
        return Statement{
            .branches = branches,
        };
    } else if (try parseScope(ctx, true)) |scope| {
        return Statement{
            .scope = scope,
        };
    } else return null;
}

fn parseVarDeclaration(ctx: *Context) !?VarDecl {
    const checkpoint = ctx.idx;

    // 'var' MUST be followed by a identifer/bareword
    const id = ctx.getIf(.Bareword) orelse return null;

    // the identifer must be followed by a walrus operator
    if (ctx.getIfBoth(.Colon, .Assign) == null) {
        ctx.idx = checkpoint;
        return null;
    }

    // the assignment operation must be followed by an expression
    const expr = try parseExpression(ctx) orelse {
        return ctx.err(.{
            .error_source = ctx.peek(),
            .msg = "expected expression",
        });
    };

    // the assignment must be followed by a terminating newline or eot
    if (ctx.getIf(.Newline) == null and !ctx.eot()) {
        return ctx.err(.{
            .error_source = ctx.peek(),
            .msg = "expected newline (\\n)",
        });
    }

    return VarDecl{
        .token = id,
        .expression = expr,
    };
}

fn parseAssignment(ctx: *Context) !?Assignment {
    const checkpoint = ctx.idx;

    const variable = ctx.getIf(.Variable) orelse return null;
    _ = ctx.getIf(.Assign) orelse {
        ctx.idx = checkpoint;
        return null;
    };
    // Needs expression
    const expr = try parseExpression(ctx) orelse {
        return ctx.err(.{
            .error_source = ctx.peek(),
            .msg = "expected expression",
        });
    };

    // the assignment must be followed by a terminating newline or eot
    if (ctx.getIf(.Newline) == null and !ctx.eot()) {
        return ctx.err(.{
            .error_source = ctx.peek(),
            .msg = "expected newline (\\n)",
        });
    }

    return .{
        .token = variable,
        .expression = expr,
    };
}

fn parseBranches(ctx: *Context) !?Branches {
    if (ctx.getIf(.If) == null) {
        return null;
    }

    var branches = std.ArrayList(Branch).init(ctx.ally);
    defer branches.deinit();

    const first_expr = try parseExpression(ctx) orelse {
        return ctx.err(.{
            .error_source = ctx.peek(),
            .msg = "expected expression",
        });
    };
    const first_scope = try parseScope(ctx, false) orelse {
        return ctx.err(.{
            .error_source = ctx.peek(),
            .msg = "expected '{'",
        });
    };

    try branches.append(.{
        .scope = first_scope,
        .condition = first_expr,
    });

    var checkpoint = ctx.idx;

    if (ctx.getIf(.Newline) != null) {
        return try branches.toOwnedSlice();
    }

    while (ctx.getIf(.Else) != null and ctx.getIf(.If) != null) {
        const expr = try parseExpression(ctx) orelse {
            return ctx.err(.{
                .error_source = ctx.peek(),
                .msg = "expected expression",
            });
        };
        const scope = try parseScope(ctx, false) orelse {
            return ctx.err(.{
                .error_source = ctx.peek(),
                .msg = "expected '{'",
            });
        };

        try branches.append(.{
            .scope = scope,
            .condition = expr,
        });
        checkpoint = ctx.idx;
    }
    ctx.idx = checkpoint;

    if (ctx.getIf(.Else) != null) {
        const last_scope = try parseScope(ctx, true) orelse {
            return ctx.err(.{
                .error_source = ctx.peek(),
                .msg = "expected '{'",
            });
        };
        try branches.append(.{
            .scope = last_scope,
            .condition = null,
        });
    }

    return try branches.toOwnedSlice();
}

fn parseScope(ctx: *Context, needsNewline: bool) !?Scope {
    const lbrace = ctx.getIf(.LBrace);
    if (ctx.getIf(.LBrace) == null) {
        return null;
    }
    _ = ctx.getIf(.Newline);

    var statements = std.ArrayList(Statement).init(ctx.ally);
    defer statements.deinit();

    while (try parseStatement(ctx)) |stmnt| {
        try statements.append(stmnt);
    }

    _ = ctx.getIf(.RBrace) orelse {
        return ctx.err(.{
            .error_source = lbrace.?,
            .msg = "expected closing '}'",
            .trailing = true,
        });
    };
    if (needsNewline and ctx.getIf(.Newline) == null and !ctx.eot()) {
        return ctx.err(.{
            .error_source = ctx.peek(),
            .msg = "expected newline (\\n)",
        });
    }
    return try statements.toOwnedSlice();
}

fn parsePipeline(ctx: *Context) !?Call {
    const call = try parseIndividualPipeline(ctx, false);

    if (ctx.getIf(.Newline) == null and !ctx.eot()) {
        return ctx.err(.{
            .error_source = ctx.peek(),
            .msg = "expected newline (\\n)",
        });
    }

    return call;
}

fn parseIndividualPipeline(ctx: *Context, capturing: bool) !?Call {
    var pipeline_begin = try parseIndividualCall(ctx, capturing) orelse return null;

    var prev_node = &pipeline_begin;
    while (ctx.getIf(.Pipe) != null) {
        const next = try parseIndividualCall(ctx, capturing) orelse {
            return ctx.err(.{
                .error_source = ctx.peek(),
                .msg = "expected function call",
            });
        };
        const next_ptr = try ctx.ally.create(Call);
        next_ptr.* = next;
        prev_node.pipe = next_ptr;
        prev_node = next_ptr;
    }

    return pipeline_begin;
}

fn parseIndividualCall(ctx: *Context, capturing: bool) !?Call {
    const token = ctx.getIf(.Bareword);
    if (token == null) {
        return null;
    }

    var args = std.ArrayList(Expression).init(ctx.ally);
    defer args.deinit();
    while (try parseExpression(ctx)) |expr| {
        try args.append(expr);
    }

    return Call{
        .token = token.?,
        .arguments = try args.toOwnedSlice(),
        .pipe = null,
        .capturing_external_cmd = capturing,
    };
}

fn parseCapturingCall(ctx: *Context) errors!?Call {
    const left = ctx.getIf(.LParens);
    if (left == null) {
        return null;
    }

    const call = parseIndividualPipeline(ctx, true);

    if (ctx.getIf(.RParens) != null) {
        return call;
    }
    // missing rparens
    return ctx.err(.{
        .error_source = left.?,
        .msg = "expected closing ')'",
        .trailing = true,
    });
}

fn parseExpression(ctx: *Context) errors!?Expression {
    if (try parseCapturingCall(ctx)) |capturing_inv| {
        return Expression{
            .capturing_call = capturing_inv,
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
    } else if (try parseListLiteral(ctx)) |list| {
        return Expression{
            .list_literal = list,
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

fn parseListLiteral(ctx: *Context) !?ListLiteral {
    const token = ctx.getIf(.LBracket) orelse return null;

    var exprs = try std.ArrayList(Expression).initCapacity(ctx.ally, 5);
    defer exprs.deinit();

    // must have expressions
    while (try parseExpression(ctx)) |expr| {
        try exprs.append(expr);
    }

    // must find closing bracket
    _ = ctx.getIf(.RBracket) orelse {
        return ctx.err(.{
            .error_source = token,
            .msg = "expected one or more expresions, followed by a closing ']'",
            .trailing = true,
        });
    };

    return ListLiteral{
        .token = token,
        .items = try exprs.toOwnedSlice(),
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
