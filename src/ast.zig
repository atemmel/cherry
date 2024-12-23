const std = @import("std");
const builtin = @import("builtin");
const semantics = @import("semantics.zig");
const Token = @import("tokens.zig").Token;
const PipelineState = @import("pipeline.zig").State;

pub const dump = @import("ast_dump.zig").dump;

pub const errors = error{ParseFailed} || std.mem.Allocator.Error;

const string_primitive_lookup = std.StaticStringMap(semantics.TypeInfo).initComptime(.{
    .{ "bool", .boolean },
    .{ "float", .float },
    .{ "int", .integer },
    .{ "string", .string },
});

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

pub const Member = Bareword;

pub const Accessor = struct {
    member: Member,
    child: ?*const Accessor,
};

pub const Variable = struct {
    token: *const Token,
};

pub const Expression = struct {
    as: union(enum) {
        bareword: Bareword,
        string_literal: StringLiteral,
        integer_literal: IntegerLiteral,
        bool_literal: BoolLiteral,
        variable: Variable,
        capturing_call: Call,
        list_literal: ListLiteral,
        record_literal: RecordLiteral,
    },
    accessor: ?Accessor,
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
    variable: Variable,
    accessor: ?Accessor,
    expression: Expression,
};

pub const Scope = []const Statement;

pub const Branch = struct {
    condition: ?Expression,
    scope: Scope,
};

pub const Parameter = struct {
    name: []const u8,
    param_type: Type,
};

pub const Type = struct {
    type_info: semantics.TypeInfo,
};

pub const Signature = struct {
    generics: []const []const u8 = &.{},
    parameters: []const Parameter,
    last_parameter_is_variadic: bool = false,
    produces: semantics.TypeInfo = .nothing,
};

pub const Func = struct {
    token: *const Token, // contains identifier
    signature: Signature,
    scope: Scope,
};

pub const Return = struct {
    expression: ?Expression,
};

pub const Branches = []const Branch;

pub const InitOp = union(enum) {
    assignment: Assignment,
    declaration: VarDecl,
};

pub const PostOp = union(enum) {
    assignment: Assignment,
    call: Call,
};

pub const Loop = struct {
    token: *const Token,
    init_op: ?InitOp,
    expr: ?Expression,
    post_op: ?PostOp,
    scope: Scope,
};

pub const Break = struct {
    token: *const Token,
};

pub const Continue = struct {
    token: *const Token,
};

pub const Import = struct {
    token: *const Token,
    name: []const u8,
    alias: ?[]const u8,
};

pub const Statement = union(enum) {
    call: Call,
    var_decl: VarDecl,
    assignment: Assignment,
    branches: Branches,
    scope: Scope,
    func: Func,
    import: Import,
    ret: Return,
    loop: Loop,
    brk: Break,
    cont: Continue,
};

pub const Module = struct {
    name: []const u8,
    // should only 'main' or 'repl' module be allowed to have statements?
    statements: []Statement,
    functions: std.StringHashMap(Func),
    imports: std.StringHashMap(Import),
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

    pub fn err(c: *Context, s: struct { msg: []const u8, trailing: bool = false }) errors {
        if (builtin.mode == .Debug) {
            const bar = "===============================================";
            std.debug.print("Encountered an error while parsing the ast:\n\n{s}\n\n", .{bar});
            std.debug.dumpCurrentStackTrace(null);
            std.debug.print("\n{s}\n\nThe actual error message produced is as follows:\n\n", .{bar});
        }

        const token = if (c.eot())
            &c.tokens[c.tokens.len - 1]
        else
            c.peek();

        c.state.error_report = .{
            .offending_token = token,
            .msg = s.msg,
            .trailing = s.trailing,
        };
        return errors.ParseFailed;
    }
};

pub fn parse(state: *PipelineState, tokens: []Token, name: []const u8) !Module {
    var ctx = Context{
        .tokens = tokens,
        .ally = state.arena,
        .state = state,
    };
    return parseModule(&ctx, name);
}

fn parseModule(ctx: *Context, name: []const u8) !Module {
    var statements = std.ArrayList(Statement).init(ctx.ally);
    defer statements.deinit();
    var functions = std.StringHashMap(Func).init(ctx.ally);
    defer functions.deinit();
    var imports = std.StringHashMap(Import).init(ctx.ally);
    defer imports.deinit();

    while (try parseStatement(ctx)) |stmnt| {
        switch (stmnt) {
            .func => |f| try functions.put(f.token.value, f),
            .import => |i| try imports.put(i.name, i),
            else => try statements.append(stmnt),
        }
    }

    // being unable to parse statements without running out of tokens is an error
    if (!ctx.eot()) {
        return ctx.err(.{ .msg = "Expected statement" });
    }

    return Module{
        .name = name,
        .statements = try statements.toOwnedSlice(),
        .functions = functions.move(),
        .imports = imports.move(),
    };
}

fn parseStatement(ctx: *Context) errors!?Statement {
    if (try parseVarDeclaration(ctx, .{})) |var_decl| {
        return Statement{
            .var_decl = var_decl,
        };
    } else if (try parsePipeline(ctx)) |inv| {
        return Statement{
            .call = inv,
        };
    } else if (try parseAssignment(ctx, .{})) |assign| {
        return Statement{
            .assignment = assign,
        };
    } else if (try parseBranches(ctx)) |branches| {
        return Statement{
            .branches = branches,
        };
    } else if (try parseLoop(ctx)) |loop| {
        return Statement{
            .loop = loop,
        };
    } else if (try parseReturn(ctx)) |ret| {
        return Statement{
            .ret = ret,
        };
    } else if (try parseFunc(ctx)) |func| {
        return Statement{
            .func = func,
        };
    } else if (try parseImport(ctx)) |import| {
        return Statement{
            .import = import,
        };
    } else if (try parseScope(ctx, true)) |scope| {
        return Statement{
            .scope = scope,
        };
    } else if (parseBreak(ctx)) |brk| {
        return Statement{
            .brk = brk,
        };
    } else if (parseContinue(ctx)) |cont| {
        return Statement{
            .cont = cont,
        };
    } else return null;
}

fn parseVarDeclaration(ctx: *Context, opt: struct { needs_newline: bool = true }) !?VarDecl {
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
            .msg = "expected expression",
        });
    };

    if (opt.needs_newline) {
        // the assignment must be followed by a terminating newline or eot
        if (ctx.getIf(.Newline) == null and !ctx.eot()) {
            return ctx.err(.{
                .msg = "expected newline (\\n)",
            });
        }
    }

    return VarDecl{
        .token = id,
        .expression = expr,
    };
}

fn parseAssignment(ctx: *Context, opt: struct { needs_newline: bool = true }) !?Assignment {
    const checkpoint = ctx.idx;

    const variable = try parseVariable(ctx) orelse return null;
    const accessor = try parseAccessorChain(ctx);
    _ = ctx.getIf(.Assign) orelse {
        ctx.idx = checkpoint;
        return null;
    };

    // Needs expression
    const expr = try parseExpression(ctx) orelse {
        return ctx.err(.{
            .msg = "expected expression",
        });
    };

    if (opt.needs_newline) {
        // the assignment must be followed by a terminating newline or eot
        if (ctx.getIf(.Newline) == null and !ctx.eot()) {
            return ctx.err(.{
                .msg = "expected newline (\\n)",
            });
        }
    }

    return .{
        .variable = variable,
        .accessor = accessor,
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
            .msg = "expected expression",
        });
    };
    const first_scope = try parseScope(ctx, false) orelse {
        return ctx.err(.{
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
                .msg = "expected expression",
            });
        };
        const scope = try parseScope(ctx, false) orelse {
            return ctx.err(.{
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
    if (lbrace == null) {
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
            .msg = "expected statement or closing '}'",
            .trailing = true,
        });
    };
    if (needsNewline and ctx.getIf(.Newline) == null and !ctx.eot()) {
        return ctx.err(.{
            .msg = "expected newline (\\n)",
        });
    }
    return try statements.toOwnedSlice();
}

fn parseFunc(ctx: *Context) !?Func {
    if (ctx.getIf(.Fn) == null) {
        return null;
    }

    const token = ctx.getIf(.Bareword) orelse {
        return ctx.err(.{
            .msg = "expected function name",
        });
    };

    var produces: semantics.TypeInfo = .nothing;
    var parameters = std.ArrayList(Parameter).init(ctx.ally);
    defer parameters.deinit();

    while (try parseParam(ctx)) |param| {
        try parameters.append(param);
    }

    if (try parseProducingType(ctx)) |type_info| {
        produces = type_info;
    }

    const scope = try parseScope(ctx, true) orelse {
        return ctx.err(.{
            .msg = "expected '{'",
        });
    };

    //TODO: support more of this
    return Func{
        .token = token,
        .scope = scope,
        .signature = .{
            .generics = &.{},
            .produces = produces,
            .parameters = try parameters.toOwnedSlice(),
            .last_parameter_is_variadic = false,
        },
    };
}

fn parseParam(ctx: *Context) !?Parameter {
    if (ctx.state.useSemanticAnalysis) {
        return parseParamWithType(ctx);
    }
    return parseParamWithoutType(ctx);
}

fn parseParamWithType(ctx: *Context) !?Parameter {
    const name_token = parseBareword(ctx) orelse {
        return null;
    };

    _ = ctx.getIf(.Colon) orelse {
        return ctx.err(.{
            .msg = "expected ':'",
        });
    };

    const parsed_type = try parseType(ctx) orelse {
        return ctx.err(.{
            .msg = "expected valid type",
        });
    };

    return Parameter{
        .name = name_token.token.value,
        .param_type = parsed_type,
    };
}

fn parseParamWithoutType(ctx: *Context) !?Parameter {
    const name_token = parseBareword(ctx) orelse {
        return null;
    };

    return Parameter{
        .name = name_token.token.value,
        .param_type = .{
            .type_info = .something,
        },
    };
}

//TODO: expand on this to handle, lists, records, etc
fn parseType(ctx: *Context) !?Type {
    if (try parseListType(ctx)) |list| {
        return list;
    }

    const primitive_type = try parsePrimitiveType(ctx) orelse {
        return null;
    };

    return Type{
        .type_info = primitive_type,
    };
}

fn parseListType(ctx: *Context) !?Type {
    _ = ctx.getIf(.LBracket) orelse {
        return null;
    };
    _ = ctx.getIf(.RBracket) orelse {
        return ctx.err(.{ .msg = "expected ']'" });
    };

    const inner = try ctx.ally.create(semantics.TypeInfo);
    inner.* = try parsePrimitiveType(ctx) orelse {
        return ctx.err(.{ .msg = "expected type" });
    };

    return Type{
        .type_info = .{
            .list = .{
                .of = inner,
            },
        },
    };
}

fn parsePrimitiveType(ctx: *Context) !?semantics.TypeInfo {
    const bareword = parseBareword(ctx) orelse {
        return null;
    };
    const name = bareword.token.value;
    return string_primitive_lookup.get(name) orelse {
        const msg = try std.fmt.allocPrint(ctx.ally, "'{s}' is not a recognized type", .{name});
        return ctx.err(.{ .msg = msg });
    };
}

fn parseProducingType(ctx: *Context) !?semantics.TypeInfo {
    _ = ctx.getIf(.Pipe) orelse {
        return null;
    };

    const parsed_type = try parseType(ctx) orelse {
        return ctx.err(.{
            .msg = "expected valid type",
        });
    };

    return parsed_type.type_info;
}

fn parseImport(ctx: *Context) !?Import {
    const token = ctx.getIf(.Import) orelse return null;
    const name_bareword = parseBareword(ctx) orelse {
        return ctx.err(.{
            .msg = "expected name of valid import as bareword",
        });
    };
    const maybe_alias = parseBareword(ctx);
    const maybe_newline = ctx.getIf(.Newline);

    if (maybe_newline == null) {
        if (maybe_alias == null) {
            return ctx.err(.{
                .msg = "expected newline (\\n) or alias of import",
            });
        } else {
            return ctx.err(.{
                .msg = "expected newline (\\n)",
            });
        }
    }

    const alias = if (maybe_alias != null) maybe_alias.?.token.value else null;

    return Import{
        .token = token,
        .name = name_bareword.token.value,
        .alias = alias,
    };
}

fn parseInitOp(ctx: *Context) !?InitOp {
    if (try parseVarDeclaration(ctx, .{ .needs_newline = false })) |decl| {
        return .{
            .declaration = decl,
        };
    } else if (try parseAssignment(ctx, .{ .needs_newline = false })) |assign| {
        return .{
            .assignment = assign,
        };
    }
    return null;
}

fn parsePostOp(ctx: *Context) !?PostOp {
    if (try parseAssignment(ctx, .{ .needs_newline = false })) |assign| {
        return .{
            .assignment = assign,
        };
    } else if (try parseCapturingCall(ctx)) |call| {
        return .{
            .call = call,
        };
    }
    return null;
}

fn parseLoop(ctx: *Context) !?Loop {
    const token = ctx.getIf(.For) orelse {
        return null;
    };

    const init_op = try parseInitOp(ctx);
    const left_semicolon = ctx.getIf(.Semicolon);

    if (left_semicolon == null and init_op != null) {
        return ctx.err(.{
            .msg = "expected ';' between initial operation and loop expression",
        });
    }

    const expr = try parseExpression(ctx);
    var post_op: ?PostOp = null;

    if (left_semicolon != null) {
        _ = ctx.getIf(.Semicolon) orelse {
            return ctx.err(.{
                .msg = "",
            });
        };

        post_op = try parsePostOp(ctx);
    }

    const scope = try parseScope(ctx, true) orelse {
        return ctx.err(.{
            .msg = "expected ';' between initial operation and loop expression",
        });
    };

    return .{
        .token = token,
        .init_op = init_op,
        .expr = expr,
        .post_op = post_op,
        .scope = scope,
    };
}

fn parseBreak(ctx: *Context) ?Break {
    const pair = ctx.getIfBoth(.Break, .Newline) orelse return null;
    return Break{
        .token = pair.first,
    };
}

fn parseContinue(ctx: *Context) ?Continue {
    const pair = ctx.getIfBoth(.Continue, .Newline) orelse return null;
    return Continue{
        .token = pair.first,
    };
}

fn parseReturn(ctx: *Context) !?Return {
    if (ctx.getIf(.Return) == null) {
        return null;
    }

    const maybe_expr = try parseExpression(ctx);

    if (ctx.getIf(.Newline) == null) {
        return ctx.err(.{
            .msg = "expected newline (\\n)",
        });
    }

    return Return{
        .expression = maybe_expr,
    };
}

fn parsePipeline(ctx: *Context) !?Call {
    const call = try parseIndividualPipeline(ctx, false) orelse {
        return null;
    };

    if (ctx.getIf(.Newline) == null and !ctx.eot()) {
        return ctx.err(.{
            .msg = "expected newline (\\n) or additional function arguments",
        });
    }

    return call;
}

fn parseIndividualPipeline(ctx: *Context, capturing: bool) !?Call {
    var pipeline_begin = try parseIndividualCall(ctx, capturing) orelse {
        return null;
    };

    var prev_node = &pipeline_begin;
    while (ctx.getIf(.Pipe) != null) {
        const next = try parseIndividualCall(ctx, capturing) orelse {
            return ctx.err(.{
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
        .msg = "expected closing ')'",
        .trailing = true,
    });
}

fn parseExpression(ctx: *Context) errors!?Expression {
    if (try parseCapturingCall(ctx)) |capturing_inv| {
        return Expression{
            .as = .{
                .capturing_call = capturing_inv,
            },
            .accessor = try parseAccessorChain(ctx),
        };
    } else if (parseBareword(ctx)) |bareword| {
        return Expression{
            .as = .{
                .bareword = bareword,
            },
            .accessor = try parseAccessorChain(ctx),
        };
    } else if (parseStringLiteral(ctx)) |string_literal| {
        return Expression{
            .as = .{
                .string_literal = string_literal,
            },
            .accessor = null,
        };
    } else if (try parseVariable(ctx)) |variable| {
        return Expression{
            .as = .{
                .variable = variable,
            },
            .accessor = try parseAccessorChain(ctx),
        };
    } else if (parseIntegerLiteral(ctx)) |integer_literal| {
        return Expression{
            .as = .{
                .integer_literal = integer_literal,
            },
            .accessor = null,
        };
    } else if (parseBoolLiteral(ctx)) |bool_literal| {
        return Expression{
            .as = .{
                .bool_literal = bool_literal,
            },
            .accessor = null,
        };
    } else if (try parseListLiteral(ctx)) |list| {
        return Expression{
            .as = .{
                .list_literal = list,
            },
            .accessor = null,
        };
    } else if (try parseRecordLiteral(ctx)) |record| {
        return Expression{
            .as = .{
                .record_literal = record,
            },
            .accessor = null,
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
            .msg = "expected one or more expresions, followed by a closing ']'",
            .trailing = true,
        });
    };

    return ListLiteral{
        .token = token,
        .items = try exprs.toOwnedSlice(),
    };
}

fn parseRecordLiteral(ctx: *Context) !?RecordLiteral {
    const token = ctx.getIf(.EmptyRecord) orelse return null;
    return RecordLiteral{
        .token = token,
        .items = &.{},
    };
}

fn parseVariable(ctx: *Context) !?Variable {
    const token = ctx.getIf(.Variable) orelse return null;

    return Variable{
        .token = token,
    };
}

fn parseAccessorChain(ctx: *Context) !?Accessor {
    var first: Accessor = .{
        .member = try parseMember(ctx) orelse {
            return null;
        },
        .child = null,
    };

    var ptr = &first;

    while (try parseMember(ctx)) |member| {
        const child = try ctx.ally.create(Accessor);
        child.member = member;

        ptr.child = child;
        ptr = child;
    }

    return first;
}

fn parseMember(ctx: *Context) !?Member {
    _ = ctx.getIf(.Colon) orelse {
        return null;
    };

    return parseBareword(ctx) orelse {
        return ctx.err(.{ .msg = "expected member name (bareword)" });
    };
}

pub fn tokenFromExpr(expr: Expression) *const Token {
    return switch (expr.as) {
        .bareword => |bw| bw.token,
        .bool_literal => |bl| bl.token,
        .capturing_call => |cc| cc.token,
        .integer_literal => |il| il.token,
        .list_literal => |li| li.token,
        .record_literal => |rl| rl.token,
        .string_literal => |sl| sl.token,
        .variable => |v| v.token,
    };
}
