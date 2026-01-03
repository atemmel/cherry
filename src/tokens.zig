const std = @import("std");
const ast = @import("ast.zig");
const pipeline = @import("pipeline.zig");
const PipelineState = pipeline.State;

pub const LexerError = error{
    UnterminatedStringLiteral,
    UnterminatedBlockComment,
} || std.mem.Allocator.Error;

pub const Token = struct {
    pub const Kind = enum {
        // Values/literals
        Bareword,
        StringLiteral,
        IntegerLiteral,
        Variable,
        EmptyRecord, // [=]
        // Significant whitespace
        Newline,
        // Symbols
        Bang,
        Colon,
        Semicolon,
        Assign, // =
        Pipe,
        LParens,
        RParens,
        LBrace,
        RBrace,
        LBracket,
        RBracket,
        Lesser, // <
        Greater, // >
        LesserEquals, // <=
        GreaterEquals, // >=
        RedirectOut, // |>
        RedirectIn, // <|
        SingleQuote, // '
        Equals, // ==
        NotEquals, // !=
        AddAssign, // +=
        SubAssign, // -=
        MulAssign, // *=
        DivAssign, // /=
        PipeAssign, // |=
        RightArrow, // ->
        // Keywords
        If,
        Else,
        For,
        Fn,
        In,
        Return,
        Break,
        Continue,
        True,
        False,
        Import,
        Pub,
    };

    pub fn isBinaryOperator(token: *const Token) bool {
        return switch (token.kind) {
            .Equals, .NotEquals, .Greater, .Lesser, .LesserEquals, .GreaterEquals => true,
            else => false,
        };
    }

    pub fn precedence(token: *const Token) i64 {
        return switch (token.kind) {
            .Equals, .NotEquals => 10,
            .Greater, .Lesser, .GreaterEquals, .LesserEquals => 5,
            else => unreachable, // this should never happen
        };
    }

    pub const Range = struct {
        from: usize,
        to: usize,
    };

    pub fn indicies(token: *const Token, src: []const u8) Range {
        const token_begin = @intFromPtr(token.value.ptr);
        const token_end = @intFromPtr(token.value.ptr) + token.value.len;
        const src_begin = @intFromPtr(src.ptr);
        return .{
            .from = token_begin - src_begin,
            .to = token_end - src_begin,
        };
    }

    pub fn indiciesAndPrefix(token: *const Token, src: []const u8) Range {
        const i = token.indicies(src);
        return .{
            .from = i.from - 1,
            .to = i.to,
        };
    }

    pub fn indiciesAndSurrounding(token: *const Token, src: []const u8) Range {
        const i = token.indicies(src);
        return .{
            .from = i.from - 1,
            .to = if (src.len == i.to) i.to else i.to + 1,
        };
    }

    kind: Kind,
    value: []const u8,
};

const string_keyword_map = std.StaticStringMap(Token.Kind).initComptime(&.{
    .{ "if", .If },
    .{ "else", .Else },
    .{ "for", .For },
    .{ "fn", .Fn },
    .{ "in", .In },
    .{ "return", .Return },
    .{ "break", .Break },
    .{ "continue", .Continue },
    .{ "true", .True },
    .{ "false", .False },
    .{ "import", .Import },
    .{ "pub", .Pub },
});

const LexState = struct {
    ally: std.mem.Allocator,
    source: []const u8,
    idx: usize = 0,
    list: std.ArrayList(Token),

    pub fn get(self: LexState) u8 {
        return self.source[self.idx];
    }

    pub fn eof(self: LexState) bool {
        return self.idx >= self.source.len;
    }

    pub fn next(self: *LexState) void {
        self.idx += 1;
    }

    pub fn isSymbolChar(self: LexState) bool {
        return switch (self.get()) {
            '!', ':', ';', '=', '|', '(', ')', '{', '}', '[', ']', '\'', '<', '>' => true,
            else => false,
        };
    }

    pub fn isNum(self: LexState) bool {
        return std.ascii.isDigit(self.get());
    }

    pub fn isUnallowedBarewordChar(self: LexState) bool {
        // chars that are never allowed to appear in the middle of a bareword
        return switch (self.get()) {
            '!', ':', ';', '|', '(', ')', '{', '}', '[', ']', ' ', '\n', '\t', '\r', '#', '"', '`', '\'', '<', '>' => true,
            else => false,
        };
    }

    pub fn lastWasNewline(self: *LexState) bool {
        return self.list.items[self.list.items.len - 1].kind == .Newline;
    }

    pub fn slice(self: LexState, from: usize, to: usize) []const u8 {
        return self.source[from..to];
    }
};

pub fn lex(state: *PipelineState, source: []const u8) LexerError![]Token {
    const ally = state.scratch_arena.allocator();
    var lstate = LexState{
        .source = source,
        .list = std.ArrayList(Token){},
        .ally = ally,
    };

    while (!lstate.eof()) : (lstate.next()) {
        try skipChars(&lstate);
        if (lstate.eof()) {
            break;
        }

        if (lexSymbol(&lstate)) |symbol| {
            try lstate.list.append(ally, symbol);
        } else if (lexVariable(&lstate)) |variable| {
            try lstate.list.append(ally, variable);
        } else if (lexKeyword(&lstate)) |keyword| {
            try lstate.list.append(ally, keyword);
        } else if (try lexStringLiteral(&lstate)) |string_literal| {
            try lstate.list.append(ally, string_literal);
        } else if (lexIntegerLiteral(&lstate)) |integer_literal| {
            try lstate.list.append(ally, integer_literal);
        } else if (lexBareword(&lstate)) |bareword| {
            try lstate.list.append(ally, bareword);
        }
    }

    return lstate.list.toOwnedSlice(ally);
}

fn lexSymbol(state: *LexState) ?Token {
    const any_maybe_part_of_symbol = "+-*/<";
    const char_maybe_part_of_symbol = std.mem.indexOfScalar(u8, any_maybe_part_of_symbol, state.get()) != null;

    if (!state.isSymbolChar() and !char_maybe_part_of_symbol) {
        return null;
    }

    const token_begin = state.idx;
    const kind: Token.Kind = switch (state.get()) {
        '=' => peekAssign(state, .Assign, .Equals),
        '!' => peekAssign(state, .Bang, .NotEquals),
        '+' => peekMustAssign(state, .AddAssign) orelse return null,
        '-' => peekMustAssign(state, .SubAssign) orelse peekMust(state, '>', .RightArrow) orelse return null,
        '*' => peekMustAssign(state, .MulAssign) orelse return null,
        '/' => peekMustAssign(state, .DivAssign) orelse return null,
        '|' => peekMustAssign(state, .PipeAssign) orelse peekMust(state, '>', .RedirectOut) orelse .Pipe,
        ':' => .Colon,
        ';' => .Semicolon,
        '<' => peekMustAssign(state, .LesserEquals) orelse peekMust(state, '|', .RedirectIn) orelse .Lesser,
        '>' => peekAssign(state, .Greater, .GreaterEquals),
        '(' => .LParens,
        ')' => .RParens,
        '{' => .LBrace,
        '}' => .RBrace,
        '[' => blk: {
            state.next();
            if (state.eof() or state.get() != '=') {
                state.idx -= 1;
                break :blk .LBracket;
            }

            state.next();
            if (state.eof() or state.get() != ']') {
                state.idx -= 2;
                break :blk .LBracket;
            }
            break :blk .EmptyRecord;
        },
        ']' => .RBracket,
        '\'' => .SingleQuote,
        else => unreachable,
    };

    return .{
        .kind = kind,
        .value = state.slice(token_begin, state.idx + 1),
    };
}

fn peekAssign(state: *LexState, regular: Token.Kind, with_assign: Token.Kind) Token.Kind {
    state.next();
    if (state.eof()) {
        state.idx -= 1;
        return regular;
    }
    return switch (state.get()) {
        '=' => with_assign,
        else => blk: {
            state.idx -= 1;
            break :blk regular;
        },
    };
}

fn peekMustAssign(state: *LexState, with_assign: Token.Kind) ?Token.Kind {
    return peekMust(state, '=', with_assign);
}

fn peekMust(state: *LexState, comptime must: u8, with_assign: Token.Kind) ?Token.Kind {
    state.next();
    if (state.eof()) {
        state.idx -= 1;
        return null;
    }
    return switch (state.get()) {
        must => with_assign,
        else => {
            state.idx -= 1;
            return null;
        },
    };
}

fn lexVariable(state: *LexState) ?Token {
    switch (state.get()) {
        '$' => state.next(),
        else => return null,
    }

    const begin = state.idx;
    while (!state.eof() and (std.ascii.isAlphabetic(state.get()) or state.get() == '-')) : (state.next()) {}
    const end = state.idx;
    state.idx -= 1;
    return .{
        .kind = .Variable,
        .value = state.slice(begin, end),
    };
}

fn lexKeyword(state: *LexState) ?Token {
    const begin = state.idx;
    while (!state.eof() and std.ascii.isAlphabetic(state.get())) : (state.next()) {}
    const end = state.idx;
    const keyword = state.slice(begin, end);
    if (begin == end) {
        return null;
    }
    if (string_keyword_map.get(keyword)) |kind| {
        state.idx -= 1;
        return Token{
            .kind = kind,
            .value = keyword,
        };
    } else {
        state.idx = begin;
        return null;
    }
}

fn lexStringLiteral(state: *LexState) !?Token {
    if (try lexSingleLineStringLiteral(state)) |lit| {
        return lit;
    } else if (try lexMultiLineStringLiteral(state)) |lit| {
        return lit;
    }
    return null;
}

fn lexSingleLineStringLiteral(state: *LexState) !?Token {
    if (state.get() != '"') {
        return null;
    }
    state.next();
    const begin = state.idx;
    while (!state.eof()) : (state.next()) {
        switch (state.get()) {
            '"', //TODO: handle escapes
            => break,
            else => {},
        }
    }

    if (state.eof()) {
        return LexerError.UnterminatedStringLiteral;
    }

    return .{
        .kind = .StringLiteral,
        .value = state.slice(begin, state.idx),
    };
}

fn lexMultiLineStringLiteral(state: *LexState) !?Token {
    if (state.get() != '`') {
        return null;
    }
    state.next();
    const begin = state.idx;
    while (!state.eof()) : (state.next()) {
        switch (state.get()) {
            '`', //TODO: handle escapes
            => break,
            else => {},
        }
    }

    if (state.eof()) {
        return LexerError.UnterminatedStringLiteral;
    }

    return .{
        .kind = .StringLiteral,
        .value = state.slice(begin, state.idx),
    };
}

fn lexIntegerLiteral(state: *LexState) ?Token {
    if (!state.isNum() and state.get() != '-') {
        return null;
    }
    const begin = state.idx;
    while (!state.eof() and !state.isUnallowedBarewordChar()) : (state.next()) {}
    const end = state.idx;

    const original_prospect = state.slice(begin, end);
    var int_prospect = original_prospect;

    if (int_prospect.len > 0 and int_prospect[0] == '-') {
        int_prospect = state.slice(begin + 1, end);
    }

    if (int_prospect.len == 0) {
        state.idx = begin;
        return null;
    }

    for (int_prospect) |c| {
        if (!std.ascii.isDigit(c)) {
            state.idx = begin;
            return null;
        }
    }

    state.idx -= 1;

    return .{
        .value = original_prospect,
        .kind = .IntegerLiteral,
    };
}

fn lexBareword(state: *LexState) ?Token {
    const begin = state.idx;
    while (!state.eof()) : (state.next()) {
        switch (state.get()) {
            // non-token
            ' ', '\n', '\t', '\r', '#', '"' => break,
            else => {},
        }
        if (state.isUnallowedBarewordChar()) {
            break;
        }
    }
    const end = state.idx;
    if (!state.eof()) {
        state.idx -= 1;
    }
    return if (end == begin)
        null
    else
        .{
            .kind = .Bareword,
            .value = state.slice(begin, end),
        };
}

fn skipChars(state: *LexState) LexerError!void {
    while (!state.eof()) : (state.next()) {
        switch (state.get()) {
            ' ', '\t' => {},
            '\n' => {
                if (state.list.items.len == 0 or state.lastWasNewline()) {
                    continue;
                }

                try state.list.append(state.ally, .{
                    .kind = .Newline,
                    .value = state.slice(state.idx, state.idx + 1),
                });
            },
            '#' => {
                state.next();
                if (!state.eof() and state.get() == '{') {
                    // seek end of block comment
                    try skipBlockComments(state);
                    continue;
                }
                while (!state.eof()) : (state.next()) {
                    switch (state.get()) {
                        '\n' => return,
                        else => {},
                    }
                }
                return;
            },
            else => return,
        }
    }
}

fn skipBlockComments(state: *LexState) !void {
    var depth: usize = 1;
    var prev = state.get();
    state.next();
    while (!state.eof()) : (state.next()) {
        defer prev = state.get();
        if (prev == '#' and state.get() == '}') {
            depth -= 1;
            if (depth == 0) {
                return;
            }
        } else if (prev == '#' and state.get() == '{') {
            depth += 1;
        }
    }
    return error.UnterminatedBlockComment;
}

pub fn dump(tokens: []const Token) void {
    const print = std.debug.print;
    for (tokens) |token| {
        print("{}", .{token.kind});
        if (token.value.len != 0) {
            print(", {s}", .{token.value});
        }
        print("\n", .{});
    }
}

const testState = @import("pipeline.zig").testState;
const expectEqual = std.testing.expectEqual;
const expectEqualStrings = std.testing.expectEqualStrings;

test "lex barewords" {
    testState();
    defer pipeline.deinit();

    const tokens = try lex(&pipeline.state, "ls -l -a");

    try expectEqual(3, tokens.len);
    try expectEqualStrings("ls", tokens[0].value);
    try expectEqual(Token.Kind.Bareword, tokens[0].kind);
    try expectEqualStrings("-l", tokens[1].value);
    try expectEqual(Token.Kind.Bareword, tokens[1].kind);
    try expectEqualStrings("-a", tokens[2].value);
    try expectEqual(Token.Kind.Bareword, tokens[2].kind);
}

test "lex string literals" {
    testState();
    defer pipeline.deinit();

    const tokens = try lex(&pipeline.state, "\"hi\" \"hello\"");

    try expectEqual(2, tokens.len);
    try expectEqualStrings("hi", tokens[0].value);
    try expectEqual(Token.Kind.StringLiteral, tokens[0].kind);
    try expectEqualStrings("hello", tokens[1].value);
    try expectEqual(Token.Kind.StringLiteral, tokens[1].kind);
}

test "lex var declaration" {
    testState();
    defer pipeline.deinit();

    const tokens = try lex(&pipeline.state, "x := \"hello\"");

    try expectEqual(4, tokens.len);
    try expectEqual(Token.Kind.Bareword, tokens[0].kind);
    try expectEqual(Token.Kind.Colon, tokens[1].kind);
    try expectEqual(Token.Kind.Assign, tokens[2].kind);
    try expectEqual(Token.Kind.StringLiteral, tokens[3].kind);
}

test "lex call with variable" {
    testState();
    defer pipeline.deinit();

    const tokens = try lex(&pipeline.state, "say $x");

    try expectEqual(2, tokens.len);
    try expectEqual(Token.Kind.Bareword, tokens[0].kind);
    try expectEqual(Token.Kind.Variable, tokens[1].kind);
    try expectEqualStrings("x", tokens[1].value);
}

test "lex equals" {
    testState();
    defer pipeline.deinit();

    const tokens = try lex(&pipeline.state, "== 1 2");

    try expectEqual(3, tokens.len);
    try expectEqual(.Equals, tokens[0].kind);
    try expectEqualStrings("==", tokens[0].value);
    try expectEqual(.IntegerLiteral, tokens[1].kind);
    try expectEqualStrings("1", tokens[1].value);
    try expectEqual(.IntegerLiteral, tokens[2].kind);
    try expectEqualStrings("2", tokens[2].value);
}

test "lex rpar rpar" {
    testState();
    defer pipeline.deinit();

    const tokens = try lex(&pipeline.state, "3))");

    try expectEqual(3, tokens.len);
    try expectEqual(.IntegerLiteral, tokens[0].kind);
    try expectEqual(.RParens, tokens[1].kind);
    try expectEqual(.RParens, tokens[2].kind);
}

test "lex assert equals rpar" {
    testState();
    defer pipeline.deinit();

    const tokens = try lex(&pipeline.state, "assert (== \"hello\" $x)");

    try expectEqual(6, tokens.len);
    try expectEqual(.Bareword, tokens[0].kind);
    try expectEqual(.LParens, tokens[1].kind);
    try expectEqual(.Equals, tokens[2].kind);
    try expectEqual(.StringLiteral, tokens[3].kind);
    try expectEqual(.Variable, tokens[4].kind);
    try expectEqual(.RParens, tokens[5].kind);
}

test "lex assert str str" {
    testState();
    defer pipeline.deinit();

    const tokens = try lex(&pipeline.state, "assert (== \"H\" \"H\")");

    try expectEqual(6, tokens.len);
    try expectEqual(.Bareword, tokens[0].kind);
    try expectEqual(.LParens, tokens[1].kind);
    try expectEqual(.Equals, tokens[2].kind);
    try expectEqual(.StringLiteral, tokens[3].kind);
    try expectEqual(.StringLiteral, tokens[4].kind);
    try expectEqual(.RParens, tokens[5].kind);
}

test "lex x y newline }" {
    testState();
    defer pipeline.deinit();

    const tokens = try lex(&pipeline.state, "\tassert true\n}");

    try expectEqual(4, tokens.len);
    try expectEqual(.Bareword, tokens[0].kind);
    try expectEqual(.True, tokens[1].kind);
    try expectEqual(.Newline, tokens[2].kind);
    try expectEqual(.RBrace, tokens[3].kind);
}

test "lex integer" {
    testState();
    defer pipeline.deinit();

    const tokens = try lex(&pipeline.state, "3 5");

    try expectEqual(2, tokens.len);
    try expectEqual(.IntegerLiteral, tokens[0].kind);
    try expectEqualStrings("3", tokens[0].value);
    try expectEqual(.IntegerLiteral, tokens[1].kind);
    try expectEqualStrings("5", tokens[1].value);
}

test "lex multiline string" {
    testState();
    defer pipeline.deinit();

    const tokens = try lex(&pipeline.state,
        \\say `
        \\
        \\guys`
    );

    try expectEqual(2, tokens.len);
    try expectEqual(.Bareword, tokens[0].kind);
    try expectEqualStrings("say", tokens[0].value);
    try expectEqual(.StringLiteral, tokens[1].kind);
    try expectEqualStrings("\n\nguys", tokens[1].value);
}

test "lex block comment" {
    testState();
    defer pipeline.deinit();

    const tokens = try lex(&pipeline.state,
        \\hello
        \\#{
        \\  comment
        \\#}
        \\bye
    );

    try expectEqual(3, tokens.len);
    try expectEqual(.Bareword, tokens[0].kind);
    try expectEqualStrings("hello", tokens[0].value);
    try expectEqual(.Newline, tokens[1].kind);
    try expectEqual(.Bareword, tokens[2].kind);
    try expectEqualStrings("bye", tokens[2].value);
}

test "lex nested block comment" {
    testState();
    defer pipeline.deinit();

    const tokens = try lex(&pipeline.state,
        \\hello
        \\#{
        \\comment
        \\#{
        \\nested comment
        \\#}
        \\comment
        \\#}
        \\bye
    );

    try expectEqual(3, tokens.len);
    try expectEqual(.Bareword, tokens[0].kind);
    try expectEqualStrings("hello", tokens[0].value);
    try expectEqual(.Newline, tokens[1].kind);
    try expectEqual(.Bareword, tokens[2].kind);
    try expectEqualStrings("bye", tokens[2].value);
}

test "lex negative number" {
    testState();
    defer pipeline.deinit();

    const tokens = try lex(&pipeline.state,
        \\say -1
    );

    try expectEqual(2, tokens.len);
    try expectEqual(.Bareword, tokens[0].kind);
    try expectEqualStrings("say", tokens[0].value);
    try expectEqual(.IntegerLiteral, tokens[1].kind);
    try expectEqualStrings("-1", tokens[1].value);
}

test "lex subtraction" {
    testState();
    defer pipeline.deinit();

    const tokens = try lex(&pipeline.state,
        \\(- 3 2)
    );

    try expectEqual(5, tokens.len);
    try expectEqual(.LParens, tokens[0].kind);
    try expectEqual(.Bareword, tokens[1].kind);
    try expectEqualStrings("-", tokens[1].value);
    try expectEqual(.IntegerLiteral, tokens[2].kind);
    try expectEqualStrings("3", tokens[2].value);
    try expectEqual(.IntegerLiteral, tokens[3].kind);
    try expectEqualStrings("2", tokens[3].value);
    try expectEqual(.RParens, tokens[4].kind);
}

test "lex double dot" {
    testState();
    defer pipeline.deinit();

    const tokens = try lex(&pipeline.state,
        \\..
    );

    try expectEqual(1, tokens.len);
    try expectEqual(.Bareword, tokens[0].kind);
    try expectEqualStrings("..", tokens[0].value);
}

test "lex redirect out" {
    testState();
    defer pipeline.deinit();

    const tokens = try lex(&pipeline.state,
        \\|>
    );

    try expectEqual(1, tokens.len);
    try expectEqual(.RedirectOut, tokens[0].kind);
}

test "lex pipe greater than" {
    testState();
    defer pipeline.deinit();

    const tokens = try lex(&pipeline.state,
        \\| >
    );

    try expectEqual(2, tokens.len);
    try expectEqual(.Pipe, tokens[0].kind);
    try expectEqual(.Greater, tokens[1].kind);
}

test "lex redirect in" {
    testState();
    defer pipeline.deinit();

    const tokens = try lex(&pipeline.state,
        \\<|
    );

    try expectEqual(1, tokens.len);
    try expectEqual(.RedirectIn, tokens[0].kind);
}

test "lex less than pipe" {
    testState();
    defer pipeline.deinit();

    const tokens = try lex(&pipeline.state,
        \\< |
    );

    try expectEqual(2, tokens.len);
    try expectEqual(.Lesser, tokens[0].kind);
    try expectEqual(.Pipe, tokens[1].kind);
}

test "lex equals combinations" {
    testState();
    defer pipeline.deinit();

    const tokens = try lex(&pipeline.state,
        \\-= += /= *=
    );

    try expectEqual(4, tokens.len);
    try expectEqual(.SubAssign, tokens[0].kind);
    try expectEqual(.AddAssign, tokens[1].kind);
    try expectEqual(.DivAssign, tokens[2].kind);
    try expectEqual(.MulAssign, tokens[3].kind);
}

test "lex arrow" {
    testState();
    defer pipeline.deinit();

    const tokens = try lex(&pipeline.state,
        \\->
    );

    try expectEqual(1, tokens.len);
    try expectEqual(.RightArrow, tokens[0].kind);
}

test "lex record and newline" {
    testState();
    defer pipeline.deinit();

    const tokens = try lex(&pipeline.state,
        \\[=]
        \\[=]
    );

    try expectEqual(3, tokens.len);
    try expectEqual(.EmptyRecord, tokens[0].kind);
    try expectEqual(.Newline, tokens[1].kind);
    try expectEqual(.EmptyRecord, tokens[2].kind);
}
