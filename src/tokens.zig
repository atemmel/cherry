const std = @import("std");
const ast = @import("ast.zig");
const PipelineState = @import("pipeline.zig").State;

pub const LexerError = error{
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
        Colon,
        Semicolon,
        Assign,
        Pipe,
        LParens,
        RParens,
        LBrace,
        RBrace,
        LBracket,
        RBracket,
        RedirectOut, // |>
        RedirectIn, // <|
        // Keywords
        If,
        Else,
        For,
        Fn,
        Return,
        True,
        False,
    };

    kind: Kind,
    value: []const u8,
};

const string_keyword_map = std.StaticStringMap(Token.Kind).initComptime(&.{
    .{ "if", .If },
    .{ "else", .Else },
    .{ "for", .For },
    .{ "fn", .Fn },
    .{ "return", .Return },
    .{ "true", .True },
    .{ "false", .False },
});

const LexState = struct {
    state: *PipelineState,
    idx: usize = 0,
    list: std.ArrayList(Token),

    pub fn get(self: LexState) u8 {
        return self.state.source[self.idx];
    }

    pub fn eof(self: LexState) bool {
        return self.idx >= self.state.source.len;
    }

    pub fn next(self: *LexState) void {
        self.idx += 1;
    }

    pub fn isSymbolChar(self: LexState) bool {
        return switch (self.get()) {
            ':', ';', '=', '|', '(', ')', '{', '}', '[', ']' => true,
            else => false,
        };
    }

    pub fn isNum(self: LexState) bool {
        return std.ascii.isDigit(self.get());
    }

    pub fn isUnallowedBarewordChar(self: LexState) bool {
        // chars that are never allowed to appear in the middle of a bareword
        return switch (self.get()) {
            ':', ';', '|', '(', ')', '{', '}', '[', ']', ' ', '\n', '\t', '\r', '#', '"', '`' => true,
            else => false,
        };
    }

    pub fn lastWasNewline(self: *LexState) bool {
        return self.list.items[self.list.items.len - 1].kind == .Newline;
    }

    pub fn slice(self: LexState, from: usize, to: usize) []const u8 {
        return self.state.source[from..to];
    }
};

pub fn lex(state: *PipelineState) LexerError![]Token {
    var lstate = LexState{
        .state = state,
        .list = std.ArrayList(Token).init(state.arena),
    };

    while (!lstate.eof()) : (lstate.next()) {
        try skipChars(&lstate);
        if (lstate.eof()) {
            break;
        }

        if (lexSymbol(&lstate)) |symbol| {
            try lstate.list.append(symbol);
        } else if (lexVariable(&lstate)) |variable| {
            try lstate.list.append(variable);
        } else if (lexKeyword(&lstate)) |keyword| {
            try lstate.list.append(keyword);
        } else if (lexStringLiteral(&lstate)) |string_literal| {
            try lstate.list.append(string_literal);
        } else if (lexIntegerLiteral(&lstate)) |string_literal| {
            try lstate.list.append(string_literal);
        } else if (lexBareword(&lstate)) |bareword| {
            try lstate.list.append(bareword);
        }
    }

    return lstate.list.toOwnedSlice();
}

fn lexSymbol(state: *LexState) ?Token {
    if (!state.isSymbolChar()) {
        return null;
    }

    const token_begin = state.idx;
    const kind: Token.Kind = switch (state.get()) {
        '=' => blk: {
            state.next();
            if (state.eof()) {
                state.idx -= 1;
                break :blk .Assign;
            }
            break :blk switch (state.get()) {
                '=' => {
                    state.idx -= 1;
                    return null;
                },
                else => {
                    state.idx -= 1;
                    break :blk .Assign;
                },
            };
        },
        ':' => .Colon,
        ';' => .Semicolon,
        '|' => .Pipe,
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
            state.next();
            break :blk .EmptyRecord;
        },
        ']' => .RBracket,
        else => unreachable,
    };

    return .{
        .kind = kind,
        .value = state.slice(token_begin, state.idx),
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
    if (begin == end) unreachable;
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

fn lexStringLiteral(state: *LexState) ?Token {
    if (lexSingleLineStringLiteral(state)) |lit| {
        return lit;
    } else if (lexMultiLineStringLiteral(state)) |lit| {
        return lit;
    }
    return null;
}

fn lexSingleLineStringLiteral(state: *LexState) ?Token {
    if (state.get() != '"') {
        return null;
    }
    state.next();
    const begin = state.idx;
    while (!state.eof()) : (state.next()) {
        switch (state.get()) {
            // TODO: handle error
            '\n',
            => unreachable,
            '"', //TODO: handle escapes
            => break,
            else => {},
        }
    }
    return .{
        .kind = .StringLiteral,
        .value = state.slice(begin, state.idx),
    };
}

fn lexMultiLineStringLiteral(state: *LexState) ?Token {
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
    return .{
        .kind = .StringLiteral,
        .value = state.slice(begin, state.idx),
    };
}

fn lexIntegerLiteral(state: *LexState) ?Token {
    if (!state.isNum()) {
        return null;
    }
    const begin = state.idx;
    while (!state.eof() and !state.isUnallowedBarewordChar()) : (state.next()) {}
    const end = state.idx;

    const int_prospect = state.slice(begin, end);

    for (int_prospect) |c| {
        if (!std.ascii.isDigit(c)) {
            std.debug.print("{s}\n", .{int_prospect});
            if (state.isSymbolChar()) {
                break;
            }
            //TODO: bad token
            unreachable;
        }
    }

    state.idx -= 1;

    return .{
        .value = int_prospect,
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

                try state.list.append(.{
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

pub fn dump(state: *PipelineState) void {
    const print = std.debug.print;
    for (state.tokens) |token| {
        print("{}", .{token.kind});
        if (token.value.len != 0) {
            print(", {s}", .{token.value});
        }
        print("\n", .{});
    }
}

const expectEqual = std.testing.expectEqual;
const expectEqualStrings = std.testing.expectEqualStrings;

fn testState(source: []const u8) PipelineState {
    return PipelineState{
        .ally = std.testing.allocator,
        .arena = std.testing.allocator,
        .source = source,
        .filename = "",
        .arena_source = undefined,
        .verboseLexer = false,
        .verboseParser = false,
        .verboseAnalysis = false,
        .verboseInterpretation = false,
        .useSemanticAnalysis = false,
    };
}

test "lex barewords" {
    const ally = std.testing.allocator;

    var state = testState("ls -l -a");

    const tokens = try lex(&state);
    defer ally.free(tokens);

    try expectEqual(3, tokens.len);
    try expectEqualStrings("ls", tokens[0].value);
    try expectEqual(Token.Kind.Bareword, tokens[0].kind);
    try expectEqualStrings("-l", tokens[1].value);
    try expectEqual(Token.Kind.Bareword, tokens[1].kind);
    try expectEqualStrings("-a", tokens[2].value);
    try expectEqual(Token.Kind.Bareword, tokens[2].kind);
}

test "lex string literals" {
    const ally = std.testing.allocator;

    var state = testState("\"hi\" \"hello\"");

    const tokens = try lex(&state);
    defer ally.free(tokens);

    try expectEqual(2, tokens.len);
    try expectEqualStrings("hi", tokens[0].value);
    try expectEqual(Token.Kind.StringLiteral, tokens[0].kind);
    try expectEqualStrings("hello", tokens[1].value);
    try expectEqual(Token.Kind.StringLiteral, tokens[1].kind);
}

test "lex var declaration" {
    const ally = std.testing.allocator;

    var state = testState("x := \"hello\"");

    const tokens = try lex(&state);
    defer ally.free(tokens);

    try expectEqual(4, tokens.len);
    try expectEqual(Token.Kind.Bareword, tokens[0].kind);
    try expectEqual(Token.Kind.Colon, tokens[1].kind);
    try expectEqual(Token.Kind.Assign, tokens[2].kind);
    try expectEqual(Token.Kind.StringLiteral, tokens[3].kind);
}

test "lex call with variable" {
    const ally = std.testing.allocator;

    var state = testState("say $x");

    const tokens = try lex(&state);
    defer ally.free(tokens);

    try expectEqual(2, tokens.len);
    try expectEqual(Token.Kind.Bareword, tokens[0].kind);
    try expectEqual(Token.Kind.Variable, tokens[1].kind);
    try expectEqualStrings("x", tokens[1].value);
}

test "lex equals" {
    const ally = std.testing.allocator;

    var state = testState("== 1 2");

    const tokens = try lex(&state);
    defer ally.free(tokens);

    try expectEqual(3, tokens.len);
    try expectEqual(.Bareword, tokens[0].kind);
    try expectEqualStrings("==", tokens[0].value);
    try expectEqual(.IntegerLiteral, tokens[1].kind);
    try expectEqualStrings("1", tokens[1].value);
    try expectEqual(.IntegerLiteral, tokens[2].kind);
    try expectEqualStrings("2", tokens[2].value);
}

test "lex rpar rpar" {
    const ally = std.testing.allocator;

    var state = testState("3))");

    const tokens = try lex(&state);
    defer ally.free(tokens);

    try expectEqual(3, tokens.len);
    try expectEqual(.IntegerLiteral, tokens[0].kind);
    try expectEqual(.RParens, tokens[1].kind);
    try expectEqual(.RParens, tokens[2].kind);
}

test "lex assert equals rpar" {
    const ally = std.testing.allocator;
    var state = testState("assert (== \"hello\" $x)");

    const tokens = try lex(&state);
    defer ally.free(tokens);

    try expectEqual(6, tokens.len);
    try expectEqual(.Bareword, tokens[0].kind);
    try expectEqual(.LParens, tokens[1].kind);
    try expectEqual(.Bareword, tokens[2].kind);
    try expectEqual(.StringLiteral, tokens[3].kind);
    try expectEqual(.Variable, tokens[4].kind);
    try expectEqual(.RParens, tokens[5].kind);
}

test "lex assert str str" {
    const ally = std.testing.allocator;

    var state = testState("assert (== \"H\" \"H\")");

    const tokens = try lex(&state);
    defer ally.free(tokens);

    try expectEqual(6, tokens.len);
    try expectEqual(.Bareword, tokens[0].kind);
    try expectEqual(.LParens, tokens[1].kind);
    try expectEqual(.Bareword, tokens[2].kind);
    try expectEqual(.StringLiteral, tokens[3].kind);
    try expectEqual(.StringLiteral, tokens[4].kind);
    try expectEqual(.RParens, tokens[5].kind);
}

test "lex x y newline }" {
    const ally = std.testing.allocator;

    var state = testState("\tassert true\n}");

    const tokens = try lex(&state);
    defer ally.free(tokens);

    try expectEqual(4, tokens.len);
    try expectEqual(.Bareword, tokens[0].kind);
    try expectEqual(.True, tokens[1].kind);
    try expectEqual(.Newline, tokens[2].kind);
    try expectEqual(.RBrace, tokens[3].kind);
}

test "lex integer" {
    const ally = std.testing.allocator;

    var state = testState("3 5");

    const tokens = try lex(&state);
    defer ally.free(tokens);

    try expectEqual(2, tokens.len);
    try expectEqual(.IntegerLiteral, tokens[0].kind);
    try expectEqualStrings("3", tokens[0].value);
    try expectEqual(.IntegerLiteral, tokens[1].kind);
    try expectEqualStrings("5", tokens[1].value);
}

test "lex multiline string" {
    const ally = std.testing.allocator;

    var state = testState(
        \\say `
        \\
        \\guys`
    );

    const tokens = try lex(&state);
    defer ally.free(tokens);

    try expectEqual(2, tokens.len);
    try expectEqual(.Bareword, tokens[0].kind);
    try expectEqualStrings("say", tokens[0].value);
    try expectEqual(.StringLiteral, tokens[1].kind);
    try expectEqualStrings("\n\nguys", tokens[1].value);
}

test "lex block comment" {
    const ally = std.testing.allocator_instance.allocator();

    var state = testState(
        \\hello
        \\#{
        \\  comment
        \\#}
        \\bye
    );

    const tokens = try lex(&state);
    defer ally.free(tokens);

    try expectEqual(3, tokens.len);
    try expectEqual(.Bareword, tokens[0].kind);
    try expectEqualStrings("hello", tokens[0].value);
    try expectEqual(.Newline, tokens[1].kind);
    try expectEqual(.Bareword, tokens[2].kind);
    try expectEqualStrings("bye", tokens[2].value);
}
test "lex nested block comment" {
    const ally = std.testing.allocator_instance.allocator();

    var state = testState(
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

    const tokens = try lex(&state);
    defer ally.free(tokens);

    try expectEqual(3, tokens.len);
    try expectEqual(.Bareword, tokens[0].kind);
    try expectEqualStrings("hello", tokens[0].value);
    try expectEqual(.Newline, tokens[1].kind);
    try expectEqual(.Bareword, tokens[2].kind);
    try expectEqualStrings("bye", tokens[2].value);
}
