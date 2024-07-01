const std = @import("std");
const ast = @import("ast.zig");
const PipelineState = @import("pipeline.zig").State;

pub const Token = struct {
    pub const Kind = enum {
        // Values/literals
        Bareword,
        StringLiteral,
        Variable,
        // Significant whitespace
        Newline,
        // Symbols
        Assign,
        Equals,
        Pipe,
        LParens,
        RParens,
        LBrace,
        RBrace,
        LBracket,
        RBracket,
        // Keywords
        Var,
        If,
        Else,
        While,
        For,
    };

    kind: Kind,
    value: []const u8,
};

const string_keyword_map = std.StaticStringMap(Token.Kind).initComptime(&.{
    .{ "var", .Var },
    .{ "if", .If },
    .{ "else", .Else },
    .{ "while", .While },
    .{ "for", .For },
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

    pub fn lastWasNewline(self: *LexState) bool {
        return self.list.items[self.list.items.len - 1].kind == .Newline;
    }

    pub fn slice(self: LexState, from: usize, to: usize) []const u8 {
        return self.state.source[from..to];
    }
};

pub fn lex(state: *PipelineState) ![]Token {
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
        } else if (lexBareword(&lstate)) |bareword| {
            try lstate.list.append(bareword);
        }
    }

    return lstate.list.toOwnedSlice();
}

fn lexSymbol(state: *LexState) ?Token {
    switch (state.get()) {
        // ok symbols
        '=', '|', '(', ')', '{', '}', '[', ']' => {},
        // not a symbol
        else => return null,
    }

    defer state.next();

    const token_begin = state.idx;
    const kind: Token.Kind = switch (state.get()) {
        '=' => blk: {
            state.next();
            if (state.eof()) {
                break :blk .Assign;
            }
            break :blk switch (state.get()) {
                '=' => .Equals,
                else => {
                    state.idx -= 1;
                    break :blk .Assign;
                },
            };
        },
        '|' => .Pipe,
        '(' => .LParens,
        ')' => .RParens,
        '{' => .LBrace,
        '}' => .RBrace,
        '[' => .LBracket,
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
    while (!state.eof() and std.ascii.isAlphabetic(state.get())) : (state.next()) {}
    const end = state.idx;
    if (begin == end) unreachable;
    return .{
        .kind = .Variable,
        .value = state.slice(begin, end),
    };
}

fn lexKeyword(state: *LexState) ?Token {
    const begin = state.idx;
    while (!state.eof() and std.ascii.isAlphabetic(state.get())) : (state.next()) {}
    const end = state.idx;
    return if (begin == end)
        null
    else if (string_keyword_map.get(state.slice(begin, end))) |kind| .{
        .kind = kind,
        .value = "",
    } else {
        state.idx = begin;
        return null;
    };
}

fn lexStringLiteral(state: *LexState) ?Token {
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
            '"',
            => break,
            else => {},
        }
    }
    defer state.next();
    return .{
        .kind = .StringLiteral,
        .value = state.slice(begin, state.idx),
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
    }
    return if (state.idx == begin)
        null
    else
        .{
            .kind = .Bareword,
            .value = state.slice(begin, state.idx),
        };
}

fn skipChars(state: *LexState) !void {
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

test "lex barewords" {
    const ally = std.testing.allocator_instance.allocator();

    var state: PipelineState = .{
        .ally = ally,
        .arena = ally,
        .source = "ls -l -a",
    };

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
    const ally = std.testing.allocator_instance.allocator();

    var state: PipelineState = .{
        .ally = ally,
        .arena = ally,
        .source = "\"hi\" \"hello\"",
    };

    const tokens = try lex(&state);
    defer ally.free(tokens);

    try expectEqual(2, tokens.len);
    try expectEqualStrings("hi", tokens[0].value);
    try expectEqual(Token.Kind.StringLiteral, tokens[0].kind);
    try expectEqualStrings("hello", tokens[1].value);
    try expectEqual(Token.Kind.StringLiteral, tokens[1].kind);
}

test "lex var declaration" {
    const ally = std.testing.allocator_instance.allocator();

    var state: PipelineState = .{
        .ally = ally,
        .arena = ally,
        .source = "var x = \"hello\"",
    };

    const tokens = try lex(&state);
    defer ally.free(tokens);

    try expectEqual(4, tokens.len);
    try expectEqual(Token.Kind.Var, tokens[0].kind);
    try expectEqual(Token.Kind.Bareword, tokens[1].kind);
    try expectEqual(Token.Kind.Assign, tokens[2].kind);
    try expectEqual(Token.Kind.StringLiteral, tokens[3].kind);
}

test "lex call with variable" {
    const ally = std.testing.allocator_instance.allocator();

    var state: PipelineState = .{
        .ally = ally,
        .arena = ally,
        .source = "say $x",
    };

    const tokens = try lex(&state);
    defer ally.free(tokens);

    try expectEqual(2, tokens.len);
    try expectEqual(Token.Kind.Bareword, tokens[0].kind);
    try expectEqual(Token.Kind.Variable, tokens[1].kind);
    try expectEqualStrings("x", tokens[1].value);
}
