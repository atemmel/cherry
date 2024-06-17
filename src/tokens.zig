const std = @import("std");
const ast = @import("ast.zig");
const PipelineState = @import("pipeline.zig").State;

pub const Token = struct {
    pub const Kind = enum {
        Bareword,
        StringLiteral,
        Newline,
        // Operators
        Assign,
        Equals,
        Pipe,
        // Keywords
        If,
        Else,
        For,
    };

    kind: Kind,
    value: []const u8,
};

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
};

pub fn lex(state: *PipelineState) ![]Token {
    var lstate = LexState{
        .state = state,
        .list = std.ArrayList(Token).init(state.arena),
    };

    var idx: usize = 0;
    while (!lstate.eof()) : (lstate.next()) {
        skipChars(&lstate);
        if (lstate.eof()) {
            break;
        }
        const c = lstate.get();
        if (c == ' ' or c == '\n') {
            try lstate.list.append(.{
                .kind = .Bareword,
                .value = state.source[idx..lstate.idx],
            });
            idx = lstate.idx + 1;
        }
    }

    if (idx != state.source.len) {
        try lstate.list.append(.{
            .kind = .Bareword,
            .value = state.source[idx..state.source.len],
        });
    }

    return lstate.list.toOwnedSlice();
}

fn skipChars(state: *LexState) void {
    while (!state.eof()) : (state.next()) {
        switch (state.get()) {
            ' ', '\n', '\t' => {},
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
            print(", {s}\n", .{token.value});
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
        .verboseLexer = false,
        .verboseParser = false,
        .verboseCodegen = false,
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
