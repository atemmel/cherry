const std = @import("std");

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

pub fn lex(src: []const u8, ally: std.mem.Allocator) ![]Token {
    var idx: usize = 0;
    var list = std.ArrayList(Token).init(ally);

    for (src, 0..src.len) |c, i| {
        if (c == ' ' or c == '\n') {
            try list.append(.{
                .kind = .Bareword,
                .value = src[idx..i],
            });
            idx = i + 1;
        }
    }

    if (idx != src.len) {
        try list.append(.{
            .kind = .Bareword,
            .value = src[idx..src.len],
        });
    }

    return list.toOwnedSlice();
}

const expectEqual = std.testing.expectEqual;
const expectEqualStrings = std.testing.expectEqualStrings;

test "lex barewords" {
    const src = "ls -l -a";
    const ally = std.testing.allocator_instance.allocator();

    const tokens = try lex(src, ally);
    defer ally.free(tokens);

    try expectEqual(3, tokens.len);
    try expectEqualStrings("ls", tokens[0].value);
    try expectEqual(Token.Kind.Bareword, tokens[0].kind);
    try expectEqualStrings("-l", tokens[1].value);
    try expectEqual(Token.Kind.Bareword, tokens[1].kind);
    try expectEqualStrings("-a", tokens[2].value);
    try expectEqual(Token.Kind.Bareword, tokens[2].kind);
}
