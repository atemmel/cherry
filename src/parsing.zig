const std = @import("std");
const Token = @import("lexing.zig").Token;

pub const Bareword = struct {
    token: *const Token,
};

pub const StringLiteral = struct {
    token: *const Token,
};

pub const Expression = union {
    bareword: Bareword,
    invocation: Invocation,
    stringLiteral: StringLiteral,
};

pub const Invocation = struct {
    token: *const Token,
    arguments: []Expression,
};
