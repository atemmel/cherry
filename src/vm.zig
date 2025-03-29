const std = @import("std");
const print = std.debug.print;

pub const Op = enum {
    Constant,
    Return,
};

pub const Chunk = std.ArrayList(Op);

pub const Value = struct {};

pub const Values = std.ArrayList(Value);

pub fn disassembleChunk(chunk: *Chunk, name: []const u8) void {
    print("== {s} ==\n", .{name});
    var offset: usize = 0;
    while (offset < chunk.items.len) {
        offset = disassembleInstruction(chunk, offset);
    }
}

fn disassembleInstruction(chunk: *Chunk, offset: usize) usize {
    print("{:0>4} ", .{offset});

    const inst = chunk.items[offset];

    return switch (inst) {
        .Return => simpleInstruction(inst, offset),
    };
}

fn simpleInstruction(op: Op, offset: usize) usize {
    print("{s}\n", .{@tagName(op)});
    return offset + 1;
}

test "vm" {
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    try chunk.append(.Return);
    try chunk.append(.Return);

    disassembleChunk(&chunk, "example_chunk");
}
