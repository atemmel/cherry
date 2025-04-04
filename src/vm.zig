const std = @import("std");
const print = std.debug.print;

pub const Op = enum {
    Constant,
    Return,
};

pub const Instructions = std.ArrayList(usize);

pub const Chunk = struct {
    instructions: Instructions,
    constants: Values,

    pub fn deinit(self: *Chunk) void {
        self.instructions.deinit();
        self.constants.deinit();
    }

    pub fn addInstruction(self: *Chunk, op: Op) !void {
        return try self.instructions.append(@intFromEnum(op));
    }

    pub fn addAdress(self: *Chunk, addr: usize) !void {
        return try self.instructions.append(addr);
    }

    pub fn addConstant(self: *Chunk, value: Value) !usize {
        try self.constants.append(value);
        return self.constants.items.len - 1;
    }
};

pub const Value = struct {
    as: union(enum) {
        integer: i64,
    },

    pub fn format(
        self: Value,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        switch (self.as) {
            .integer => |i| try writer.print("{}", .{i}),
        }
    }
};

pub const Values = std.ArrayList(Value);

pub fn disassembleChunk(chunk: *Chunk, name: []const u8) !void {
    print("== {s} ==\n", .{name});
    var offset: usize = 0;
    while (offset < chunk.instructions.items.len) {
        offset = try disassembleInstruction(chunk, offset);
    }
}

fn disassembleInstruction(chunk: *Chunk, offset: usize) !usize {
    print("{:0>8} ", .{offset});

    const inst = try std.meta.intToEnum(Op, chunk.instructions.items[offset]);

    return switch (inst) {
        .Return => simpleInstruction(inst, offset),
        .Constant => constantInstruction(chunk, inst, offset),
    };
}

fn simpleInstruction(op: Op, offset: usize) usize {
    print("{s}\n", .{@tagName(op)});
    return offset + 1;
}

fn constantInstruction(chunk: *Chunk, op: Op, offset: usize) usize {
    const constant = chunk.instructions.items[offset + 1];
    print("{s} {:0>8} {}\n", .{ @tagName(op), constant, chunk.constants.items[constant] });
    return offset + 2;
}

test "vm" {
    var chunk: Chunk = .{
        .instructions = Instructions.init(std.testing.allocator),
        .constants = Values.init(std.testing.allocator),
    };
    defer chunk.deinit();

    const constant = try chunk.addConstant(.{ .as = .{ .integer = 5 } });
    try chunk.addInstruction(.Constant);
    try chunk.addAdress(constant);
    try chunk.addInstruction(.Return);

    try disassembleChunk(&chunk, "example_chunk");
}
