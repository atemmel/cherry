const std = @import("std");
const print = std.debug.print;

pub const Op = union(enum) {
    Constant: usize,
    Return,
};

pub const Instructions = std.ArrayList(Op);

pub const Chunk = struct {
    instructions: Instructions,
    constants: Values,

    pub fn deinit(self: *Chunk) void {
        self.instructions.deinit();
        self.constants.deinit();
    }

    pub fn addInstruction(self: *Chunk, op: Op) !void {
        return try self.instructions.append(op);
    }

    pub fn addConstant(self: *Chunk, value: Value) !usize {
        try self.constants.append(value);
        return self.constants.items.len - 1;
    }
};

pub const Value = struct {
    as: union {
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

pub fn disassembleChunk(chunk: *Chunk, name: []const u8) void {
    print("== {s} ==\n", .{name});
    var offset: usize = 0;
    while (offset < chunk.instructions.items.len) {
        offset = disassembleInstruction(chunk, offset);
    }
}

fn disassembleInstruction(chunk: *Chunk, offset: usize) usize {
    print("{:0>4} ", .{offset});

    const inst = chunk.instructions.items[offset];

    return switch (inst) {
        .Return => simpleInstruction(inst, offset),
        .Constant => constantInstruction(inst, offset),
    };
}

fn simpleInstruction(op: Op, offset: usize) usize {
    print("{s}\n", .{@tagName(op)});
    return offset + 1;
}

fn constantInstruction(op: Op, offset: usize) usize {
    _ = op; // autofix
    _ = offset; // autofix
    return 0;
}

test "vm" {
    var chunk: Chunk = .{
        .instructions = Instructions.init(std.testing.allocator),
        .constants = Values.init(std.testing.allocator),
    };
    defer chunk.deinit();

    //const constant = try chunk.addConstant(.{ .as = .{ .integer = 5 } });
    //try chunk.addInstruction(Op{ .Constant = constant });
    try chunk.addInstruction(.Return);

    disassembleChunk(&chunk, "example_chunk");
}
