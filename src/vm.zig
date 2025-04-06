const std = @import("std");
const print = std.debug.print;

pub const Op = enum(usize) {
    False,
    True,
    Constant,
    Add,
    Mul,
    Sub,
    Div,
    Negate,
    And,
    Or,
    Equals,
    NotEquals,
    Not,
    Return,
};

pub const ValueKind = enum {
    integer,
    boolean,
    string,
};

pub const Value = union(ValueKind) {
    integer: i64,
    boolean: bool,
    string: *HeapNode,

    pub fn format(
        self: Value,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        switch (self) {
            .integer => |i| try writer.print("{}", .{i}),
            .boolean => |b| try writer.print("{s}", .{if (b) "true" else "false"}),
            .string => |s| try writer.print("{s}", .{s.data.string}),
        }
    }

    pub fn isString(self: Value) bool {
        return self.kind() == .string;
    }

    pub fn kindName(self: Value) [:0]const u8 {
        return switch (self) {
            .integer => "int",
            .string => "string",
            .record => "record",
            .float => "float",
            .boolean => "bool",
            .list => "list",
            .closure => "closure",
        };
    }

    pub fn kind(self: Value) ValueKind {
        return std.meta.activeTag(self);
    }

    const Order = union(enum) {
        less,
        greater,
        equal,
        failure: struct {
            wants: ValueKind,
            got: ValueKind,
        },
    };

    pub fn compare(self: Value, other: Value) Order {
        return switch (self) {
            .integer => |lhs| switch (other) {
                .integer => |rhs| blk: {
                    if (lhs < rhs) {
                        break :blk .less;
                    } else if (lhs > rhs) {
                        break :blk .greater;
                    }
                    break :blk .equal;
                },
                else => typeMismatch(.integer, other.kind()),
            },
            .boolean => |lhs| switch (other) {
                .boolean => |rhs| blk: {
                    if (lhs and rhs) {
                        break :blk .equal;
                    } else if (!lhs and rhs) {
                        break :blk .less;
                    } else if (lhs and !rhs) {
                        break :blk .greater;
                    }
                    break :blk .equal;
                },
                else => typeMismatch(.boolean, other.kind()),
            },
            .string => |addr| blk: {
                if (!other.isString()) {
                    break :blk typeMismatch(.string, other.kind());
                }
                const lhs = addr.data.string;
                const rhs = other.string.data.string;
                const order = std.mem.order(u8, lhs, rhs);
                break :blk switch (order) {
                    .lt => .less,
                    .gt => .greater,
                    .eq => .equal,
                };
            },
        };
    }

    fn typeMismatch(lhs_type: ValueKind, rhs_type: ValueKind) Value.Order {
        return Value.Order{
            .failure = .{
                .wants = lhs_type,
                .got = rhs_type,
            },
        };
    }
};

pub const HeapNode = struct {
    pub const Kind = enum {
        string,
    };

    data: union(Kind) {
        string: []const u8,
    },
    next: ?*HeapNode,

    pub fn deinit(self: *HeapNode, ally: std.mem.Allocator) void {
        switch (self.data) {
            .string => |str| ally.free(str),
        }
    }

    pub fn kind(self: HeapNode) Kind {
        return std.meta.activeTag(self);
    }
};

pub const Values = std.ArrayList(Value);
pub const Instructions = std.ArrayList(usize); //TODO: should perhaps be made more compact

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

    pub fn disassemble(chunk: *Chunk, name: []const u8) !void {
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
            .False,
            .True,
            .Return,
            .Negate,
            .Add,
            .Mul,
            .Div,
            .Sub,
            .And,
            .Or,
            .Equals,
            .NotEquals,
            .Not,
            => simpleInstruction(inst, offset),
            .Constant => constantInstruction(chunk, inst, offset),
        };
    }

    fn simpleInstruction(op: Op, offset: usize) usize {
        print("{s}\n", .{@tagName(op)});
        return offset + 1;
    }

    fn constantInstruction(chunk: *Chunk, op: Op, offset: usize) usize {
        const constant = chunk.instructions.items[offset + 1];
        print("{s} c{:0>8} {}\n", .{ @tagName(op), constant, chunk.constants.items[constant] });
        return offset + 2;
    }
};

pub const VM = struct {
    chunk: *Chunk,
    ip: usize,
    stack: Values,
    heap: ?*HeapNode,
    runtime_ally: std.mem.Allocator,

    pub const Errors = error{
        UnexpectedTypeOfValue,
    };

    pub fn init(arena: std.mem.Allocator, ally: std.mem.Allocator) !VM {
        return VM{
            .chunk = undefined,
            .ip = undefined,
            .stack = try Values.initCapacity(arena, 4096),
            .heap = null,
            .runtime_ally = ally,
        };
    }

    pub fn deinit(self: *VM) void {
        self.stack.deinit();
        while (self.heap) |node| {
            self.heap = node.next;
            node.deinit(self.runtime_ally);
            self.runtime_ally.destroy(node);
        }
    }

    pub fn allocateString(self: *VM, str: []const u8) !Value {
        const duped_str = try self.runtime_ally.dupe(u8, str);
        const node = try self.runtime_ally.create(HeapNode);
        node.* = HeapNode{
            .data = .{
                .string = duped_str,
            },
            .next = null,
        };
        if (self.heap) |heap| {
            node.next = heap;
        }
        self.heap = node;
        return Value{
            .string = node,
        };
    }

    fn eoi(self: *VM) bool {
        return self.ip >= self.chunk.instructions.items.len;
    }

    fn readInstruction(self: *VM) Op {
        return std.meta.intToEnum(Op, self.readAddr()) catch unreachable;
    }

    fn readAddr(self: *VM) usize {
        defer self.ip += 1;
        return self.chunk.instructions.items[self.ip];
    }

    fn readConstant(self: *VM) Value {
        const addr = self.readAddr();
        return self.chunk.constants.items[addr];
    }

    pub fn push(self: *VM, value: Value) void {
        self.stack.append(value) catch unreachable;
    }

    pub fn pop(self: *VM) Value {
        return self.stack.pop().?;
    }

    pub fn peek(self: *VM) Value {
        return self.stack.getLast();
    }

    pub fn popField(self: *VM, comptime tag: ValueKind) Errors!std.meta.TagPayload(Value, tag) {
        if (std.meta.activeTag(self.peek()) != tag) {
            return Errors.UnexpectedTypeOfValue;
        }
        return @field(self.stack.pop().?, @tagName(tag));
    }
};

pub fn interpret(vm: *VM, chunk: *Chunk) VM.Errors!void {
    vm.chunk = chunk;
    vm.ip = 0;

    while (true) { //TODO: not like this
        switch (vm.readInstruction()) {
            .False => vm.push(.{ .boolean = false }),
            .True => vm.push(.{ .boolean = true }),
            .Constant => vm.push(vm.readConstant()),
            .Add => vm.push(.{ .integer = try vm.popField(.integer) + try vm.popField(.integer) }),
            .Mul => vm.push(.{ .integer = try vm.popField(.integer) * try vm.popField(.integer) }),
            .Sub => {
                const rhs = try vm.popField(.integer);
                const lhs = try vm.popField(.integer);
                vm.push(.{ .integer = lhs - rhs });
            },
            .Div => {
                const rhs = try vm.popField(.integer);
                const lhs = try vm.popField(.integer);
                vm.push(.{ .integer = @divFloor(lhs, rhs) });
            },
            .Negate => vm.push(Value{ .integer = -try vm.popField(.integer) }),
            .And => vm.push(.{ .boolean = try vm.popField(.boolean) and try vm.popField(.boolean) }),
            .Or => vm.push(.{ .boolean = try vm.popField(.boolean) or try vm.popField(.boolean) }),
            .Not => vm.push(.{ .boolean = !try vm.popField(.boolean) }),
            .Equals => {
                const rhs = vm.pop();
                const lhs = vm.pop();
                vm.push(.{
                    .boolean = switch (lhs.compare(rhs)) {
                        .equal => true,
                        .less, .greater => false,
                        //TODO: unwrap and display nice error message
                        .failure => return VM.Errors.UnexpectedTypeOfValue,
                    },
                });
            },
            .NotEquals => {
                const rhs = vm.pop();
                const lhs = vm.pop();
                vm.push(.{
                    .boolean = switch (lhs.compare(rhs)) {
                        .equal => false,
                        .less, .greater => true,
                        //TODO: unwrap and display nice error message
                        .failure => return VM.Errors.UnexpectedTypeOfValue,
                    },
                });
            },
            .Return => {
                std.debug.print("VM produced: {}\n", .{vm.pop()});
                return;
            },
        }
    }
}

test "basic vm instructions using integers" {
    var chunk: Chunk = .{
        .instructions = Instructions.init(std.testing.allocator),
        .constants = Values.init(std.testing.allocator),
    };
    defer chunk.deinit();

    const constant = try chunk.addConstant(.{ .integer = 5 });
    try chunk.addInstruction(.Constant);
    try chunk.addAdress(constant);
    try chunk.addInstruction(.Negate);
    try chunk.addInstruction(.Return);

    try chunk.disassemble("example_chunk");

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var vm = try VM.init(arena.allocator(), std.testing.allocator);
    defer vm.deinit();

    _ = try interpret(&vm, &chunk);
}

test "basic vm instructions using strings" {
    var chunk: Chunk = .{
        .instructions = Instructions.init(std.testing.allocator),
        .constants = Values.init(std.testing.allocator),
    };
    defer chunk.deinit();

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var vm = try VM.init(arena.allocator(), std.testing.allocator);
    defer vm.deinit();

    {
        const constant = try chunk.addConstant(try vm.allocateString("hasse"));
        try chunk.addInstruction(.Constant);
        try chunk.addAdress(constant);
    }

    {
        const constant = try chunk.addConstant(try vm.allocateString("hasse"));
        try chunk.addInstruction(.Constant);
        try chunk.addAdress(constant);
    }

    try chunk.addInstruction(.Equals);
    try chunk.addInstruction(.Return);
    try chunk.disassemble("string_eq_chunk");

    _ = try interpret(&vm, &chunk);
}
