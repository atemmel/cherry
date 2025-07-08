const std = @import("std");
const print = std.debug.print;
const assert = std.debug.assert;

pub const Op = enum(usize) {
    false,
    true,
    constant,
    add,
    mul,
    sub,
    div,
    negate,
    @"and",
    @"or",
    equals,
    not_equals,
    not,
    @"return",
    define_global,
    set_global,
    get_global,
};

pub const Value = union(Kind) {
    pub const Kind = enum {
        integer,
        boolean,
        string,
    };

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

    pub fn kind(self: Value) Value.Kind {
        return std.meta.activeTag(self);
    }

    const Order = union(enum) {
        less,
        greater,
        equal,
        failure: struct {
            wants: Value.Kind,
            got: Value.Kind,
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
                // interning check
                if (addr == other.string) {
                    return .equal;
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

    fn typeMismatch(lhs_type: Value.Kind, rhs_type: Value.Kind) Value.Order {
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

    pub const Data = union(Kind) {
        string: []const u8,
    };

    data: Data,
    next: ?*HeapNode,

    pub fn deinit(self: *HeapNode, ally: std.mem.Allocator) void {
        switch (self.data) {
            .string => |str| {
                //if (str.len > 32) {
                ally.free(str);
                //}
            },
        }
    }

    pub fn kind(self: HeapNode) Kind {
        return std.meta.activeTag(self);
    }
};

pub const Local = struct {
    //name: Token,
    depth: usize,
};

pub const Scopes = struct {
    locals: Locals,
    scope_depth: usize,

    pub fn beginScope(self: *Scopes) void {
        self.scope_depth += 1;
    }

    pub fn endScope(self: *Scopes) void {
        assert(self.scope_depth > 0);
        self.scope_depth -= 1;
    }
};

pub const Locals = std.ArrayList(Local);
pub const Values = std.ArrayList(Value);
pub const Instructions = std.ArrayList(usize); //TODO: should perhaps be made more compact

pub const Chunk = struct {
    instructions: Instructions,
    constants: Values,

    pub fn init(alloc: std.mem.Allocator) Chunk {
        return Chunk{
            .instructions = Instructions.init(alloc),
            .constants = Values.init(alloc),
        };
    }

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
            .false,
            .true,
            .@"return",
            .negate,
            .add,
            .mul,
            .div,
            .sub,
            .@"and",
            .@"or",
            .equals,
            .not_equals,
            .not,
            => simpleInstruction(inst, offset),
            .constant,
            .define_global,
            .set_global,
            .get_global,
            => constantInstruction(chunk, inst, offset),
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
    interned_strings: std.StringHashMap(*HeapNode),
    globals: std.StringHashMap(Value),
    scopes: Scopes,

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
            .interned_strings = std.StringHashMap(*HeapNode).init(ally),
            .globals = std.StringHashMap(Value).init(ally),
            .scopes = .{
                .locals = Locals.init(arena),
                .scope_depth = 0,
            },
        };
    }

    pub fn deinit(self: *VM) void {
        while (self.heap) |node| {
            self.heap = node.next;
            node.deinit(self.runtime_ally);
            self.runtime_ally.destroy(node);
        }
        self.stack.deinit();
        self.interned_strings.deinit();
        self.globals.deinit();
    }

    pub fn allocateString(self: *VM, str: []const u8) !Value {
        if (self.interned_strings.get(str)) |node| {
            return Value{ .string = node };
        }
        const duped_str = try self.runtime_ally.dupe(u8, str);
        return self.appendHeapStr(duped_str);
    }

    pub fn ownString(self: *VM, str: []const u8) !Value {
        if (self.interned_strings.get(str)) |node| {
            self.runtime_ally.free(str);
            return Value{ .string = node };
        }
        return self.appendHeapStr(str);
    }

    fn appendHeapStr(self: *VM, str: []const u8) !Value {
        const node = try self.runtime_ally.create(HeapNode);
        node.* = HeapNode{
            .data = .{
                .string = str,
            },
            .next = null,
        };
        self.appendHeap(node);
        if (str.len <= 32) { // intern smaller strings
            try self.interned_strings.put(str, node);
        }
        return Value{
            .string = node,
        };
    }

    fn appendHeap(self: *VM, node: *HeapNode) void {
        if (self.heap) |heap| {
            node.next = heap;
        }
        self.heap = node;
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

    pub fn peek(self: *VM, offset: usize) Value {
        return self.stack.items[self.stack.items.len - offset];
    }

    pub fn popField(self: *VM, comptime tag: Value.Kind) Errors!std.meta.TagPayload(Value, tag) {
        if (std.meta.activeTag(self.peek(1)) != tag) {
            return Errors.UnexpectedTypeOfValue;
        }

        return switch (tag) {
            .integer,
            .boolean,
            => @field(self.stack.pop().?, @tagName(tag)),
            .string,
            => unreachable,
        };
    }

    pub fn readConstantString(self: *VM) []const u8 {
        return self.readConstant().string.data.string;
    }

    pub fn popHeapField(self: *VM, comptime tag: HeapNode.Kind) Errors!std.meta.TagPayload(HeapNode.Data, tag) {
        return switch (self.peek(1)) {
            .integer, .boolean => Errors.UnexpectedTypeOfValue,
            .string => @field(self.stack.pop().?, @tagName(tag)).data.string,
        };
    }
};

pub fn interpret(vm: *VM, chunk: *Chunk) !void {
    vm.chunk = chunk;
    vm.ip = 0;

    while (true) { //TODO: not like this
        switch (vm.readInstruction()) {
            .false => vm.push(.{ .boolean = false }),
            .true => vm.push(.{ .boolean = true }),
            .constant => vm.push(vm.readConstant()),
            .add => {
                if (vm.peek(1).isString() and vm.peek(2).isString()) {
                    const rhs = vm.pop().string.data.string;
                    const lhs = vm.pop().string.data.string;
                    const join = try std.mem.concat(vm.runtime_ally, u8, &.{ lhs, rhs });
                    vm.push(try vm.ownString(join));
                } else {
                    vm.push(.{ .integer = try vm.popField(.integer) + try vm.popField(.integer) });
                }
            },
            .mul => vm.push(.{ .integer = try vm.popField(.integer) * try vm.popField(.integer) }),
            .sub => {
                const rhs = try vm.popField(.integer);
                const lhs = try vm.popField(.integer);
                vm.push(.{ .integer = lhs - rhs });
            },
            .div => {
                const rhs = try vm.popField(.integer);
                const lhs = try vm.popField(.integer);
                vm.push(.{ .integer = @divFloor(lhs, rhs) });
            },
            .negate => vm.push(Value{ .integer = -try vm.popField(.integer) }),
            .@"and" => vm.push(.{ .boolean = try vm.popField(.boolean) and try vm.popField(.boolean) }),
            .@"or" => vm.push(.{ .boolean = try vm.popField(.boolean) or try vm.popField(.boolean) }),
            .not => vm.push(.{ .boolean = !try vm.popField(.boolean) }),
            .equals => {
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
            .not_equals => {
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
            .@"return" => {
                std.debug.print("VM produced: {}\n", .{vm.pop()});
                return;
            },
            .define_global => {
                const name = vm.readConstantString();
                try vm.globals.put(name, vm.peek(1));
                _ = vm.pop();
            },
            .get_global => {
                const name = vm.readConstantString();
                const global = vm.globals.get(name) orelse {
                    @panic("did not find global");
                };
                vm.push(global);
            },
            .set_global => {
                const name = vm.readConstantString();
                const fetch = try vm.globals.fetchPut(name, vm.peek(1));
                std.debug.assert(fetch != null);
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
    try chunk.addInstruction(.constant);
    try chunk.addAdress(constant);
    try chunk.addInstruction(.negate);
    try chunk.addInstruction(.@"return");

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
        try chunk.addInstruction(.constant);
        try chunk.addAdress(constant);
    }

    {
        const constant = try chunk.addConstant(try vm.allocateString("hasse"));
        try chunk.addInstruction(.constant);
        try chunk.addAdress(constant);
    }

    try chunk.addInstruction(.equals);
    try chunk.addInstruction(.@"return");
    try chunk.disassemble("string_eq_chunk");

    _ = try interpret(&vm, &chunk);
}

test "more basic vm instructions using strings" {
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
        try chunk.addInstruction(.constant);
        try chunk.addAdress(constant);
    }

    {
        const constant = try chunk.addConstant(try vm.allocateString("boyyyy"));
        try chunk.addInstruction(.constant);
        try chunk.addAdress(constant);
    }

    try chunk.addInstruction(.add);
    try chunk.addInstruction(.@"return");
    try chunk.disassemble("string_concat_chunk");

    _ = try interpret(&vm, &chunk);
}

test "global variables" {
    var chunk: Chunk = .{
        .instructions = Instructions.init(std.testing.allocator),
        .constants = Values.init(std.testing.allocator),
    };
    defer chunk.deinit();

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var vm = try VM.init(arena.allocator(), std.testing.allocator);
    defer vm.deinit();

    const name = try chunk.addConstant(try vm.allocateString("hasse"));
    try chunk.addInstruction(.constant);
    try chunk.addAdress(name);

    const value = try chunk.addConstant(.{ .integer = 5 });
    try chunk.addInstruction(.constant);
    try chunk.addAdress(value);

    try chunk.addInstruction(.define_global);
    try chunk.addAdress(name);

    try chunk.addInstruction(.get_global);
    try chunk.addAdress(name);

    try chunk.addInstruction(.get_global);
    try chunk.addAdress(name);

    try chunk.addInstruction(.add);
    try chunk.addInstruction(.@"return");
    try chunk.disassemble("global_variables_define_and_get_chunk");

    _ = try interpret(&vm, &chunk);
}
