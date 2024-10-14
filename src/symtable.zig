const std = @import("std");

const InterpreterError = @import("interpreter.zig").InterpreterError;

const Value = @import("value.zig").Value;

const Root = std.ArrayList(*Value);
const Symtable = std.StringHashMap(*Value);

const Frame = struct {
    root_values: Root,
    symtable: Symtable,
};

const Stack = std.ArrayList(Frame);

pub const Alias = struct {
    from: []const u8,
    to: []const u8,
};

const Aliases = std.ArrayList(Alias);

var stack: Stack = undefined;
pub var aliases: Aliases = undefined;
var ally: std.mem.Allocator = undefined;

pub fn init(ally_arg: std.mem.Allocator) void {
    ally = ally_arg;
    stack = Stack.init(ally);
    aliases = Aliases.init(ally);
}

pub fn deinit() void {
    aliases.deinit();
    defer stack.deinit();
    for (stack.items) |*frame| {
        frame.symtable.deinit();
        frame.root_values.deinit();
    }
}

fn topFrame() *Frame {
    return &stack.items[stack.items.len - 1];
}

pub fn pushFrame() !void {
    try stack.append(.{
        .symtable = Symtable.init(ally),
        .root_values = Root.init(ally),
    });
}

pub fn popFrame() void {
    var old_frame = stack.pop();
    old_frame.root_values.deinit();
    old_frame.symtable.deinit();
}

pub fn get(key: []const u8) ?*Value {
    for (stack.items) |frame| {
        if (frame.symtable.get(key)) |symbol| {
            return symbol;
        }
    }
    return null;
}

pub fn getEntry(key: []const u8) ?Symtable.Entry {
    for (stack.items) |frame| {
        if (frame.symtable.getEntry(key)) |entry| {
            return entry;
        }
    }
    return null;
}

pub fn put(key: []const u8, value: *Value) !void {
    try topFrame().symtable.put(key, value);
}

pub fn appendRoot(value: *Value) !void {
    try topFrame().root_values.append(value);
}

pub fn mark() void {
    for (stack.items) |frame| {
        for (frame.root_values.items) |v| {
            v.mark();
        }
        var it = frame.symtable.iterator();
        while (it.next()) |entry| {
            entry.value_ptr.*.mark();
        }
    }
}

pub fn insert(key: []const u8, value: *Value) !void {
    const frame = topFrame();
    return if (frame.symtable.get(key) != null)
        InterpreterError.VariableAlreadyDeclared
    else
        frame.symtable.put(key, value);
}
