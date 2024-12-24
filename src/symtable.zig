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
        deinitFrame(frame);
    }
}

fn deinitFrame(frame: *Frame) void {
    var it = frame.symtable.keyIterator();
    while (it.next()) |key| {
        ally.free(key.*);
    }
    frame.symtable.deinit();
    frame.root_values.deinit();
}

pub fn stackDepth() usize {
    return stack.items.len;
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
    deinitFrame(&old_frame);
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

pub fn appendRoot(value: *Value) !void {
    try topFrame().root_values.append(value);
}

pub fn appendToFrameRoot(root_idx: usize, value: *Value) !void {
    std.debug.assert(stack.items.len > root_idx);
    try stack.items[root_idx].root_values.append(value);
}

pub fn dump(dump_root_values: bool) void {
    std.debug.print("+--------------+\n|   var dump   |\n+--------------+\n", .{});
    for (stack.items, 0..) |frame, idx| {
        std.debug.print("| frame {}:\n", .{idx});
        if (dump_root_values) {
            for (frame.root_values.items) |v| {
                std.debug.print("root value {s} ({*})\n", .{ v, v });
            }
        }
        var it = frame.symtable.iterator();
        while (it.next()) |entry| {
            std.debug.print("variable: {s}, is: {s} ({*})\n", .{ entry.key_ptr.*, entry.value_ptr.*, entry.value_ptr.* });
        }
    }
    std.debug.print("+--------------+\n| end var dump |\n+--------------+\n", .{});
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
        frame.symtable.put(try ally.dupe(u8, key), value);
}
