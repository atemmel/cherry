const std = @import("std");

pub const Value = @import("value.zig").Value;

pub const SymtableError = error{
    VariableAlreadyDeclared,
};

const Root = std.ArrayList(*Value);
const Symtable = std.StringHashMap(*Value);

var root_values: Root = undefined;
var symtable: Symtable = undefined;

pub fn init(ally: std.mem.Allocator) void {
    symtable = Symtable.init(ally);
    root_values = Root.init(ally);
}

pub fn deinit() void {
    symtable.deinit();
    root_values.deinit();
}

pub fn get(key: []const u8) ?*Value {
    return symtable.get(key);
}

pub fn put(key: []const u8, value: *Value) !void {
    return try symtable.put(key, value);
}

pub fn appendRoot(value: *Value) !void {
    try root_values.append(value);
}

pub fn mark() void {
    for (root_values.items) |v| {
        v.mark();
    }
    var it = symtable.iterator();
    while (it.next()) |entry| {
        entry.value_ptr.*.mark();
    }
}

pub fn insert(key: []const u8, value: *Value) !void {
    return if (symtable.get(key) != null)
        SymtableError.VariableAlreadyDeclared
    else
        symtable.put(key, value);
}
