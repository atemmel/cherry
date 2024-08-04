const std = @import("std");

pub const Value = @import("value.zig").Value;

pub const SymtableError = error{
    VariableAlreadyDeclared,
};

const Symtable = std.StringHashMap(*Value);

var symtable: Symtable = undefined;

pub fn init(ally: std.mem.Allocator) void {
    symtable = Symtable.init(ally);
}

pub fn deinit() void {
    symtable.deinit();
}

pub fn get(key: []const u8) ?*Value {
    return symtable.get(key);
}

pub fn iterator() Symtable.Iterator {
    return symtable.iterator();
}

pub fn insert(key: []const u8, value: *Value) !void {
    return if (symtable.get(key) != null)
        SymtableError.VariableAlreadyDeclared
    else
        symtable.put(key, value);
}
