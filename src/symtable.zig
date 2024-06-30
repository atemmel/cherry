const std = @import("std");

pub const SymtableError = error{
    VariableAlreadyDeclared,
};

pub const Value = []const u8;

const Symtable = std.StringHashMap(Value);

var symtable: Symtable = undefined;

pub fn init(ally: std.mem.Allocator) void {
    symtable = Symtable.init(ally);
}

pub fn deinit() void {
    symtable.deinit();
}

pub fn get(str: []const u8) ?Value {
    return symtable.get(str);
}

pub fn insert(str: []const u8, value: Value) !void {
    return if (symtable.get(str) != null)
        SymtableError.VariableAlreadyDeclared
    else
        symtable.put(str, value);
}
