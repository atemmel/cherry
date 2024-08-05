const std = @import("std");
const Value = @import("value.zig").Value;
const symtable = @import("symtable.zig");

const Stack = std.ArrayList(*Value);

var stack: Stack = undefined;
var backing_allocator: std.mem.Allocator = undefined;
var n_allocs: usize = 0;

pub fn init(ally: std.mem.Allocator) !void {
    stack = try Stack.initCapacity(ally, 128);
    backing_allocator = ally;
}

pub fn deinit() void {
    defer stack.deinit();
    for (stack.items) |ptr| {
        ptr.deinit(backing_allocator);
        backing_allocator.destroy(ptr);
    }
}

fn get(idx: usize) *Value {
    return stack.items[idx];
}

fn mark() void {
    var it = symtable.iterator();
    while (it.next()) |entry| {
        entry.value_ptr.*.mark();
    }
}

fn sweep() void {
    var i: usize = 0;
    while (i < stack.items.len) {
        const ptr = get(i);
        i += 1;

        if (ptr.marked) {
            ptr.unmark();
        } else { // collect
            ptr.deinit(backing_allocator);
            backing_allocator.destroy(ptr);
            _ = stack.swapRemove(i);
            // retry index
            i -= 1;
            // lose an allocation
            n_allocs -= 1;
        }
    }
}

pub fn collect() void {
    mark();
    sweep();
}

fn shouldCollect() bool {
    return n_allocs > 100;
}

fn push(value: Value) !*Value {
    n_allocs += 1;
    if (shouldCollect()) {
        collect();
    }
    const ptr = try backing_allocator.create(Value);
    try stack.append(ptr);
    ptr.* = value;
    return ptr;
}

pub fn integer(i: i64) !*Value {
    return push(.{
        .as = .{
            .integer = i,
        },
    });
}

pub fn string(s: []const u8) !*Value {
    return push(.{
        .as = .{
            .string = try backing_allocator.dupe(u8, s),
        },
    });
}

pub fn allocedString(s: []const u8) !*Value {
    return push(.{
        .as = .{
            .string = s,
        },
    });
}

pub fn boolean(b: bool) !*Value {
    return push(.{
        .as = .{
            .boolean = b,
        },
    });
}
