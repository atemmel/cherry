const std = @import("std");
const values = @import("value.zig");
const symtable = @import("symtable.zig");

const Stack = std.ArrayList(*values.Value);

var stack: Stack = undefined;
pub var backing_allocator: std.mem.Allocator = undefined;
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

fn get(idx: usize) *values.Value {
    return stack.items[idx];
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
            _ = stack.swapRemove(i - 1);
            // retry index
            i -= 1;
            // lose an allocation
            n_allocs -= 1;
        }
    }
}

pub fn collect() void {
    symtable.mark();
    sweep();
}

fn shouldCollect() bool {
    return n_allocs > 800000;
}

fn push(value: values.Value) !*values.Value {
    n_allocs += 1;
    if (shouldCollect()) {
        collect();
    }
    const ptr = try backing_allocator.create(values.Value);
    try stack.append(ptr);
    ptr.* = value;
    return ptr;
}

pub fn integer(i: i64) !*values.Value {
    return push(.{
        .as = .{
            .integer = i,
        },
    });
}

pub fn floating(f: f64) !*values.Value {
    return push(.{
        .as = .{
            .float = f,
        },
    });
}

pub fn string(s: []const u8) !*values.Value {
    return push(.{
        .as = .{
            .string = try backing_allocator.dupe(u8, s),
        },
    });
}

pub fn allocedString(s: []const u8) !*values.Value {
    return push(.{
        .as = .{
            .string = s,
        },
    });
}

pub fn boolean(b: bool) !*values.Value {
    return push(.{
        .as = .{
            .boolean = b,
        },
    });
}

pub fn list(l: values.List) !*values.Value {
    return push(.{
        .as = .{
            .list = l,
        },
    });
}

pub fn emptyList() !*values.Value {
    return list(values.List.init(backing_allocator));
}

pub fn record(r: values.Record) !*values.Value {
    return push(.{
        .as = .{
            .record = r,
        },
    });
}

pub fn emptyRecord() !*values.Value {
    return record(values.Record.init(backing_allocator));
}

pub fn cloneOrReference(origin: *values.Value) !*values.Value {
    return switch (origin.as) {
        // clone
        .string => |s| try string(s),
        .integer => |i| try integer(i),
        .float => |f| try floating(f),
        .boolean => |b| try boolean(b),
        // reference
        .list, .record => origin,
    };
}
