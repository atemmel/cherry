const std = @import("std");
const values = @import("value.zig");
const symtable = @import("symtable.zig");
const tokens = @import("tokens.zig");
const PipelineState = @import("pipeline.zig").State;

const Stack = std.ArrayList(*values.Value);

var stack: Stack = undefined;
pub var backing_allocator: std.mem.Allocator = undefined;
var n_allocs: usize = 0;
var allocs_until_collect: usize = 800;
var pipeline_state: *PipelineState = undefined;

pub fn init(ally: std.mem.Allocator, state: *PipelineState) !void {
    stack = try Stack.initCapacity(ally, 128);
    backing_allocator = ally;
    pipeline_state = state;
}

pub fn deinit() void {
    defer stack.deinit();
    for (stack.items) |ptr| {
        ptr.deinit(backing_allocator);
        backing_allocator.destroy(ptr);
    }
}

pub fn dump() void {
    std.debug.print("Begin GC dump\n", .{});
    for (stack.items) |value| {
        std.debug.print("{*} {any} {s}\n", .{ value, value.origin.kind, value.origin.value });
        pipeline_state.dumpSourceAtToken(value.origin);
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
            if (pipeline_state.verboseGc) {
                std.debug.print("collecting: {*} {any} {s}\n", .{ ptr, ptr.origin.kind, ptr.origin.value });
                pipeline_state.dumpSourceAtToken(ptr.origin);
            }
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

pub fn shouldCollect() bool {
    return n_allocs >= allocs_until_collect;
}

fn push(value: values.Value) !*values.Value {
    n_allocs += 1;
    const ptr = try backing_allocator.create(values.Value);
    try stack.append(ptr);
    ptr.* = value;
    return ptr;
}

pub fn integer(i: i64, origin: *const tokens.Token) !*values.Value {
    return push(.{
        .as = .{
            .integer = i,
        },
        .origin = origin,
    });
}

pub fn floating(f: f64, origin: *const tokens.Token) !*values.Value {
    return push(.{
        .as = .{
            .float = f,
        },
        .origin = origin,
    });
}

pub fn string(s: []const u8, origin: *const tokens.Token) !*values.Value {
    return push(.{
        .as = .{
            .string = try backing_allocator.dupe(u8, s),
        },
        .origin = origin,
    });
}

pub fn allocedString(s: []const u8, origin: *const tokens.Token) !*values.Value {
    return push(.{
        .as = .{
            .string = s,
        },
        .origin = origin,
    });
}

pub fn boolean(b: bool, origin: *const tokens.Token) !*values.Value {
    return push(.{
        .as = .{
            .boolean = b,
        },
        .origin = origin,
    });
}

pub fn list(l: values.List, origin: *const tokens.Token) !*values.Value {
    return push(.{
        .as = .{
            .list = l,
        },
        .origin = origin,
    });
}

pub fn emptyList(origin: *const tokens.Token) !*values.Value {
    return list(values.List.init(backing_allocator), origin);
}

pub fn record(r: values.Record, origin: *const tokens.Token) !*values.Value {
    return push(.{
        .as = .{
            .record = r,
        },
        .origin = origin,
    });
}

pub fn emptyRecord(origin: *const tokens.Token) !*values.Value {
    return record(values.Record.init(backing_allocator), origin);
}

pub fn cloneOrReference(origin: *values.Value) !*values.Value {
    return switch (origin.as) {
        // clone
        .string => |s| try string(s, origin.origin),
        .integer => |i| try integer(i, origin.origin),
        .float => |f| try floating(f, origin.origin),
        .boolean => |b| try boolean(b, origin.origin),
        // reference
        .list, .record => origin,
    };
}
