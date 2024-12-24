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

pub fn allocator() std.mem.Allocator {
    return backing_allocator;
}

pub fn dump() void {
    std.debug.print("Begin GC dump\n", .{});
    for (stack.items) |value| {
        std.debug.print("{*} {any} {s}\n", .{ value, value.origin.kind, value.origin.value });
        pipeline_state.dumpSourceAtToken(value.origin, value.origin_module);
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
                pipeline_state.dumpSourceAtToken(ptr.origin, ptr.origin_module);
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

pub const ValueOptions = struct {
    origin: *const tokens.Token,
    origin_module: []const u8,
};

pub fn integer(i: i64, opt: ValueOptions) !*values.Value {
    return push(.{
        .as = .{
            .integer = i,
        },
        .origin = opt.origin,
        .origin_module = opt.origin_module,
    });
}

pub fn floating(f: f64, opt: ValueOptions) !*values.Value {
    return push(.{
        .as = .{
            .float = f,
        },
        .origin = opt.origin,
        .origin_module = opt.origin_module,
    });
}

pub fn string(s: []const u8, opt: ValueOptions) !*values.Value {
    return push(.{
        .as = .{
            .string = try backing_allocator.dupe(u8, s),
        },
        .origin = opt.origin,
        .origin_module = opt.origin_module,
    });
}

pub fn allocedString(s: []const u8, opt: ValueOptions) !*values.Value {
    return push(.{
        .as = .{
            .string = s,
        },
        .origin = opt.origin,
        .origin_module = opt.origin_module,
    });
}

pub fn boolean(b: bool, opt: ValueOptions) !*values.Value {
    return push(.{
        .as = .{
            .boolean = b,
        },
        .origin = opt.origin,
        .origin_module = opt.origin_module,
    });
}

pub fn list(l: values.List, opt: ValueOptions) !*values.Value {
    return push(.{
        .as = .{
            .list = l,
        },
        .origin = opt.origin,
        .origin_module = opt.origin_module,
    });
}

pub fn emptyList(opt: ValueOptions) !*values.Value {
    return list(values.List.init(backing_allocator), opt);
}

pub fn record(r: values.Record, opt: ValueOptions) !*values.Value {
    return push(.{
        .as = .{
            .record = r,
        },
        .origin = opt.origin,
        .origin_module = opt.origin_module,
    });
}

pub fn emptyRecord(opt: ValueOptions) !*values.Value {
    return record(values.Record.init(backing_allocator), opt);
}

pub fn cloneOrReference(origin_value: *values.Value) !*values.Value {
    const opt = ValueOptions{
        .origin_module = origin_value.origin_module,
        .origin = origin_value.origin,
    };
    return switch (origin_value.as) {
        // clone
        .string => |s| try string(s, opt),
        .integer => |i| try integer(i, opt),
        .float => |f| try floating(f, opt),
        .boolean => |b| try boolean(b, opt),
        // reference
        .list, .record => origin_value,
    };
}
