const std = @import("std");
const values = @import("value.zig");
const tokens = @import("tokens.zig");
const PipelineState = @import("pipeline.zig").State;
const InterpreterError = @import("interpreter.zig").InterpreterError;
const Value = @import("value.zig").Value;

const Roots = std.ArrayList(*Value);
const Symtable = std.StringHashMap(*Value);

const Frame = struct {
    root_values: Roots,
    symtable: Symtable,
};

// bookkeepring code for user-defined aliases lives here so as to be reachable by the interpreter
// in reality, this would perhaps be more appropriate to be 'owned' by the repl
pub const Alias = struct {
    from: []const u8,
    to: []const u8,
};

const GcList = std.ArrayList(*values.Value);
const ProgramStack = std.ArrayList(Frame);
const Aliases = std.ArrayList(Alias);

var gc_list: GcList = undefined;
var stack: ProgramStack = undefined;
var persistent_allocator: std.mem.Allocator = undefined;
var pipeline_state: *PipelineState = undefined;
var n_allocs: usize = 0;
var allocs_until_collect: usize = 800;

pub var aliases: Aliases = undefined; // exposed for convenience

pub fn init(ally: std.mem.Allocator, state: *PipelineState) !void {
    gc_list = try GcList.initCapacity(ally, 128);
    stack = try ProgramStack.initCapacity(ally, 16);
    aliases = try Aliases.initCapacity(ally, 16);

    persistent_allocator = ally;
    pipeline_state = state;
}

pub fn deinit() void {
    defer aliases.deinit();
    defer gc_list.deinit();
    defer stack.deinit();
    for (stack.items) |*frame| {
        deinitFrame(frame);
    }
    for (gc_list.items) |ptr| {
        ptr.deinit(persistent_allocator);
        persistent_allocator.destroy(ptr);
    }
}

fn deinitFrame(frame: *Frame) void {
    var it = frame.symtable.keyIterator();
    while (it.next()) |key| {
        persistent_allocator.free(key.*);
    }
    frame.symtable.deinit();
    frame.root_values.deinit();
}

pub fn allocator() std.mem.Allocator {
    return persistent_allocator;
}

pub fn dump() void {
    std.debug.print("Begin GC dump\n", .{});
    for (gc_list.items) |value| {
        std.debug.print("{*} {any} {s}\n", .{ value, value.origin.kind, value.origin.value });
        pipeline_state.dumpSourceAtToken(value.origin, value.origin_module);
    }
}

fn get(idx: usize) *values.Value {
    return gc_list.items[idx];
}

fn sweep() void {
    var i: usize = 0;
    while (i < gc_list.items.len) {
        const ptr = get(i);
        i += 1;

        if (ptr.marked) {
            ptr.unmark();
        } else { // collect
            if (pipeline_state.verboseGc) {
                std.debug.print("collecting: {*} {any} {s}\n", .{ ptr, ptr.origin.kind, ptr.origin.value });
                pipeline_state.dumpSourceAtToken(ptr.origin, ptr.origin_module);
            }
            ptr.deinit(persistent_allocator);
            persistent_allocator.destroy(ptr);
            _ = gc_list.swapRemove(i - 1);
            // retry index
            i -= 1;
        }
    }
    // lose all allocations
    n_allocs = 0;
}

pub fn collect() void {
    mark();
    sweep();
}

pub fn shouldCollect() bool {
    return n_allocs >= allocs_until_collect;
}

fn push(value: values.Value) !*values.Value {
    n_allocs += 1;
    const ptr = try persistent_allocator.create(values.Value);
    try gc_list.append(ptr);
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
            .string = try persistent_allocator.dupe(u8, s),
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
    return list(values.List.init(persistent_allocator), opt);
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
    return record(values.Record.init(persistent_allocator), opt);
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

pub fn stackDepth() usize {
    return stack.items.len;
}

fn topFrame() *Frame {
    return &stack.items[stack.items.len - 1];
}

pub fn pushFrame() !void {
    try stack.append(.{
        .symtable = Symtable.init(persistent_allocator),
        .root_values = Roots.init(persistent_allocator),
    });
}

pub fn popFrame() void {
    var old_frame = stack.pop();
    deinitFrame(&old_frame);
}

pub fn getSymbol(key: []const u8) ?*Value {
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

pub fn varDump(dump_root_values: bool) void {
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

fn mark() void {
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

pub fn insertSymbol(key: []const u8, value: *Value) !void {
    const frame = topFrame();
    return if (frame.symtable.get(key) != null)
        InterpreterError.VariableAlreadyDeclared
    else
        frame.symtable.put(try persistent_allocator.dupe(u8, key), value);
}
